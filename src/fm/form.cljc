(ns fm.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.meta :as meta]
   [fm.form.lib :as form.lib]))

(defn cond-form
  [{::keys [sym bindings args-syms args]
    :or    {args args-syms}}]

  (let [args-sym     (get-in bindings [::args-sym          ::form.lib/sym] args)
        ret-sym      (get-in bindings [::ret-sym           ::form.lib/sym])
        conf-ret-sym (get-in bindings [::conformed-ret-sym ::form.lib/sym])
        ret-spec-sym (get-in bindings [:fm/ret             ::form.lib/sym])
        rel-spec-sym (get-in bindings [:fm/rel             ::form.lib/sym])
        ret?         (contains? bindings :fm/ret)
        rel?         (contains? bindings :fm/rel)
        conform-ret? (contains? bindings ::conformed-ret-sym)]

    (if (or ret? rel?)
      `(cond

         ~@(when ret?
             [(if conform-ret?
                `(s/invalid? ~conf-ret-sym)
                `(not (s/valid? ~ret-spec-sym ~ret-sym)))

              #::anomaly{:spec ::anomaly/ret
                         :sym  `'~sym
                         :args args-sym
                         :data `(s/explain-data ~ret-spec-sym ~ret-sym)}])

         ~@(when rel?
             (let [ret-sym  (if conform-ret? conf-ret-sym ret-sym)
                   rel-data #:fm.rel{:args args-sym :ret ret-sym}]
               [`(not (s/valid? ~rel-spec-sym ~rel-data))

                #::anomaly{:spec ::anomaly/rel
                           :sym  `'~sym
                           :args args-sym
                           :data `(s/explain-data ~rel-spec-sym ~rel-data)}]))

         :else ~(if conform-ret? conf-ret-sym ret-sym))

      (if conform-ret? conf-ret-sym ret-sym))))

(defn ret-binding-form
  [{::keys [sym bindings body cond-form-fn]
    :as    form-args
    :or    {cond-form-fn cond-form}}]

  (let [ret-sym      (gensym 'ret)
        conf-ret-sym (gensym 'conformed-ret)
        trace-sym    (get-in bindings [:fm/trace   ::form.lib/sym])
        ret-spec-sym (get-in bindings [:fm/ret     ::form.lib/sym])
        conform?     (get-in bindings [:fm/conform ::form.lib/form] #{})
        body?        (seq body)
        trace?       (contains? bindings :fm/trace)
        conform-ret? (and
                      (contains? bindings :fm/ret)
                      (conform? :fm/ret))
        ret-bindings (merge
                      {::ret-sym {::form.lib/sym ret-sym}}
                      (when conform-ret?
                        {::conformed-ret-sym {::form.lib/sym conf-ret-sym}}))
        form-args    (update form-args ::bindings merge ret-bindings)
        cond-form    (cond-form-fn form-args)]

    `(let [~ret-sym ~(when body? `(do ~@body))]

       ~@(when trace?
           [`(~trace-sym #:fm.trace{:sym '~sym :ret ~ret-sym})])

       (if (s/valid? :fm/anomaly ~ret-sym)
         ~ret-sym

         ~(if conform-ret?
            `(let [~conf-ret-sym (s/conform ~ret-spec-sym ~ret-sym)]

               ~@(when trace?
                   [`(~trace-sym
                      #:fm.trace{:sym '~sym :conformed-ret ~conf-ret-sym})])

               ~cond-form)

            cond-form)))))

(defn args-anomaly-form
  [{::keys [sym bindings ret-binding-form-fn]
    :as    form-args
    :or    {ret-binding-form-fn ret-binding-form}}]

  (let [args-spec-sym (get-in bindings [:fm/args             ::form.lib/sym])
        args-sym      (get-in bindings [::args-sym           ::form.lib/sym])
        conf-args-sym (get-in bindings [::conformed-args-sym ::form.lib/sym])
        conform-args? (contains? bindings ::conformed-args-sym)]

    `(if ~(if conform-args?
            `(s/invalid? ~conf-args-sym)
            `(not (s/valid? ~args-spec-sym ~args-sym)))

       #::anomaly{:spec ::anomaly/args
                  :sym  '~sym
                  :args ~args-sym
                  :data (s/explain-data ~args-spec-sym ~args-sym)}

       ~(ret-binding-form-fn form-args))))

(defn args-binding-form
  [{::keys [sym bindings args-form args-syms args args-anomaly-form-fn
            ret-binding-form-fn]
    :as    form-args
    :or    {args                 args-syms
            args-anomaly-form-fn args-anomaly-form
            ret-binding-form-fn  ret-binding-form}}]

  (let [args-sym      (gensym 'args)
        conf-args-sym (gensym 'conformed-args)
        args-spec-sym (get-in bindings [:fm/args    ::form.lib/sym])
        trace-sym     (get-in bindings [:fm/trace   ::form.lib/sym])
        conform?      (get-in bindings [:fm/conform ::form.lib/form] #{})
        trace?        (contains? bindings :fm/trace)
        args?         (contains? bindings :fm/args)
        conform-args? (and args? (conform? :fm/args))
        args-bindings (merge
                       {::args-sym {::form.lib/sym args-sym}}
                       (when conform-args?
                         {::conformed-args-sym {::form.lib/sym conf-args-sym}}))
        form-args     (update form-args ::bindings merge args-bindings)
        nested-form   (if args?
                        (args-anomaly-form-fn form-args)
                        (ret-binding-form-fn form-args))]

    `(let [~args-sym ~args

           ~@(when conform-args?
               [conf-args-sym `(s/conform ~args-spec-sym ~args-sym)])]

       ~@(when (and trace? conform-args?)
           [`(~trace-sym
              #:fm.trace{:sym '~sym :conformed-args ~conf-args-sym})])

       ~(if conform-args?
          `(let [~args-form ~conf-args-sym]
             ~nested-form)

          nested-form))))

(defn received-anomaly-form
  [{::keys [args-syms args args-binding-form-fn]
    :as    form-args
    :or    {args                 args-syms
            args-binding-form-fn args-binding-form}}]

  `(if (anomaly/recd-anomaly?* ~args-syms)
     ~args
     ~(args-binding-form-fn form-args)))

(defn try-form
  [{::keys [sym bindings args-syms args received-anomaly-form-fn
            ret-binding-form-fn]
    :as    form-args
    :or    {args                     args-syms
            received-anomaly-form-fn received-anomaly-form
            ret-binding-form-fn      ret-binding-form}}]

  (let [trace-sym   (get-in bindings [:fm/trace ::form.lib/sym])
        trace?      (contains? bindings :fm/trace)
        nested-form (if (seq args-syms)
                      (received-anomaly-form-fn form-args)
                      (ret-binding-form-fn form-args))]

    `(try
       ~@(when trace?
           [`(~trace-sym #:fm.trace{:sym '~sym :args ~args})])

       ~nested-form

       (catch Throwable throw#
         #::anomaly{:spec ::anomaly/throw
                    :sym  '~sym
                    :args ~args
                    :data throw#}))))

(defn fn-form
  [{::keys [sym metadata bindings args-form try-form-fn]
    :as    form-args
    :or    {try-form-fn try-form}}]

  (let [handler-sym (get-in bindings [:fm/handler ::form.lib/sym] `identity)
        metadata    (into
                     (hash-map)
                     (map
                      (fn [[k v]]
                        (if (contains? bindings k)
                          [k (get-in bindings [k ::form.lib/sym])]
                          [k v])))
                     metadata)]

    `(with-meta

       (fn ~@(when sym [(symbol (name sym))])
         ~args-form

         (let [res# ~(try-form-fn form-args)]
           (if (s/valid? :fm/anomaly res#)
             (~handler-sym res#)
             res#)))

       ~metadata)))

(defn binding-form
  [{::keys [bindings nested-form-fn]
    :as    form-args
    :or    {nested-form-fn fn-form}}]

  (let [f        (fn [[_ v]] (contains? v ::form.lib/sym))
        bindings (into {} (filter f) bindings)
        bindings (interleave
                  (map ::form.lib/sym  (vals bindings))
                  (map ::form.lib/form (vals bindings)))]

    `(let [~@bindings]
       ~(nested-form-fn form-args))))

(defn fm
  [{::keys [sym args-form]
    :as    form-args}]

  (let [metadata  (into
                   (hash-map)
                   (map meta/fn-xf)
                   (merge (meta args-form) {:fm/sym sym}))
        bindings  (into
                   (hash-map)
                   (map form.lib/binding-xf) metadata)
        args-form (form.lib/args-form->form args-form)
        args-syms (form.lib/args-form->syms args-form)
        form-args (merge
                   form-args
                   {::metadata  metadata
                    ::bindings  bindings
                    ::args-form args-form
                    ::args-syms args-syms})]

    (binding-form form-args)))
