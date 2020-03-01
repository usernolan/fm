(ns fm.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.meta :as meta]
   [fm.form.lib :as form.lib]))

(defn cond-form
  [{::keys [sym metadata body args-syms args-sym ret-sym conformed-ret-sym]
    :or    {args-sym args-syms}}]

  (let [ret-spec-sym (get-in metadata [:fm/ret     ::meta/sym])
        rel-spec-sym (get-in metadata [:fm/rel     ::meta/sym])
        conform?     (get-in metadata [:fm/conform ::meta/form] #{})
        body?        (seq body)
        ret?         (contains? metadata :fm/ret)
        conform-ret? (and ret? (conform? :fm/ret))
        rel?         (contains? metadata :fm/rel)]

    `(cond

       ~@(when body?
           [`(s/valid? :fm/anomaly ~ret-sym)
            ret-sym])

       ~@(when ret?
           [(if conform-ret?
              `(s/invalid? ~conformed-ret-sym)
              `(not (s/valid? ~ret-spec-sym ~ret-sym)))
            #::anomaly{:spec ::anomaly/ret
                       :sym  `'~sym
                       :args args-sym
                       :data `(s/explain-data ~ret-spec-sym ~ret-sym)}])

       ~@(when rel?
           (let [ret-sym  (if conform-ret?
                            conformed-ret-sym
                            ret-sym)
                 rel-data {:args args-sym
                           :ret  ret-sym}]

             [`(not (s/valid? ~rel-spec-sym ~rel-data))
              #::anomaly{:spec ::anomaly/rel
                         :sym  `'~sym
                         :args args-sym
                         :data `(s/explain-data ~rel-spec-sym ~rel-data)}]))

       :else ~(if conform-ret?
                conformed-ret-sym
                ret-sym))))

(defn ret-form
  [{::keys [sym metadata body cond-form-fn]
    :as    form-args
    :or    {cond-form-fn cond-form}}]

  (let [ret-sym           (gensym 'ret)
        conformed-ret-sym (gensym 'conformed-ret)
        trace-sym         (get-in metadata [:fm/trace   ::meta/sym])
        ret-spec-sym      (get-in metadata [:fm/ret     ::meta/sym])
        conform?          (get-in metadata [:fm/conform ::meta/form] #{})
        body?             (seq body)
        trace?            (contains? metadata :fm/trace)
        conform-ret?      (and
                           (contains? metadata :fm/ret)
                           (conform? :fm/ret))
        form-args         (merge
                           form-args
                           {::ret-sym ret-sym}
                           (when conform-ret?
                             {::conformed-ret-sym conformed-ret-sym}))
        cond-form         (cond-form-fn form-args)]

    `(let [~ret-sym ~(when body? `(do ~@body))]

       ~@(when trace?
           [`(~trace-sym #:fm.trace{:sym '~sym :ret ~ret-sym})])

       ~(if conform-ret?
          `(let [~conformed-ret-sym (s/conform ~ret-spec-sym ~ret-sym)]

            ~@(when trace?
                [`(~trace-sym
                   #:fm.trace{:sym '~sym :conformed-ret ~conformed-ret-sym})])

            ~cond-form)

          cond-form))))

(defn args-anomaly-form
  [{::keys [sym metadata args-form args-syms ret-form-fn]
    :as    form-args
    :or    {ret-form-fn ret-form}}]

  (let [args-sym           (gensym 'args)
        conformed-args-sym (gensym 'conformed-args)
        trace-sym          (get-in metadata [:fm/trace   ::meta/sym])
        args-spec-sym      (get-in metadata [:fm/args    ::meta/sym])
        conform?           (get-in metadata [:fm/conform ::meta/form] #{})
        trace?             (contains? metadata :fm/trace)
        args?              (contains? metadata :fm/args)
        conform-args?      (and args? (conform? :fm/args))
        form-args          (merge
                            form-args
                            {::args-sym args-sym}
                            (when conform-args?
                              {::conformed-args-sym conformed-args-sym}))
        ret-form           (ret-form-fn form-args)]

    `(if (anomaly/recd-anomaly?* ~args-syms)
       ~args-syms

       (let [~args-sym ~args-syms

             ~@(when conform-args?
                 [conformed-args-sym `(s/conform ~args-spec-sym ~args-sym)])]

         ~@(when (and trace? conform-args?)
             [`(~trace-sym
                #:fm.trace{:sym '~sym :conformed-args ~conformed-args-sym})])

         ~(if args?
            `(if ~(if conform-args?
                    `(s/invalid? ~conformed-args-sym)
                    `(not (s/valid? ~args-spec-sym ~args-sym)))

               #::anomaly{:spec ::anomaly/args
                          :sym  '~sym
                          :args ~args-sym
                          :data (s/explain-data ~args-spec-sym ~args-sym)}

               ~(if conform-args?
                  `(let [~args-form ~conformed-args-sym]
                     ~ret-form)
                  ret-form))

            ret-form)))))

(defn try-form
  [{::keys [sym metadata args-syms args-anomaly-form-fn ret-form-fn]
    :as    form-args
    :or    {args-anomaly-form-fn args-anomaly-form
            ret-form-fn          ret-form}}]

  (let [trace-sym (get-in metadata [:fm/trace ::meta/sym])
        trace?    (contains? metadata :fm/trace)
        body-form (if (seq args-syms)
                    (args-anomaly-form-fn form-args)
                    (ret-form-fn form-args))]

    `(try
       ~@(when trace?
           [`(~trace-sym #:fm.trace{:sym '~sym :args ~args-syms})])

       ~body-form

       (catch Throwable throw#
         #::anomaly{:spec ::anomaly/throw
                    :sym  '~sym
                    :args ~args-syms
                    :data throw#}))))

(defn fn-form
  [{::keys [sym metadata args-form body args-syms try-form-fn]
    :as    form-args
    :or    {try-form-fn try-form}}]

  (let [handler-sym (get-in metadata [:fm/handler ::meta/sym] `identity)
        try?        (or
                     (seq body)
                     (and
                      (seq args-syms)
                      (> (count metadata) 1)))
        try-form    (try-form-fn form-args)]

    `(fn ~@(when sym [(symbol (name sym))])
       ~args-form

       ~@(when try?
           [`(let [res# ~try-form]
               (if (s/valid? :fm/anomaly res#)
                 (~handler-sym res#)
                 res#))]))))

(defn fm
  [{::keys [sym args-form body fn-form-fn]
    :as    form-args
    :or    {fn-form-fn fn-form}}]

  (let [metadata     (into
                      (hash-map)
                      (map meta/fn-xf)
                      (merge (meta args-form) {:fm/sym sym}))
        args-form    (form.lib/args-form->form args-form)
        args-syms    (form.lib/args-form->syms args-form)
        try?         (or
                      (seq body)
                      (and
                       (seq args-syms)
                       (> (count metadata) 1)))
        bindings-map (into
                      (hash-map)
                      (filter meta/binding-filter) metadata)
        bindings-map (if try?
                       bindings-map
                       (select-keys bindings-map [:fm/sym]))
        bindings     (interleave
                      (map ::meta/sym  (vals bindings-map))
                      (map ::meta/form (vals bindings-map)))
        form-args    (merge
                      form-args
                      {::metadata  metadata
                       ::args-form args-form
                       ::args-syms args-syms})
        fn-form      (with-meta
                       (fn-form-fn form-args)
                       (not-empty
                        (zipmap
                         (keys bindings-map)
                         (map ::meta/sym (vals bindings-map)))))]

    `(let [~@bindings] ~fn-form)))
