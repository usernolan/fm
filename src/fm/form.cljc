(ns fm.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.meta :as meta]
   [fm.form.lib :as form.lib]))

(defn cond-form
  [{:fm/keys [sym metadata args-sym ret-sym conformed-ret-sym]
    :as      form-args}]

  (let [ret-spec-sym (get-in metadata [:fm/ret     ::meta/sym])
        rel-spec-sym (get-in metadata [:fm/rel     ::meta/sym])
        handler-sym  (get-in metadata [:fm/handler ::meta/sym] `identity)
        conform?     (get-in metadata [:fm/conform ::meta/form] #{})
        ignore?      (get-in metadata [:fm/ignore  ::meta/form] #{})
        ret?         (and
                      (contains? metadata :fm/ret)
                      (not (ignore? :fm/ret)))
        conform-ret? (and
                      ret?
                      (not (ignore? :fm/conform))
                      (conform? :fm/ret))
        rel?         (and
                      (contains? metadata :fm/rel)
                      (not (ignore? :fm/rel)))]

    `(cond
       (s/valid? :fm/anomaly ~ret-sym)
       (~handler-sym ~ret-sym)

       ~@(when ret?
           [(if conform-ret?
              `(s/invalid? ~conformed-ret-sym)
              `(not (s/valid? ~ret-spec-sym ~ret-sym)))

            `(~handler-sym
              #::anomaly{:spec ::anomaly/ret
                         :sym  '~sym
                         :args ~args-sym
                         :data (s/explain-data ~ret-spec-sym ~ret-sym)})])

       ~@(when rel?
           (let [ret-sym (if conform-ret?
                           conformed-ret-sym
                           ret-sym)]

             [`(not (s/valid? ~rel-spec-sym {:args ~args-sym :ret ~ret-sym}))
              `(~handler-sym
                #::anomaly{:spec ::anomaly/rel
                           :sym  '~sym
                           :args ~args-sym
                           :data (s/explain-data ~rel-spec-sym {:args ~args-sym :ret ~ret-sym})})]))

       :else ~(if conform-ret?
                conformed-ret-sym
                ret-sym))))

(defn try-form
  [{:fm/keys [sym metadata body args-sym args-syms]
    :as      form-args}]

  (let [ret-sym           (gensym 'ret)
        conformed-ret-sym (gensym 'conformed-ret)
        args-spec-sym     (get-in metadata [:fm/args    ::meta/sym])
        trace-sym         (get-in metadata [:fm/trace   ::meta/sym])
        ret-spec-sym      (get-in metadata [:fm/ret     ::meta/sym])
        handler-sym       (get-in metadata [:fm/handler ::meta/sym] `identity)
        conform?          (get-in metadata [:fm/conform ::meta/form] #{})
        ignore?           (get-in metadata [:fm/ignore  ::meta/form] #{})
        conform-args?     (and
                           (contains? metadata :fm/args)
                           (not (ignore? :fm/conform))
                           (not (ignore? :fm/args))
                           (conform? :fm/args))
        conform-ret?      (and
                           (contains? metadata :fm/ret)
                           (not (ignore? :fm/conform))
                           (not (ignore? :fm/ret))
                           (conform? :fm/ret))
        trace?            (and
                           (contains? metadata :fm/trace)
                           (not (ignore? :fm/trace)))
        form-args         (merge
                           form-args
                           {:fm/ret-sym           ret-sym
                            :fm/conformed-ret-sym conformed-ret-sym})
        cond-form         (cond-form form-args)]

    `(try
       (let [~@(when conform-args?
                 [args-syms `(s/conform ~args-spec-sym ~args-sym)])

             ~@(when (and trace? conform-args?)
                 ['_ `(~trace-sym
                       #:fm.trace{:sym '~sym :conformed-args ~args-syms})])

             ~ret-sym (do ~@body)

             ~@(when trace?
                 ['_ `(~trace-sym
                       #:fm.trace{:sym '~sym :ret ~ret-sym})])

             ~@(when conform-ret?
                 [conformed-ret-sym `(s/conform ~ret-spec-sym ~ret-sym)])

             ~@(when (and trace? conform-ret?)
                 ['_ `(~trace-sym
                       #:fm.trace{:sym '~sym :conformed-ret ~conformed-ret-sym})])]

         ~cond-form)

       (catch Throwable throw#
         (~handler-sym
          #::anomaly{:spec ::anomaly/throw
                     :sym  '~sym
                     :args ~args-sym
                     :data throw#})))))

(defn fn-form
  [{:fm/keys [sym args-form metadata]
    :as      form-args}]

  (let [args-fmt      (form.lib/arg-fmt* args-form)
        args-syms     (form.lib/arg-sym* args-fmt)
        args-sym      (gensym 'args)
        trace-sym     (get-in metadata [:fm/trace   ::meta/sym])
        args-spec-sym (get-in metadata [:fm/args    ::meta/sym])
        handler-sym   (get-in metadata [:fm/handler ::meta/sym] `identity)
        ignore?       (get-in metadata [:fm/ignore  ::meta/form] #{})
        args?         (and
                       (contains? metadata :fm/args)
                       (not (ignore? :fm/args)))
        trace?        (and
                       (contains? metadata :fm/trace)
                       (not (ignore? :fm/trace)))
        form-args     (merge
                       form-args
                       {:fm/args-sym  args-sym
                        :fm/args-syms args-syms})
        try-form      (try-form form-args)]

    `(fn ~@(when sym [(symbol (name sym))])
       ~args-fmt

       (let [~args-sym ~args-syms]

         ~@(when trace?
             [`(~trace-sym #:fm.trace{:sym '~sym :args ~args-sym})])

         (if (anomaly/recd-anomaly?* ~args-sym)
           (~handler-sym ~args-sym)

           ~(if args?
              `(if (s/valid? ~args-spec-sym ~args-sym)

                 ~try-form

                 (~handler-sym
                  #::anomaly{:spec ::anomaly/args
                             :sym  '~sym
                             :args ~args-sym
                             :data (s/explain-data ~args-spec-sym ~args-sym)}))

              try-form))))))

(defn fm
  [{:fm/keys [sym args-form body]
    :as      form-args}]

  (let [metadata     (into
                      (hash-map)
                      (map meta/fn-xf)
                      (merge (meta args-form) {:fm/sym sym}))
        bindings-map (into
                      (hash-map)
                      (filter meta/binding-filter) metadata)
        bindings     (interleave
                      (map ::meta/sym  (vals bindings-map))
                      (map ::meta/form (vals bindings-map)))
        form-args    (merge form-args {:fm/metadata metadata})
        fn-form      (with-meta
                       (fn-form form-args)
                       (not-empty
                        (zipmap
                         (keys bindings-map)
                         (map ::meta/sym (vals bindings-map)))))]

    `(let [~@bindings] ~fn-form)))
