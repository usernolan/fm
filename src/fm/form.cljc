(ns fm.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.meta :as meta]
   [fm.form.lib :as form.lib]))

(defn try-form
  [{:fm/keys [sym metadata body args-syms]
    :as      form-args}]

  (let [ret-sym      (gensym 'ret)
        trace-sym    (::meta/sym (:fm/trace metadata))
        handler-sym  (::meta/sym (:fm/handler metadata) `identity)
        ret-spec-sym (::meta/sym (:fm/ret metadata))
        rel-spec-sym (::meta/sym (:fm/rel metadata))]

    `(try
       (let [~ret-sym (do ~@body)]

         ~@(when (:fm/trace metadata)
             [`(~trace-sym #:fm.trace{:sym '~sym :ret ~ret-sym})])

         (cond
           (s/valid? :fm/anomaly ~ret-sym)
           (~handler-sym ~ret-sym)

           ~@(when (:fm/ret metadata)
               [`(not (s/valid? ~ret-spec-sym ~ret-sym))
                `(~handler-sym
                  #::anomaly{:spec ::anomaly/ret
                             :sym  '~sym
                             :args ~args-syms
                             :data (s/explain-data ~ret-spec-sym ~ret-sym)})])

           ~@(when (:fm/rel metadata)
               [`(not (s/valid? ~rel-spec-sym {:args ~args-syms :ret ~ret-sym}))
                `(~handler-sym
                  #::anomaly{:spec ::anomaly/rel
                             :sym  '~sym
                             :args ~args-syms
                             :data (s/explain-data ~rel-spec-sym {:args ~args-syms :ret ~ret-sym})})])

           :else ~ret-sym))

       (catch Throwable throw#
         (~handler-sym
          #::anomaly{:spec ::anomaly/throw
                     :sym  '~sym
                     :args ~args-syms
                     :data throw#})))))

(defn fn-form
  [{:fm/keys [sym args-form metadata]
    :as      form-args}]

  (let [args-fmt      (form.lib/arg-fmt* args-form)
        args-syms     (form.lib/arg-sym* args-fmt)
        trace-sym     (::meta/sym (:fm/trace metadata))
        handler-sym   (::meta/sym (:fm/handler metadata) `identity)
        args-spec-sym (::meta/sym (:fm/args metadata))
        try-form      (try-form
                       (merge
                        form-args
                        {:fm/args-syms args-syms}))]

    `(fn ~@(when sym [(symbol (name sym))])
       ~args-fmt

       ~@(when (:fm/trace metadata)
           [`(~trace-sym #:fm.trace{:sym '~sym :args ~args-syms})])

       (if (anomaly/recd-anomaly?* ~args-syms)
         (~handler-sym ~args-syms)

         ~(if (:fm/args metadata)
            `(if (s/valid? ~args-spec-sym ~args-syms)
               ~try-form

               (~handler-sym
                #::anomaly{:spec ::anomaly/args
                           :sym  '~sym
                           :args ~args-syms
                           :data (s/explain-data ~args-spec-sym ~args-syms)}))

            try-form)))))

(defn fm
  [{:fm/keys [sym args-form body]
    :as      form-args}]

  (let [metadata (->>
                  (merge (meta args-form) {:fm/sym sym})
                  (into {} (map meta/fn-xf)))
        bindings (interleave
                  (map ::meta/sym  (vals metadata))
                  (map ::meta/form (vals metadata)))
        fn-form  (fn-form
                  (merge
                   form-args
                   {:fm/metadata metadata}))
        fn-meta  (not-empty
                  (zipmap
                   (keys metadata)
                   (map ::meta/sym (vals metadata))))]

    `(let [~@bindings]
       (with-meta ~fn-form ~fn-meta))))
