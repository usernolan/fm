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
        trace-sym    (get-in metadata [:fm/trace   ::meta/sym])
        handler-sym  (get-in metadata [:fm/handler ::meta/sym] `identity)
        ret-spec-sym (get-in metadata [:fm/ret     ::meta/sym])
        rel-spec-sym (get-in metadata [:fm/rel     ::meta/sym])
        ignore?      (get-in metadata [:fm/ignore  ::meta/form] #{})]

    `(try
       (let [~ret-sym (do ~@body)]

         ~@(when (and
                  (contains? metadata :fm/trace)
                  (not (ignore? :fm/trace)))

             [`(~trace-sym #:fm.trace{:sym '~sym :ret ~ret-sym})])

         (cond
           (s/valid? :fm/anomaly ~ret-sym)
           (~handler-sym ~ret-sym)

           ~@(when (and
                    (contains? metadata :fm/ret)
                    (not (ignore? :fm/ret)))

               [`(not (s/valid? ~ret-spec-sym ~ret-sym))
                `(~handler-sym
                  #::anomaly{:spec ::anomaly/ret
                             :sym  '~sym
                             :args ~args-syms
                             :data (s/explain-data ~ret-spec-sym ~ret-sym)})])

           ~@(when (and
                    (contains? metadata :fm/rel)
                    (not (ignore? :fm/rel)))

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
        trace-sym     (get-in metadata [:fm/trace   ::meta/sym])
        handler-sym   (get-in metadata [:fm/handler ::meta/sym] `identity)
        args-spec-sym (get-in metadata [:fm/args    ::meta/sym])
        ignore?       (get-in metadata [:fm/ignore  ::meta/form] #{})
        form-args     (merge form-args {:fm/args-syms args-syms})
        try-form      (try-form form-args)]

    `(fn ~@(when sym [(symbol (name sym))])
       ~args-fmt

       ~@(when (and
                (contains? metadata :fm/trace)
                (not (ignore? :fm/trace)))

           [`(~trace-sym #:fm.trace{:sym '~sym :args ~args-syms})])

       (if (anomaly/recd-anomaly?* ~args-syms)
         (~handler-sym ~args-syms)

         ~(if (and
               (contains? metadata :fm/args)
               (not (ignore? :fm/args)))

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

  (let [metadata     (into
                      (hash-map)
                      (map meta/fn-xf)
                      (merge (meta args-form) {:fm/sym sym}))
        bindings-map (dissoc metadata :fm/ignore)
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
