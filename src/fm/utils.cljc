(ns fm.utils
  (:require
   [clojure.spec-alpha2 :as s]))

(s/def :fm/anomaly (s/and map? #(contains? % :fm/anomaly)))
(def anomaly? (partial s/valid? :fm/anomaly))

(defn schema-keys
  [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond (vector? schema)        (distinct (mapcat f schema))
          (map? schema)           (keys schema)
          (or (keyword? schema)
              (s/schema? schema)) (schema-keys (second (s/form schema)))
          (seqable? schema)       (schema-keys (second schema)))))

(defn spec-form
  [x]
  (cond (or (vector? x) (map? x)) `(s/select (s/schema ~x) ~(vec (schema-keys x)))
        (keyword? x)              `(when (s/form ~x) (s/get-spec ~x))
        (seqable? x)              `(s/spec ~x)))

(defn args-form
  [x sym]
  (if (or (vector? x) (map? x))
    `{:keys ~(vec (schema-keys x))
      :as ~sym}
    sym))

(defn fm-form
  [{:keys [fm/fname fm/args fm/ret fm/body]}]
  (let [name-sym (or fname (gensym "fm__"))
        args-sym (or (:fm/as (meta (first body)))
                     (if (keyword? args)
                       (symbol (name args))
                       '_args_))]
    `(let [args# ~(spec-form args)
           ret# ~(spec-form ret)]
       ^{:fm/args args#
         :fm/ret ret#}
       (fn ~(symbol (name name-sym))
         [~(args-form args args-sym)]
         (if (s/valid? :fm/anomaly ~args-sym)
           ~args-sym
           (if (s/valid? args# ~args-sym)
             (try
               (let [res# (do ~@body)]
                 (if (s/valid? ret# res#)
                   res#
                   #:fm{:fname '~name-sym
                        :args ~args-sym
                        :anomaly (s/explain-data ret# res#)}))
               (catch Exception e#
                 #:fm{:fname '~name-sym
                      :args ~args-sym
                      :anomaly e#}))
             #:fm{:fname '~name-sym
                  :anomaly (s/explain-data args# ~args-sym)}))))))
