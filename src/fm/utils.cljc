(ns fm.utils
  (:require
   [clojure.spec-alpha2.gen :as gen]
   [clojure.spec-alpha2 :as s]
   [clojure.walk :as walk]))

(s/def ::anomaly
  (s/select
   [{:fm/anomaly any?}]
   [*]))

(def anomaly?
  (partial s/valid? ::anomaly))

(s/def ::args-anomaly
  (s/and
   ::anomaly
   (fn [{:keys [fm/anomaly]}]
     (vector? anomaly))))

(def args-anomaly?
  (partial s/valid? ::args-anomaly))

(defn schema-keys
  [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond
      (vector? schema)     (distinct (mapcat f schema))
      (map? schema)        (keys schema)
      (or
       (keyword? schema)
       (s/schema? schema)) (schema-keys (second (s/form schema)))
      (seqable? schema)    (schema-keys (second schema)))))

(s/def ::spec-form
  (fn [x]
    (and
     (seqable? x)
     (=
      (first x)
      `s/spec))))

(defn spec-form
  [x]
  (cond
    (vector? x)      (mapv spec-form x)
    (keyword? x)     `(when (s/form ~x) (s/get-spec ~x))
    (map? x)         `(s/select [~x] ~(vec (schema-keys x)))
    (or
     (symbol? x)
     (and
      (not
       (s/valid?
        ::spec-form
        x))
      (seqable? x))) `(s/spec ~x)
    :else            x))

(defn fmt-arg
  [arg]
  (cond
    (vector? arg) (mapv fmt-arg arg)
    (map? arg)    (if (:as arg)
                    arg
                    (assoc arg :as (gensym "arg__")))
    :else         arg))

(defn args-syms-walk
  [x]
  (if (map? x)
    (:as x)
    x))

(def fn-symbols-set
  #{'fn 'fn* `fn 'fm})

(s/def ::fn-form
  (fn [x]
    (and
     (seqable? x)
     (fn-symbols-set (first x)))))

(defn anomaly-form
  [x]
  (cond
    (s/valid? ::fn-form x) `~x
    (symbol? x)            x
    :else                  `(fn [~'_] ~x)))

(defn trace-form
  [x]
  (cond
    (s/valid? ::fn-form x) `~x
    (symbol? x)            x
    :else                  `(fn [~'_] (prn ~x))))

(defn zip-syms-specs
  [syms specs]
  (mapv
   (fn [sym spec]
     (if (symbol? sym)
       [sym spec]
       (zip-syms-specs sym spec)))
   syms
   specs))

(defn rreduce
  [recurse? f init xs]
  (reduce
   (fn [acc x]
     (let [x (if (recurse? x)
               (rreduce recurse? f init x)
               x)]
       (f recurse? init xs acc x)))
   init
   xs))

(defn args-anomaly?*
  [args]
  (rreduce
   vector?
   (fn [_ _ _ _ arg]
     (cond
       (true? arg)              (reduced true)
       (s/valid? ::anomaly arg) (reduced true)
       :else                    false))
   false
   args))

(s/def ::zipped-arg-spec
  (fn [v]
    (and
     (vector? v)
     (= (count v) 2)
     (or
      (s/spec? (second v))
      (and
       (keyword? (second v))
       (s/get-spec (second v)))))))

(s/def ::args-recurse
  (fn [v]
    (and
     (vector? v)
     (not (s/valid? ::zipped-arg-spec v)))))

(defn args-valid?*
  [zipped]
  (rreduce
   (partial s/valid? ::args-recurse)
   (fn [_ _ _ _ x]
     (cond
       (false? x)                          (reduced false)
       (or
        (true? x)
        (and
         (s/valid? ::zipped-arg-spec x)
         (s/valid? (second x) (first x)))) true
       :else                               (reduced false)))
   true
   zipped))

(defn explain*
  [zipped]
  (rreduce
   (partial s/valid? ::args-recurse)
   (fn [_ _ _ acc x]
     (if (s/valid? ::zipped-arg-spec x)
       (conj acc (s/explain-data (second x) (first x)))
       (conj acc x)))
   []
   zipped))

(defn fm-form
  [{:keys [fm/sym fm/args-form fm/body]}]
  (let [sym        (or sym (gensym "fm__"))
        metadata   (meta args-form)
        args-fmt   (mapv fmt-arg args-form)
        args-syms  (walk/postwalk args-syms-walk args-fmt)
        args-specs (some->>
                    (or
                     (:fm/args metadata)
                     (if (empty? args-form)
                       nil
                       (walk/postwalk
                        (fn [x] (if (symbol? x) `any? x))
                        args-syms)))
                    ((fn [specs]
                       (if (not (vector? specs))
                         [specs]
                         specs)))
                    (rreduce
                     vector?
                     (fn [_ _ _ acc spec]
                       (conj acc (spec-form spec)))
                     []))
        zipped     (zip-syms-specs args-syms args-specs)
        ret-spec   (->>
                    (or (:fm/ret metadata) `any?)
                    (spec-form))
        anomaly    (->>
                    (or (:fm/anomaly metadata) `identity)
                    (anomaly-form))
        trace      (when-let [trace (:fm/trace metadata)]
                     (->>
                      (if (true? trace) `prn trace)
                      (trace-form)))
        ret-sym    (gensym "ret__")]

    `(let [args# ~args-specs
           ret#  ~ret-spec
           anom# ~anomaly]

       ^{:fm/sym     '~sym
         :fm/args    args#
         :fm/ret     ret#
         :fm/anomaly anom#}

       (fn ~(symbol (name sym))
         ~args-fmt

         ~(when (:fm/trace metadata)
            `(~trace {:fm/sym  '~sym
                      :fm/args ~args-syms}))

         (if (args-anomaly?* ~args-syms)
           (anom# ~args-syms)

           (if (args-valid?* ~zipped)
             (try
               (let [~ret-sym (do ~@body)]

                 ~(when (:fm/trace metadata)
                    `(~trace {:fm/sym '~sym
                              :fm/ret ~ret-sym}))

                 (cond
                   (s/valid? ::anomaly ~ret-sym)
                   (anom# ~ret-sym)

                   (s/valid? ret# ~ret-sym)
                   ~ret-sym

                   :else
                   (anom# #:fm{:sym     '~sym
                               :args    ~args-syms
                               :anomaly (s/explain-data ret# ~ret-sym)})))

               (catch Throwable e#
                 (anom# #:fm{:sym     '~sym
                             :args    ~args-syms
                             :anomaly e#})))

             (anom# #:fm{:sym     '~sym
                         :args    ~args-syms
                         :anomaly (explain* ~zipped)})))))))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))
