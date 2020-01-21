(ns fm.utils
  (:require
   [clojure.alpha.spec.gen :as gen]
   [clojure.alpha.spec :as s]))

(defn arg-fmt*
  [arg]
  (cond
    (vector? arg) (mapv arg-fmt* arg)
    (map? arg)    (update arg :as (fnil identity (gensym "arg__")))
    :else         arg))

(defn arg-sym*
  [arg]
  (cond
    (vector? arg) (mapv arg-sym* arg)
    (map? arg)    (:as arg)
    :else         arg))

(defn default-spec*
  [x]
  (if (vector? x)
    (mapv default-spec* x)
    `any?))

(defn schema-keys*
  [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond
      (vector? schema)     (distinct (mapcat f schema))
      (map? schema)        (keys schema)
      (or
       (keyword? schema)
       (s/schema? schema)) (schema-keys* (second (s/form schema)))
      (seqable? schema)    (schema-keys* (second schema)))))

(s/def ::spec-form
  (fn [x]
    (and
     (seqable? x)
     (= (first x) `s/spec))))

(defn spec-form?
  [x]
  (s/valid? ::spec-form x))

(defn spec-form*
  [x]
  (cond
    (vector? x)          (mapv spec-form* x)
    (map? x)             `(s/select [~x] ~(vec (schema-keys* x)))
    (keyword? x)         `(when (s/form ~x) (s/get-spec ~x))
    (or
     (symbol? x)
     (and
      (seqable? x)
      (not
       (spec-form? x)))) `(s/spec ~x)
    :else                x))

(defn zipv*
  [& xs]
  (if (every? coll? xs)
    (apply mapv zipv* xs)
    (vec xs)))

(def fn-symbols-set
  #{'fn 'fn* `fn 'fm})

(s/def ::fn-form
  (fn [x]
    (and
     (seqable? x)
     (fn-symbols-set (first x)))))

(defn fn-form?
  [x]
  (s/valid? ::fn-form x))

(defn anomaly-form
  [x]
  (cond
    (fn-form? x) `~x
    (symbol? x)  x
    :else        `(fn [~'_] ~x)))

(defn trace-form
  [x]
  (cond
    (fn-form? x) `~x
    (symbol? x)  x
    :else        `(fn [~'_] (prn ~x))))

(defn reduce*
  ([recur? f init xs]
   (reduce* recur? f f init xs))
  ([recur? cf f init xs]
   (reduce
    (fn [acc x]
      (if (recur? acc x)
        (cf (reduce* recur? cf f acc x) x)
        (f acc x)))
    init
    xs)))

;; TODO: 20191115 namespaced keys work strangely in
;; the current `s/select`. the following returns true for
;; `(s/valid? ::args-anomaly {:fm/anomaly {}})`
#_(s/def ::args-anomaly
    (s/select
     [{:fm/anomaly vector?}]
     [:fm/anomaly]))

(s/def ::args-anomaly
  (s/and
   map?
   (fn [{:keys [fm/anomaly]}]
     (vector? anomaly))))

(defn args-anomaly?
  [x]
  (s/valid? ::args-anomaly x))

(s/def ::ret-anomaly
  (s/and
   map?
   (fn [{:keys [fm/anomaly]}]
     (map? anomaly))))

(defn ret-anomaly?
  [x]
  (s/valid? ::ret-anomaly x))

(defn throwable?
  [x]
  (instance? Throwable x))

(s/def ::throw-anomaly
  (s/and
   map?
   (fn [{:keys [fm/anomaly]}]
     (throwable? anomaly))))

(defn throw-anomaly?
  [x]
  (s/valid? ::throw-anomaly x))

(defn contains-anomaly?*
  [recur? xs]
  (reduce*
   recur?
   (fn [acc _]
     (if (true? acc)
       (reduced true)
       false))
   (fn [_ x]
     (if (s/valid?
          (s/or
           ::args  ::args-anomaly
           ::ret   ::ret-anomaly
           ::throw ::throw-anomaly)
          x)
       (reduced true)
       false))
   false
   xs))

(defn args-anomaly-recur?
  [_ x]
  (vector? x))

(defn args-anomaly?*
  [args]
  (contains-anomaly?* args-anomaly-recur? args))

(s/def ::received-anomaly
  (s/and
   vector?
   not-empty
   args-anomaly?*))

(defn received-anomaly?
  [x]
  (s/valid? ::received-anomaly x))

(s/def ::anomaly
  (s/or
   ::args     ::args-anomaly
   ::ret      ::ret-anomaly
   ::throw    ::throw-anomaly
   ::received ::received-anomaly))

(defn anomaly?
  [x]
  (s/valid? ::anomaly x))

(s/def ::zipped-arg-spec
  (s/tuple
   any?
   (s/or
    ::spec s/spec?
    ::kw   (s/and keyword? s/get-spec))))

(defn zipped-arg-spec?
  [x]
  (s/valid? ::zipped-arg-spec x))

(s/def ::arg-vector
  (s/and
   vector?
   (fn [v] (not (zipped-arg-spec? v)))))

(defn arg-vector?
  [x]
  (s/valid? ::arg-vector x))

(defn zipped-recur?
  [_ x]
  (arg-vector? x))

(defn args-valid?*
  [zipped]
  (reduce*
   zipped-recur?
   (fn [acc _]
     (if (false? acc)
       (reduced false)
       true))
   (fn [_ [arg spec]]
     (if (s/valid? spec arg)
       true
       (reduced false)))
   true
   zipped))

(defn explain*
  [x]
  (cond
    (zipped-arg-spec? x) (s/explain-data (second x) (first x))
    (vector? x)          (mapv explain* x)
    :else                x))

(defn fm-form
  [{:keys [fm/sym fm/args-form fm/body]}]
  (let [sym        (or sym (gensym "fm__"))
        metadata   (meta args-form)
        args-fmt   (mapv arg-fmt* args-form)
        args-syms  (mapv arg-sym* args-fmt)
        args-specs (->>
                    (or
                     (:fm/args metadata)
                     (mapv default-spec* args-syms))
                    ((fn [x] (if (vector? x) x [x])))
                    (mapv spec-form*))
        zipped     (mapv zipv* args-syms args-specs)
        ret-sym    (gensym "ret__")
        ret-spec   (->>
                    (or (:fm/ret metadata) `any?)
                    (spec-form*))
        anomaly    (->>
                    (or (:fm/anomaly metadata) `identity)
                    (anomaly-form))
        trace      (when-let [trace (:fm/trace metadata)]
                     (->>
                      (if (true? trace) `prn trace)
                      (trace-form)))]

    `(let [args# ~args-specs
           ret#  ~ret-spec
           anom# ~anomaly]

       ^{:fm/sym     sym
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
                         :anomaly (mapv explain* ~zipped)})))))))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))
