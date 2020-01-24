(ns fm.utils
  (:require
   [clojure.alpha.spec.gen :as gen]
   [clojure.alpha.spec :as s]))

(s/def :fm/args
  (s/or
   :spec s/spec?
   :vector (s/coll-of s/spec?)))

(s/def :fm/meta
  (s/select
   [:fm/sym :fm/args :fm/ret :fm/rel]
   [:fm/sym]))

(s/def :fm/ret
  (s/or
   :fn fn?
   :spec s/spec?))

(s/def :fm/rel
  (s/or
   :fn fn?
   :spec s/spec?))

(s/def :fm/sym symbol?)

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

(defn any?*
  [x]
  (if (vector? x)
    (mapv any?* x)
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

(defn args-spec-form*
  [x]
  (cond
    (map? x)     `(s/select [~x] ~(vec (schema-keys* x)))
    (and
     (vector? x)
     (empty? x)) `(s/spec #{[]})
    (vector? x)  `(s/tuple ~@(mapv args-spec-form* x))
    :else        x))

(defn ret-spec-form
  [x]
  (cond
    (keyword? x)             `(when (s/form ~x) (s/get-spec ~x))
    (map? x)                 `(s/select [~x] ~(vec (schema-keys* x)))
    (or
     (symbol? x)
     (set? x)
     (and
      (seqable? x)
      (not (spec-form? x)))) `(s/spec ~x)
    :else                    x))

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

(defn handler-form
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

;; TODO: 20200126 namespaced keys work strangely in
;; the current `s/select`. the following returns true for
;; `(s/valid? :fm.anomaly/args {:fm.anomaly/type nil})`
#_(s/def :fm.anomaly/args
    (s/select
     [{:fm.anomaly/type #{:fm.anomaly/args}}]
     [:fm.anomaly/type]))

(s/def :fm.anomaly/args
  (s/and
   map?
   (fn [{:keys [fm.anomaly/spec]}]
     (= spec :fm.anomaly/args))))

(defn args-anomaly?
  [x]
  (s/valid? :fm.anomaly/args x))

(s/def :fm.anomaly/ret
  (s/and
   map?
   (fn [{:keys [fm.anomaly/spec]}]
     (= spec :fm.anomaly/ret))))

(defn ret-anomaly?
  [x]
  (s/valid? :fm.anomaly/ret x))

(s/def :fm.anomaly/throw
  (s/and
   map?
   (fn [{:keys [fm.anomaly/spec fm.anomaly/data]}]
     (and
      (= spec :fm.anomaly/throw)
      (instance? Throwable data)))))

(defn throw-anomaly?
  [x]
  (s/valid? :fm.anomaly/throw x))

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
           ::args  :fm.anomaly/args
           ::ret   :fm.anomaly/ret
           ::throw :fm.anomaly/throw)
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

(s/def :fm.anomaly/received
  (s/and
   vector?
   not-empty
   args-anomaly?*))

(defn received-anomaly?
  [x]
  (s/valid? :fm.anomaly/received x))

(s/def :fm/anomaly
  (s/or
   :fm.anomaly/args     :fm.anomaly/args
   :fm.anomaly/ret      :fm.anomaly/ret
   :fm.anomaly/throw    :fm.anomaly/throw
   :fm.anomaly/received :fm.anomaly/received))

(defn anomaly?
  [x]
  (s/valid? :fm/anomaly x))

(defn fm-form
  [{:keys [fm/sym fm/args-form fm/body]}]
  (let [sym       (or sym (gensym "fm__"))
        metadata  (meta args-form)
        args-fmt  (arg-fmt* args-form)
        args-syms (arg-sym* args-fmt)
        args-spec (->>
                   (:fm/args metadata (any?* args-syms))
                   ((fn [x] (if (vector? x) x [x])))
                   (args-spec-form*))
        ret-sym   (gensym "ret__")
        ret-spec  (ret-spec-form (:fm/ret metadata `any?))
        handler   (handler-form (:fm/handler metadata `identity))
        trace     (when-let [trace (:fm/trace metadata)]
                    (->>
                     (if (true? trace) `prn trace)
                     (trace-form)))]

    `(let [args#    ~args-spec
           ret#     ~ret-spec
           handler# ~handler]

       ^{:fm/sym     '~sym
         :fm/args    args#
         :fm/ret     ret#
         :fm/handler handler#}

       (fn ~(symbol (name sym))
         ~args-fmt

         ~(when (:fm/trace metadata)
            `(~trace #:fm.trace{:sym  '~sym
                                :args ~args-syms}))

         (if (args-anomaly?* ~args-syms)
           (handler# ~args-syms)

           (if (s/valid? args# ~args-syms)
             (try
               (let [~ret-sym (do ~@body)]

                 ~(when (:fm/trace metadata)
                    `(~trace #:fm.trace{:sym '~sym
                                        :ret ~ret-sym}))

                 (cond
                   (s/valid? :fm/anomaly ~ret-sym)
                   (handler# ~ret-sym)

                   (s/valid? ret# ~ret-sym)
                   ~ret-sym

                   :else
                   (handler# #:fm.anomaly{:spec :fm.anomaly/ret
                                          :sym  '~sym
                                          :args ~args-syms
                                          :data (s/explain-data ret# ~ret-sym)})))

               (catch Throwable e#
                 (handler# #:fm.anomaly{:spec :fm.anomaly/throw
                                        :sym  '~sym
                                        :args ~args-syms
                                        :data e#})))

             (handler# #:fm.anomaly{:spec :fm.anomaly/args
                                    :sym  '~sym
                                    :args ~args-syms
                                    :data (s/explain-data args# ~args-syms)})))))))

(defn fm?
  "Given a fn symbol, inform if the symbol is wrapped by fm"
  [fn-meta]
  (->> fn-meta
       meta
       :fm/sym
       boolean))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))
