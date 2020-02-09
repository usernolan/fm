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

(defn not-anomaly?
  [x]
  (not (anomaly? x)))

(defmulti  fn-meta-xf (fn [[k _]] k))
(defmethod fn-meta-xf :fm/sym
  [[k v]]
  [k #:fm.meta{:sym  (gensym "sym__")
               :form `'~v}])

(defmethod fn-meta-xf :fm/args
  [[k v]]
  [k #:fm.meta{:sym  (gensym "args-spec__")
               :form (args-spec-form* (if (vector? v) v [v]))}])

(defmethod fn-meta-xf :fm/ret
  [[k v]]
  [k #:fm.meta{:sym  (gensym "ret-spec__")
               :form (ret-spec-form v)}])

(defmethod fn-meta-xf :fm/handler
  [[k v]]
  [k #:fm.meta{:sym  (gensym "handler__")
               :form (handler-form v)}])

(defmethod fn-meta-xf :fm/trace
  [[k v]]
  [k #:fm.meta{:sym  (gensym "trace__")
               :form (trace-form (if (true? v) `prn v))}])

(defmethod fn-meta-xf :default
  [[k v]]
  [k #:fm.meta{:sym  (gensym)
               :form v}])

(defn try-form
  [{:keys [fm/sym fm/metadata fm/body fm/args-syms]
    :as   form-args}]

  (let [ret-sym      (gensym "ret__")
        trace-sym    (:fm.meta/sym (:fm/trace metadata))
        handler-sym  (:fm.meta/sym (:fm/handler metadata) `identity)
        ret-spec-sym (:fm.meta/sym (:fm/ret metadata))]

    `(try
       (let [~ret-sym (do ~@body)]

         ~@(when (:fm/trace metadata)
             [`(~trace-sym #:fm.trace{:sym '~sym :ret ~ret-sym})])

         (cond
           (s/valid? :fm/anomaly ~ret-sym)
           (~handler-sym ~ret-sym)

           ~@(if (:fm/ret metadata)
               [`(s/valid? ~ret-spec-sym ~ret-sym)
                ret-sym

                :else
                `(~handler-sym
                  #:fm.anomaly{:spec :fm.anomaly/ret
                               :sym  '~sym
                               :args ~args-syms
                               :data (s/explain-data ~ret-spec-sym ~ret-sym)})]

               [:else ret-sym])))

       (catch Throwable throw#
         (~handler-sym
          #:fm.anomaly{:spec :fm.anomaly/throw
                       :sym  '~sym
                       :args ~args-syms
                       :data throw#})))))

(defn fn-form
  [{:keys [fm/sym fm/args-form fm/metadata]
    :as   form-args}]

  (let [args-fmt      (arg-fmt* args-form)
        args-syms     (arg-sym* args-fmt)
        trace-sym     (:fm.meta/sym (:fm/trace metadata))
        handler-sym   (:fm.meta/sym (:fm/handler metadata) `identity)
        args-spec-sym (:fm.meta/sym (:fm/args metadata))
        try-form      (try-form
                       (merge
                        form-args
                        {:fm/args-syms args-syms}))]

    `(fn ~@(when sym [(symbol (name sym))])
       ~args-fmt

       ~@(when (:fm/trace metadata)
           [`(~trace-sym #:fm.trace{:sym '~sym :args ~args-syms})])

       (if (args-anomaly?* ~args-syms)
         (~handler-sym ~args-syms)

         ~(if (:fm/args metadata)
            `(if (s/valid? ~args-spec-sym ~args-syms)
               ~try-form

               (~handler-sym
                #:fm.anomaly{:spec :fm.anomaly/args
                             :sym  '~sym
                             :args ~args-syms
                             :data (s/explain-data ~args-spec-sym ~args-syms)}))

            try-form)))))

(defn fm-form
  [{:keys [fm/sym fm/args-form fm/body]
    :as   form-args}]

  (let [metadata (->>
                  (merge (meta args-form) {:fm/sym sym})
                  (into {} (map fn-meta-xf)))
        bindings (interleave
                  (map :fm.meta/sym  (vals metadata))
                  (map :fm.meta/form (vals metadata)))
        fn-form  (fn-form
                  (merge
                   form-args
                   {:fm/metadata metadata}))
        fn-meta  (not-empty
                  (zipmap
                   (keys metadata)
                   (map :fm.meta/sym (vals metadata))))]

    `(let [~@bindings]
       (with-meta ~fn-form ~fn-meta))))

(defn fm?
  [sym]
  (boolean (:fm/sym (meta sym))))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))
