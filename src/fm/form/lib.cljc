(ns fm.form.lib
  (:require
   [clojure.alpha.spec :as s]))

(s/def ::ns-kw
  (s/and keyword? namespace))

(def ns-kw?
  (partial s/valid? ::ns-kw))

(def nil-ns?
  (comp nil? namespace))

(s/def ::nil-ns-kw
  (s/and keyword? nil-ns?))

(def nil-ns-kw?
  (partial s/valid? ::nil-ns-kw))

(s/def ::binding-sym
  (s/and symbol? nil-ns?))

(def binding-sym?
  (partial s/valid? ::binding-sym))

(defn args-fmt*
  [arg]
  (cond
    (vector? arg) (mapv args-fmt* arg)
    (map? arg)    (update arg :as (fnil identity (gensym "arg__")))
    :else         arg))

(defn args-sym*
  [arg]
  (cond
    (vector? arg) (mapv args-sym* arg)
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

(defn tuple-spec-form*
  [x]
  (cond
    (map? x)     `(s/select [~x] ~(vec (schema-keys* x)))
    (and
     (vector? x)
     (empty? x)) `(s/spec #{[]})
    (vector? x)  `(s/tuple ~@(mapv tuple-spec-form* x))
    :else        x))

(defn spec-form
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
