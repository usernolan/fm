(ns fm.form.lib
  (:require
   [clojure.alpha.spec :as s]))

(s/def ::ns-kw
  (s/and keyword? namespace))

(def ns-kw?
  (partial s/valid? ::ns-kw))

(s/def ::ns-kw-vec
  (s/coll-of ::ns-kw :kind vector?))

(def ns-kw-vec?
  (partial s/valid? ::ns-kw-vec))

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

(defn args-form->form
  [args-form]
  (cond
    (vector? args-form) (mapv args-form->form args-form)
    (map? args-form)    (update args-form :as (fnil identity (gensym 'arg)))
    :else               args-form))

(defn args-form->syms
  [args-form]
  (cond
    (vector? args-form) (mapv args-form->syms args-form)
    (map? args-form)    (:as args-form)
    :else               args-form))

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

(defn pred-form
  [x]
  (cond
    (coll? x) (set x)
    (true? x) `(constantly true)
    :else     (hash-set x)))

(defmulti  binding-xf (fn [[k _]] k))
(defmethod binding-xf :fm/conform
  [[k v]]
  [k {::form (eval v)}])

(defmethod binding-xf :default
  [[k v]]
  [k {::sym  (gensym (name k))
      ::form v}])
