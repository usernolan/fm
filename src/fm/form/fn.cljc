(ns fm.form.fn
  (:require
   [clojure.spec.alpha :as s]
   [fm.form.lib :as lib]))

  ;; TODO: revisit tags
  ;; TODO: generators
  ;; TODO: order tags according to observed frequency

(s/def ::signature
  (s/cat
   ::argv vector?
   ::body (s/* any?)))

(s/def ::signatures
  (s/+
   (s/spec ::signature)))

(s/def ::definition
  (s/cat
   :simple-symbol? (s/? simple-symbol?)
   :rest ; ALT: `:fm.definition/rest`
   (s/alt
    ::signature  ::signature
    ::signatures ::signatures)))

(s/def ::arg
  (s/or
   ::lib/spec-keyword ::lib/spec-keyword
   ::lib/arg-symbol ::lib/arg-symbol
   ::lib/spec-form ::lib/spec-form
   ::lib/fn-form ::lib/fn-form
   ::args ::args))

  ;; NOTE: keyword arguments are poorly named in Clojure's case
(s/def ::keyword-args-map
  (s/and
   (s/map-of (some-fn keyword? symbol? string?) ::arg)
   seq)) ; NOTE: disallow [,,, & {}]

(s/def ::variadic-arg
  (s/or
   ::arg ::arg
   ::keyword-args-map ::keyword-args-map
   ::lib/sequence-spec-form ::lib/sequence-spec-form))

(s/def ::args
  (s/&
   (s/cat
    ::args (s/* ::arg)
    ::variadic (s/? (s/cat :& #{'&} ::variadic-arg ::variadic-arg)))
   seq)) ; NOTE: disallow {:fm/args []}

(s/def ::doc string?)
(s/def ::ret any?)     ; fn, spec
(s/def ::rel any?)     ; fn, spec?
(s/def ::trace any?)   ; bool, set, fn
(s/def ::conform any?) ; bool, set, fn?
(s/def ::handler any?) ; fn
(s/def ::handler? boolean?)

(defn arg->symbol
  [arg]
  (cond
    (vector? arg) (when (some #{:as} arg) (last arg))
    (map? arg)    (:as arg)
    :else         arg))

(defn zipv-args
  [argv args]
  (lib/zipvf vector? (fn [_ a] a) argv args))

(lib/zipvf vector? (fn [a] (vector a )) argv)
