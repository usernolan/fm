(ns fm.form.fn
  (:require
   [clojure.spec.alpha :as s]
   [fm.lib :as lib]))

  ;; TODO: revisit tags
  ;; TODO: generators
  ;; TODO: order tags according to observed frequency

(s/def ::signature
  (s/cat
   :fm.form/argv :fm.form/argv
   :fm.form/body (s/* any?)))

(s/def ::signatures
  (s/+
   (s/spec ::signature)))

(s/def ::definition
  (s/cat
   :fm.definition/simple-symbol (s/? simple-symbol?)
   :fm.definition/rest
   (s/alt
    ::signature ::signature
    ::signatures ::signatures)))

(s/def ::arg
  (s/or
   :fm.form/spec-keyword :fm.form/spec-keyword
   :fm.form/arg-symbol :fm.form/arg-symbol
   :fm.form/spec-form :fm.form/spec-form
   :fm.form/fn-form :fm.form/fn-form
   ::args ::args))

  ;; NOTE: keyword arguments are poorly named in Clojure's case
  ;; NOTE: `s/keys*` doesn't support `strs` and `syms`, `destructure` does
(s/def ::keyword-args-map
  (s/and
   (s/map-of (some-fn keyword? symbol? string?) ::arg)
   seq)) ; NOTE: disallow [,,, & {}]

(s/def ::variadic-arg
  (s/or
   ::arg ::arg
   ::keyword-args-map ::keyword-args-map
   :fm.form/sequence-spec-form :fm.form/sequence-spec-form))

(s/def ::args
  (s/&
   (s/cat
    ::args (s/* ::arg)
    ::variadic-arg (s/? (s/cat :& #{'&} ::variadic-arg ::variadic-arg)))
   seq)) ; NOTE: disallow {:fm/args []}

(defn arg->symbol
  [arg]
  (cond
    (vector? arg) (when (some #{:as} arg) (last arg))
    (map? arg)    (:as arg)
    :else         arg))

(defn zipv-args
  [argv args]
  (lib/zipvf vector? (fn [_ a] a) argv args))
