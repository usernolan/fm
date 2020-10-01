(ns fm.form.fn
  (:require
   [clojure.spec.alpha :as s]
   [fm.form.lib :as lib]))

  ;; TODO: revisit tags
  ;; TODO: generators

(s/def ::signature
  (s/cat
   ::argv vector?
   ::body (s/* any?)))

(s/def ::signatures
  (s/+
   (s/spec ::signature)))

(s/def ::definition
  (s/cat
   ::simple-symbol?? (s/? simple-symbol?)
   ::rest
   (s/alt
    ::signature  ::signature
    ::signatures ::signatures)))

(s/def ::arg
  (s/or
   ::lib/arg-symbol ::lib/arg-symbol
   ::lib/fn-form ::lib/fn-form
   ::lib/spec-form ::lib/spec-form
   ::lib/spec-keyword ::lib/spec-keyword
   ::arg+ (s/+ ::arg))) ; NOTE: disallow [,,, [] ,,,]

  ;; NOTE: keyword arguments are poorly named in Clojure's case
(s/def ::-keyword
  (some-fn keyword? symbol? string?))

(s/def ::keyword-args-map
  (s/and
   (s/map-of ::-keyword ::arg)
   seq)) ; NOTE: disallow [,,, & {}]

(s/def ::variadic-arg
  (s/or
   ::lib/sequence-spec-form ::lib/sequence-spec-form
   ::keyword-args-map ::keyword-args-map
   ::arg ::arg))

(s/def ::args
  (s/&
   (s/cat
    ::arg* (s/* ::arg)
    ::variadic?
    (s/?
     (s/cat
      :& #{'&}
      ::variadic-arg ::variadic-arg)))
   seq)) ; NOTE: disallow ^{:fm/args []}

(defn arg->sym
  [arg]
  (cond
    (vector? arg) (when (some #{:as} arg) (last arg))
    (map? arg)    (:as arg)
    :else         arg))

(defn zipv-args
  ([argv]      (lib/zipvf vector? (fn [a] (if (= a '&) '& `any?)) argv))
  ([argv args] (lib/zipvf vector? (fn [_ a] a) argv args)))
