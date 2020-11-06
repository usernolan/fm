(ns fm.form.lib
  (:require
   [clojure.spec.alpha :as s]
   [fm.lib :as lib]))

(def zip lib/zip)
(def zipv lib/zipv)
(def zipf lib/zipf)
(def zipvf lib/zipvf)
(def rreduce lib/rreduce)
(def conform-throw lib/conform-throw)
(def conform-explain lib/conform-explain)

(s/def ::arg-symbol
  (s/and
   symbol?
   (complement #{'&})))

(def ^:dynamic *fn-symbol-set*
  #{'fn 'fn* `fn
    'constantly `constantly
    'memfn `memfn
    'comp `comp
    'complement `complement
    'partial `partial
    'juxt `juxt
    'memoize `memoize
    'fnil `fnil
    'every-pred `every-pred
    'some-fn `some-fn
    #_#_#_#_'fm 'conse 'nonse 'merge})

(s/def ::fn-symbol
  (fn [x] (*fn-symbol-set* x))) ; NOTE: `s/def` evaluates set, breaks rebinding

(def fn-symbol?
  (partial s/valid? ::fn-symbol))

(s/def ::fn-form
  (s/and
   seq?
   not-empty
   (comp fn-symbol? first))) ; ALT: (comp fn?? eval), (comp #{fn ,,,} deref resolve)

(def fn-form?
  (partial s/valid? ::fn-form))

(def multi?
  (partial instance? clojure.lang.MultiFn))

(def fn??
  (some-fn fn? multi?))

(s/def ::bound-fn
  (s/and
   symbol?
   (comp fn?? deref resolve)))

(def bound-fn?
  (partial s/valid? ::bound-fn))

(def -spec-form?
  (comp
   (hash-set
    (namespace
     `s/*)) ; TODO: revisit
   namespace
   symbol
   resolve
   first))

(s/def ::spec-form
  (s/and
   seq?
   not-empty
   -spec-form?))

(def spec-form?
  (partial s/valid? ::spec-form))

  ;; TODO: rename e.g. `regex-op` ,,,
(def ^:dynamic
  *sequence-spec-symbol-set*
  #{`s/cat
    `s/alt
    `s/*
    `s/+
    `s/?
    `s/&})

(s/def ::sequence-spec-form
  (s/and
   ::spec-form
   (fn [x] ; NOTE: `comp` evaluates set, breaks rebinding
     (*sequence-spec-symbol-set*
      (symbol
       (resolve
        (first x)))))))

(def sequence-spec-form?
  (partial s/valid? ::sequence-spec-form))

  ;; ALT: `s/get-spec`
  ;; NOTE: `spec` respects TBD specs by their qualified keywords
(s/def ::spec-keyword
  qualified-keyword?)

(def spec-keyword?
  (partial s/valid? ::spec-keyword))

(s/def :fm/arg
  (s/or
   ::spec-keyword ::spec-keyword
   ::arg-symbol ::arg-symbol
   ::spec-form ::spec-form
   ::fn-form ::fn-form
   :fm/args :fm/args))

  ;; NOTE: keyword arguments are poorly named in Clojure's case
(s/def :fm/keyword-args-map
  (s/and
   (s/map-of (some-fn keyword? symbol? string?) :fm/arg)
   seq)) ; NOTE: disallow [,,, & {}]

(s/def :fm/variadic-arg
  (s/or
   :fm/arg :fm/arg
   :fm/keyword-args-map :fm/keyword-args-map
   ::sequence-spec-form ::sequence-spec-form))

(s/def :fm/ident any?)
(s/def :fm/arglists any?)
(s/def :fm/doc string?)
(s/def :fm/args
  (s/&
   (s/cat
    :fm/args (s/* :fm/arg)
    :fm/variadic (s/? (s/cat :& #{'&} :fm/variadic-arg :fm/variadic-arg)))
   seq)) ; NOTE: disallow {:fm/args []}
(s/def :fm/ret any?)     ; fn, spec
(s/def :fm/rel any?)     ; fn, spec?
(s/def :fm/trace any?)   ; bool, set, fn
(s/def :fm/conform any?) ; bool, set
(s/def :fm.anomaly/handler any?) ; fn
(s/def :fm.anomaly/handler? boolean?)

(defn arg->symbol
  [arg]
  (cond
    (vector? arg) (when (some #{:as} arg) (last arg))
    (map? arg)    (:as arg)
    :else         arg))

(defn zipv-args
  [argv args]
  (lib/zipvf vector? (fn [_ a] a) argv args))
