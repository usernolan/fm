(ns fm.form
  (:require
   [clojure.core.specs.alpha :as core.specs]
   [clojure.spec.alpha :as s]
   [fm.lib :as lib]))


  ;; NOTE: `spec2` requires symbolic specs, otherwise wrap `s/spec`
  ;; NOTE: `spec2` may alter symbolic predicate style preferences
  ;; TODO: revisit `lib/conform-throw!`
  ;; TODO: revisit `ctx` identifier
  ;; TODO: revisit `::defaults`
  ;; TODO: revisit tags
  ;; TODO: `:fm/ignore`, runtime `*ignore*`, `s/*compile-asserts*`, etc.
  ;; TODO: `:fm/memoize`
  ;; TODO: global spec form deduplication; `registry`, `bind!`
  ;; ALT: reader literals; (vector ,,,) vs. [,,,], `into`
  ;; ALT: qualify positional tags e.g. (s/cat :fm.signature/0 ,,,)
  ;; ALT: `core.match`; [tag x]


   ;;;
   ;;; NOTE: predicates, specs
   ;;;


(def fn-symbol?
  (comp lib/fn? deref resolve))

(s/def ::bound-fn
  (s/and
   symbol?
   resolve
   fn-symbol?)) ; NOTE: `requiring-resolve`?

(def bound-fn?
  (partial s/valid? ::bound-fn))

(def first-bound-fn?
  (comp bound-fn? first))

(s/def ::fn-form
  (s/and
   seq?
   not-empty
   first-bound-fn?))

(def fn-form?
  (partial s/valid? ::fn-form))

(def first-symbol?
  (comp symbol? first))

(def first-resolves?
  (comp resolve first)) ; ALT: boolean

(def first-spec-namespace?
  (comp
   (hash-set
    (namespace `s/*))
   namespace
   symbol
   resolve
   first)) ; TODO: revisit

(s/def ::spec-form
  (s/and
   seq?
   not-empty
   first-symbol?
   first-resolves?
   first-spec-namespace?))

(def spec-form?
  (partial s/valid? ::spec-form))

(def ^:dynamic *regex-op-symbol-set*
  (hash-set
   `s/cat
   `s/alt
   `s/*
   `s/+
   `s/?
   `s/&))

  ;; NOTE: `comp` evaluates set, breaks rebinding
(defn first-regex-op-symbol?
  [spec-form]
  (*regex-op-symbol-set*
   (symbol
    (resolve
     (first spec-form)))))

(s/def ::regex-op-form
  (s/and
   ::spec-form
   first-regex-op-symbol?))

(def regex-op-form?
  (partial s/valid? ::regex-op-form))

  ;; TODO: test-only generators
  ;; NOTE: `s/get-spec` contributes to disambiguation
(s/def ::s/registry-keyword
  (s/and
   qualified-keyword?
   s/get-spec))

(s/def ::positional-binding-map
  (s/map-of
   ::s/registry-keyword
   ::core.specs/binding-form
   :count 1))

(s/def ::positional-binding-form
  (s/or
   ::s/registry-keyword ::s/registry-keyword
   ::positional-binding-map ::positional-binding-map
   ::core.specs/binding-form ::core.specs/binding-form))

  ;; NOTE: see `::core.specs/param-list`
(s/def ::positional-param-list
  (s/cat
   :params (s/* ::positional-binding-form)
   :var-params (s/? (s/cat
                     :ampersand #{'&}
                     :var-form ::positional-binding-form))
   :as-form (s/? (s/cat
                  :as #{:as}
                  :as-sym ::core.specs/local-name))))

(s/def ::nominal-binding-map
  (s/map-of
   keyword?
   ::core.specs/binding-form))

(s/def ::nominal-binding-form
  (s/or
   :keyword keyword? ; NOTE: `:req-un`
   ::nominal-binding-map ::nominal-binding-map))

(s/def ::nominal-param-list
  (s/cat
   :params (s/* ::nominal-binding-form)
   :as-form (s/? (s/cat
                  :as #{:as}
                  :as-sym ::core.specs/local-name))))

(s/def ::specv
  (s/and
   vector?
   (s/or
    :fm.context/nominal (s/tuple ::nominal-param-list)
    :fm.context/positional ::positional-param-list)))


   ;;;
   ;;; NOTE: internal concepts, shapes
   ;;;


(comment

  (s/def ::ident
    qualified-keyword?)

  (s/def ::ns
    (partial instance? clojure.lang.Namespace))

  (s/def ::bindings
    (s/map-of
     qualified-ident?
     (s/or
      ::binding 'binding-data?
      ::bindings 'deep-contains-binding-data?)))

  (s/def ::ctx
    (s/keys
     :opt
     [::ident
      ::ns
      ::bindings]))

  ;;;
  )


   ;;;
   ;;; NOTE: top-level multimethods, hierarchies
   ;;;


(def form-hierarchy-atom
  "Specifies an ontology to concisely handle special cases of constructing and
  combining forms across multimethods that specify it as with `:hierarchy`."
  (atom
   (make-hierarchy)))

(defmulti ->form
  "Produces a form to be evaluated as with `eval` or combined with other forms"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)

(defmulti ->forms
  "Produces a sequence of forms to be spliced as with `~@`"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)

(defmulti ->metadata
  "Normalizes and combines metadata forms"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)

(defmulti ->binding
  "Produces binding data to be associated into the context"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)


   ;;;
   ;;; NOTE: form helpers
   ;;;


(defn invalid-definition! [msg data]
  (let [msg (str "\n:: Invalid definition ::\n\n" msg)]
    (throw (ex-info msg data)))) ; TODO: `tools.logging`

(def warn!
  (comp prn (partial str "\n:: Warning ::\n\n"))) ; TODO: `tools.logging`

(defn geta [m k]
  (lib/geta @form-hierarchy-atom m k)) ; ALT: partial apply

(defn geta-in [m path]
  (lib/geta-in @form-hierarchy-atom m path))

(defn finda [m k]
  (lib/finda @form-hierarchy-atom m k))


   ;;;
   ;;; NOTE: default `->metadata` implementations
   ;;;


(defmethod ->metadata :fm.metadata/default
  [ctx tag]
  (->metadata ctx [(get ctx ::ident) tag]))

(defmethod ->metadata :default ; NOTE: keep all metadata
  [ctx tag]
  (let [ctx (assoc ctx ::tag tag)]
    (->metadata ctx [(get ctx ::ident) :default])))


   ;;;
   ;;; NOTE: binding helpers
   ;;;


(defn bind [ctx tags]
  (reduce
   (fn [ctx tag]
     (let [binding (->binding ctx tag)]
       (update ctx ::bindings assoc tag binding))) ; ALT: `into`, `merge`
   ctx
   tags))

(def binding-data?
  (every-pred map? ::destructure ::form)) ; ALT: include `::symbol`

(def binding-data->tuple
  (juxt ::destructure ::form))

(def bindings->tuples
  (partial
   lib/rreduce
   (fn recur? [_acc x]
     (and
      (coll? x)
      (not (binding-data? x))))
   (fn rf [acc x]
     (if (binding-data? x)
       (conj acc (binding-data->tuple x))
       acc))
   (vector)))

(defn bindings [ctx tags]
  (mapcat
   (fn [tag]
     (when-let [b (get-in ctx [::bindings tag])]
       (if (binding-data? b)
         (binding-data->tuple b)
         (mapcat identity (distinct (bindings->tuples b))))))
   tags))

(defn form->binding-data [ctx form]
  (lib/rreduce
   (fn recur? [_acc x]
     (if (and (binding-data? x)
              (= (get x ::form) form))
       (reduced x)
       (coll? x)))
   (constantly nil)
   nil
   (get ctx ::bindings)))

(defn destructure->symbol [destructure]
  (cond
    (vector? destructure) (when (some #{:as} destructure) (last destructure))
    (map? destructure)    (:as destructure)
    :else                 destructure))

(defn default-binding [ctx tag]
  (let [form-tag    (lib/positional-combine [::binding (get ctx ::ident) tag])
        form        (->form ctx form-tag)
        extant      (form->binding-data ctx form)
        destructure (cond
                      (some? extant) (get extant ::symbol)
                      (ident? form)  form
                      :else          (gensym (name (first (lib/ensure-sequential tag)))))
        sym         (destructure->symbol destructure)]
    (if (or (some? extant) (ident? form))
      (hash-map ::destructure destructure ::symbol sym)
      (hash-map ::destructure destructure ::symbol sym ::form form)))) ; ALT: `::core.specs/local-name`, `:local-name`, etc.


   ;;;
   ;;; NOTE: default `->binding` implementations
   ;;;


(defmethod ->binding :default
  [ctx tag]
  (default-binding ctx tag))
