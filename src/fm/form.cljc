(ns fm.form
  (:refer-clojure :exclude [binding])
  (:require
   [clojure.core.specs.alpha :as core.specs]
   [clojure.spec.alpha :as s]
   [fm.lib :as lib]
   #?@(:cljs [[fm.cljs]])))


   ;;;
   ;;; NOTE: top-level multimethods, hierarchies
   ;;;


(def form-hierarchy-atom
  "Specifies an ontology to concisely handle special cases of constructing and
  combining forms across multimethods that specify it as with `:hierarchy`."
  (atom
   (make-hierarchy)))

(defmulti form
  "Produces a form to be evaluated as with `eval` or combined with other forms"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)

(defmulti forms
  "Produces a sequence of forms to be spliced as with `~@`"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)

(defmulti metadata
  "Normalizes and combines metadata forms"
  (fn [_ctx tag] tag)
  :hierarchy form-hierarchy-atom)

(defmulti binding
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
   ;;; NOTE: default `metadata` implementations
   ;;;


(defmethod metadata :fm.metadata/default
  [ctx tag]
  (metadata ctx [(get ctx ::ident) tag]))

(defmethod metadata :default ; NOTE: keep all metadata
  [ctx tag]
  (let [ctx (assoc ctx ::tag tag)
        t   (lib/ensure-sequential [(get ctx ::ident) tag])]
    (if (contains? (methods metadata) t)
      (metadata ctx t)
      (metadata ctx (lib/ensure-sequential [(get ctx ::ident) :default])))))


   ;;;
   ;;; NOTE: binding helpers
   ;;;


(defn bind [ctx tags]
  (reduce
   (fn [ctx tag]
     (let [binding (binding ctx tag)]
       (update ctx ::bindings assoc tag binding))) ; ALT: `into`, `merge`
   ctx
   tags))

(def binding-data?
  (every-pred map? ::core.specs/binding-form ::form)) ; ALT: include `::symbol`

(def binding-tuple
  (juxt ::core.specs/binding-form ::form))

(def binding-tuples
  (partial
   lib/rreduce
   (fn recur? [_acc x]
     (and
      (coll? x)
      (not (binding-data? x))))
   (fn rf [acc x]
     (if (binding-data? x)
       (conj acc (binding-tuple x))
       acc))
   (vector)))

(defn bindings [ctx tags]
  (mapcat
   (fn [tag]
     (when-let [b (get-in ctx [::bindings tag])]
       (if (binding-data? b)
         (binding-tuple b)
         (mapcat identity (distinct (binding-tuples b))))))
   tags))

(defn deep-some-binding-data [form xs]
  (let [form-match? (comp #{form} ::form)
        match?      (partial s/valid? (s/and binding-data? form-match?))]
    (lib/deep-some match? xs)))

(defn local-name-for [binding-form]
  (cond
    (vector? binding-form) (when (some #{:as} binding-form) (last binding-form))
    (map? binding-form)    (:as binding-form)
    :else                  binding-form))

(defn default-binding [ctx tag]
  (let [form-tag     (lib/positional-combine [::binding (get ctx ::ident) tag])
        form         (form ctx form-tag)
        extant       (deep-some-binding-data form ctx)
        binding-form (cond
                       (some? extant) (get extant ::symbol)
                       (ident? form)  form
                       :else          (gensym (name (first (lib/ensure-sequential tag)))))
        local-name   (local-name-for binding-form)]
    (if (or (some? extant) (ident? form))
      (hash-map ::core.specs/binding-form binding-form ::core.specs/local-name local-name)
      (hash-map ::core.specs/binding-form binding-form ::core.specs/local-name local-name ::form form))))


   ;;;
   ;;; NOTE: default `binding` implementations
   ;;;


(defmethod binding :default
  [ctx tag]
  (default-binding ctx tag))


   ;;;
   ;;; NOTE: low-level predicates, specs
   ;;;


(s/def ::s/registry-keyword
   qualified-keyword?)

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

  ;; NOTE: `spec2` requires symbolic specs, otherwise wrap `s/spec`
  ;; NOTE: `spec2` may alter symbolic predicate style preferences
  ;; TODO: revisit `lib/conform-throw!`
  ;; TODO: revisit `ctx` identifier
  ;; TODO: revisit `::defaults`
  ;; TODO: revisit tags
  ;; TODO: `:fm/ignore`, runtime `*ignore*`, `s/*compile-asserts*`, etc.
  ;; TODO: `:fm/memoize`
  ;; TODO: global spec form deduplication; `registry`, `bind!`
  ;; TODO: test generators
  ;; ALT: reader literals; (vector ,,,) vs. [,,,], `into`
  ;; ALT: qualify positional tags e.g. (s/cat :fm.signature/0 ,,,)
  ;; ALT: `core.match`; [tag x]
