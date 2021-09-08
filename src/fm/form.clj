(ns fm.form
  (:refer-clojure :exclude [binding])
  (:require
   [clojure.core.specs.alpha :as core.specs]
   [clojure.spec.alpha :as s]
   [fm.lib :as lib]))


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
    (throw (ex-info msg data))))

(def warn!
  (comp prn (partial str "\n:: Warning ::\n\n")))

(def cljs?
  (comp boolean :ns)) ; NOTE: takes `&env`


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
                       (lib/fn? form) form
                       :else          (gensym (name (first (lib/ensure-sequential tag)))))
        local-name   (local-name-for binding-form)
        binding      (hash-map ::core.specs/binding-form binding-form
                               ::core.specs/local-name local-name)]
    (if (or (some? extant) (ident? form) (lib/fn? form))
      binding
      (assoc binding ::form form))))


   ;;;
   ;;; NOTE: default `binding` implementations
   ;;;


(defmethod binding :default
  [ctx tag]
  (default-binding ctx tag))


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
