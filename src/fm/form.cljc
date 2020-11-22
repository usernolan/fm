(ns fm.form
  (:refer-clojure :exclude [fn?])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as core.specs]
   [fm.anomaly :as anomaly]
   [fm.form.fn :as fn]
   [fm.form.sequent :as sequent]
   [fm.lib :as lib]))


  ;; NOTE: `spec2` requires symbolic specs; no inline (fn ,,,), `comp`, etc..
  ;; TODO: revisit tags
  ;; TODO: runtime `*ignore*`, `s/*compile-asserts*`, etc.; config
  ;; TODO: clarify contextual dependencies?
  ;; TODO: "cut elimination"
  ;; TODO: name lambdas


(comment

  (def trace-atom (atom []))

  ;;;
  )


   ;;;
   ;;; NOTE: `fm.form` specs
   ;;;


(def multi?
  (partial instance? clojure.lang.MultiFn))

(def fn?
  (some-fn clojure.core/fn? multi?))

(def fn-symbol?
  (comp fn? deref resolve))

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
  (comp resolve first))

(def first-spec-namespace?
  (comp
   (hash-set
    (namespace `s/*))
   namespace
   symbol
   resolve
   first))

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
  #{`s/cat
    `s/alt
    `s/*
    `s/+
    `s/?
    `s/&})

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
(s/def ::s/registry-keyword
  (s/and
   qualified-keyword?
   s/get-spec))

  ;; NOTE: `s/get-spec` contributes to disambiguation
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

(s/def ::signature
  (s/cat
   :fm.signature/argv ::specv
   :fm.signature/retv (s/? ::specv)
   :fm.signature/body (s/+ any?)))

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


   ;;;
   ;;; NOTE: internal concepts, shapes
   ;;;


(comment

    ;; NOTE: "kinds"
  (s/def ::conformed-definition
    (s/keys
     :opt [:fm.definition/simple-symbol]
     :req [:fm.definition/rest]))

  (s/def ::conformed-param-list
    (s/keys
     :opt-un [::params ::var-params ::as-form]))

  (s/def ::conformed-specv
    (s/tuple
     (hash-set
      :fm.context/nominal
      :fm.context/positional)
     (s/or
      :tuple (s/tuple ::conformed-param-list)
      ::conformed-param-list ::conformed-param-list)))

  (s/def ::ident
    qualified-keyword?)

  (s/def ::ns
    (partial instance? clojure.lang.Namespace))

  (s/def ::defaults
    (s/keys
     :opt
     [:fm/trace
      :fm/trace-fn
      :fm/handler
      :fm/sequent]))

  (s/def ::metadata
    (s/keys
     :opt
     [:fm/ident
      :fm/arglists ; [a k]
      :fm/doc
      :fm/throw!
      :fm/args ; [any?] | [any? ::k]
      :fm/ret ; ::k | [::k] ,,,
      :fm/rel
      :fm/trace
      :fm/conform
      :fm/handler
      :fm/handler?
      :fm/sequent
      :fm/ignore]))

  (s/def ::signature-index int?)

  (s/def ::outer-metadata (s/or ::metadata ::metadata :nil nil?))
  (s/def ::inner-metadatas (s/* ::outer-metadata)) ; ALT: inner-metadata

  (s/def ::tag
    (s/or
     :qualified-ident qualified-ident?
     :compound (s/coll-of qualified-ident? :kind vector?)))

  (s/def ::binding
    (s/or
     :default (s/keys :req [::symbol ::form])
     ::metadata (s/* (s/keys :req [::symbol ::form]))))

  (s/def ::bindings
    (s/map-of ::tag ::binding))

  (s/def ::ctx
    (s/keys
     :opt
     [::ident
      ::ns
      ::defaults
      ::definition
      ::conformed-definition
      ::signature-index
      ::outer-metadata
      ::inner-metadatas
      ::metadata
      ::bindings]))

  ;;;
  )


   ;;;
   ;;; NOTE: `fm` specs
   ;;;


(s/def :fm/pred
  (s/or
   :boolean boolean?
   :set set?))

(s/def :fm/fn
  (s/or
   ::bound-fn ::bound-fn
   ::fn-form ::fn-form))

(s/def :fm/specv
  (s/or
   :fm.context/nominal (s/tuple vector?)
   :fm.context/positional vector?))

(s/def :fm/spec
  (s/or
   :fm/specv :fm/specv
   ::s/registry-keyword ::s/registry-keyword
   ::bound-fn ::bound-fn
   ::fn-form ::fn-form ; ALT: `:fm/fn`
   ::spec-form ::spec-form))

(s/def :fm/ident
  qualified-keyword?)

(s/def :fm/arglists
  (s/* ::core.specs/param-list))

(s/def :fm/doc
  (s/or
   ::definition string?
   ::metadata (s/* (s/or :string string? :nil nil?))))

(s/def :fm/throw!
  (s/or
   ::definition boolean?
   ::metadata (s/* (s/or :boolean boolean? :nil nil?))))

  ;; ALT: keep tags at top level
  ;; ALT: `:fm/metadata`
(s/def :fm/args
  (s/or
   ::definition :fm/spec
   ::metadata (s/* (s/or :fm/spec :fm/spec :nil nil?))))

(s/def :fm/ret
  (s/or
   ::definition :fm/spec
   ::metadata (s/* (s/or :fm/spec :fm/spec :nil nil?))))

(s/def :fm/rel
  (s/or
   ::definition :fm/spec
   ::metadata (s/* (s/or :fm/spec :fm/spec :nil nil?))))

(s/def :fm/trace
  (s/or
   ::definition (s/or :fm/pred :fm/pred :fm/fn :fm/fn)
   ::metadata (s/* (s/or :fm/trace :fm/trace :nil nil?)))) ; TODO: revisit

(s/def :fm/conform
  (s/or
   ::definition :fm/pred
   ::metadata (s/* (s/or :fm/pred :fm/pred :nil nil?))))

(s/def :fm/handler
  (s/or
   ::definition :fm/fn
   ::metadata (s/* (s/or :fm/handler :fm/handler :nil nil?)))) ; TODO: revisit

(s/def :fm/handler?
  (s/or
   ::definition boolean?
   ::metadata (s/* (s/or :boolean boolean? :nil nil?))))

(s/def :fm/sequent
  (s/or
   ::definition (s/keys :opt [:fm.sequent/ident :fm.sequent/unit :fm.sequent/combine])
   ::metadata (s/* (s/or :fm/sequent :fm/sequent :nil nil?)))) ; TODO: revisit; full definition


   ;;;
   ;;; NOTE: top-level multimethods, hierarchies
   ;;;


(defn ->signature-tag
  "Produces a tag that describes the signature of the `fm` form"
  [ctx]
  (get-in ctx [::conformed-definition :fm.definition/rest 0]))

(def form-hierarchy "Specifies an ontology of form tags" ; TODO: improve doc
  (->
   (make-hierarchy)
   (derive :fm/args            :fm/spec)
   (derive :fm/ret             :fm/spec)
   (derive :fm/rel             :fm/spec)
   (derive :fm.sequent/conse   ::sequent)
   (derive :fm.sequent/nonse   ::sequent)
   (derive :fm.sequent/merge   ::sequent)
   (derive :fm.sequent/iso     ::sequent)
   (derive ::fn/args           :fm.trace/default)
   (derive ::fn/ret            :fm.trace/default)
   (derive ::fn/conformed-args :fm.trace/default)
   (derive ::fn/conformed-ret  :fm.trace/default)))

(defmulti ->form
  "Produces a form to be evaluated as with `eval` or combined with other forms"
  (fn [_ctx tag]
    (swap! trace-atom conj ["->form" tag])
    tag)
  :hierarchy #'form-hierarchy)

(defmulti ->def "Produces a `def` form"
  (fn [_ctx tag]
    (swap! trace-atom conj ["->def" tag])
    tag))

(defmulti ->forms "Produces a sequence of forms to be spliced as with `~@`"
  (fn [_ctx tag]
    (swap! trace-atom conj ["->forms" tag])
    tag)
  :hierarchy #'form-hierarchy)

(def metadata-hierarchy
  "Specifies an ontology to concisely handle special cases of combining metadata
  forms.
  `:default` — keeps all unrecognized metadata keys exactly as-typed.
  `:fm.metadata/default` — catch-all for recognized keys. the resulting metadata
  will be either the same size as `:fm/arglists` or one element larger to
  include outer metadata.
  `:fm.metadata/fallback` — when a signature has no inner metadata, the outer
  metadata for that key will be used as a fallback. the resulting metadata form
  will always be the same size as `:fm/arglists`."
  (->
   (make-hierarchy)
   (derive :fm.metadata/fallback :fm.metadata/default)
   (derive :fm/throw!            :fm.metadata/fallback)
   (derive :fm/args              :fm.metadata/fallback)
   (derive :fm/ret               :fm.metadata/fallback)
   (derive :fm/rel               :fm.metadata/fallback)
   (derive :fm/trace             :fm.metadata/fallback)
   (derive :fm/conform           :fm.metadata/fallback)
   (derive :fm/handler           :fm.metadata/fallback)
   (derive :fm/handler?          :fm.metadata/fallback))) ; TODO: atom, `..metadata/hierarchy`

(defmulti ->metadata
  "Formats and combines metadata forms"
  (fn [_ctx tag]
    (swap! trace-atom conj ["->metadata" tag])
    tag)
  :hierarchy #'metadata-hierarchy)

(def binding-hierarchy
  "Specifies an ontology to concisely handle special cases in binding, such as
  binding certain `::metadata` forms"
  (->
   (make-hierarchy)
   (derive :fm/args    ::metadata)
   (derive :fm/ret     ::metadata)
   (derive :fm/rel     ::metadata)
   (derive :fm/trace   ::metadata)
   (derive :fm/handler ::metadata))) ; TODO: atom, `..binding/hierarchy`

(defmulti ->binding
  "Produces binding data to be associated into the context"
  (fn [_ctx tag]
    (swap! trace-atom conj ["->binding" tag])
    tag)
  :hierarchy #'binding-hierarchy)


   ;;;
   ;;; NOTE: context helpers
   ;;;


(defn bind [ctx tags]
  (swap! trace-atom conj ["bind" tags])
  (reduce
   (fn [ctx tag]
     (let [binding (->binding ctx tag)]
       (update ctx ::bindings assoc tag binding)))
   ctx
   tags))

(defn binding->tuple [binding]
  (swap! trace-atom conj ["binding->tuple" binding])
  (when (some? (get binding ::form))
    ((juxt ::symbol ::form) binding)))

(defn bindings [ctx tags]
  (swap! trace-atom conj ["bindings" tags])
  (mapcat
   (fn [tag]
     (when-let [binding (get-in ctx [::bindings tag])]
       (if (sequential? binding)
         (mapcat binding->tuple (distinct binding))
         (binding->tuple binding))))
   tags))

(defn handler? [ctx]
  (let [index    (get ctx ::signature-index 0)
        handler? (get-in ctx [::metadata :fm/handler? index])]
    handler?))

(defn metadata? [ctx tag]
  (swap! trace-atom conj ["metadata?" tag])
  (let [index     (get ctx ::signature-index 0)
        form      (get-in ctx [::metadata tag index])
        metadata? (if (sequential? form)
                    (seq form)
                    (some? form))]
    metadata?))

(defn trace? [ctx tag]
  (swap! trace-atom conj ["trace?" tag])
  (let [index  (get ctx ::signature-index 0)
        form   (or
                (get-in ctx [::metadata :fm/trace index])
                (get-in ctx [::defaults :fm/trace]))
        trace? (cond
                 (set? form) form
                 (not form)  (constantly false)
                 :else       (constantly true))]
    (trace? tag)))

(defn conform? [ctx tag]
  (swap! trace-atom conj ["conform?" tag])
  (let [index    (get ctx ::signature-index 0)
        form     (or
                  (get-in ctx [::metadata :fm/conform index])
                  (get-in ctx [::defaults :fm/conform]))
        conform? (cond
                   (set? form) form
                   (not form)  (constantly false)
                   :else       (constantly true))]
    (conform? tag)))

(defn throw? [ctx]
  (swap! trace-atom conj "throw?")
  (let [index  (get ctx ::signature-index 0)
        form   (or
                (get-in ctx [::metadata :fm/throw! index])
                (get-in ctx [::defaults :fm/throw!]))
        throw? (boolean form)]
    throw?))

(defn invalid-definition! [_ t]
  (swap! trace-atom conj "invalid-definition!")
  (throw
   (ex-info
    (str
     "\n:: Invalid definition ::"
     "\nAre all positional spec params in the registry?\n\n"
     (ex-message t))
    (ex-data t))))


   ;;;
   ;;; NOTE: top-level `->form` implementations
   ;;;


(defmethod ->form ::fn
  [ctx _]
  (let [conformed  (try (lib/conform-throw ::definition (get ctx ::definition))
                        (catch Throwable t (invalid-definition! ctx t)))
        ctx        (assoc ctx ::ident ::fn ::conformed-definition conformed)
        ctx        (assoc ctx ::metadata (->metadata ctx ::metadata))
        tags       [:fm/args :fm/ret :fm/rel :fm/trace :fm/handler]
        ctx        (bind ctx tags)
        bindings   (bindings ctx tags)
        sym        (->form ctx :fm/simple-symbol)
        definition (->forms ctx ::fn/definition)
        metadata   (->form ctx ::metadata)]
    `(let [~@bindings]
       (with-meta
         (fn ~sym ~@definition)
         ~metadata))))

#_(defmethod ->form ::sequent
    [ctx tag]
    (let [conformed (lib/conform-throw ::sequent/definition (get ctx ::definition))
          ctx       (assoc ctx ::ident tag ::conformed-definition conformed)
          ;; TODO: left -> `:fm/args`, right -> `:fm/ret`
          ctx       (assoc ctx ::metadata (->metadata ctx ::metadata))
          tags      [:fm/args :fm/ret :fm/rel :fm/trace :fm/handler]
          ctx       (bind ctx tags)
          bindings  (bindings ctx tags)
          sym       (->form ctx :fm/simple-symbol)
          argm      (->form ctx ::sequent/argm)
          body      (->form ctx ::sequent/body)
          metadata  (->form ctx ::metadata)]
      `(let [~@bindings]
         (with-meta
           (fn ~sym [& argms#]
             (try
               (let [~argm (into (hash-map) argms#)] ; TODO: when seq
                 ;; TODO: left -> dispatch
                 ;; TODO: signatures -> body
                 ;; TODO: right?
                 ;; TODO: left and right contexts, autolifting; `lift?`, `only?`
                 ~body)
               (catch Throwable t#
                 (~handler
                  {:fm/ident        ~ident
                   ::anomaly/ident  ::anomaly/thrown
                   ::anomaly/args   (vec argms)
                   ::anomaly/thrown t#}))))
           ~metadata))))


   ;;;
   ;;; NOTE: `->metadata` helpers
   ;;;


  ;; ALT: abstract "forward", "split", "branch", "retag", "dispatch"
(defn -arglist-metadata
  [[tag data]]
  (swap! trace-atom conj ["-arglist-metadata" [tag data]])
  (->metadata data [:fm/arglist tag]))

(defn -normalized-binding-tuple [[k [tag conformed]]]
  (swap! trace-atom conj ["-normalized-binding-tuple" [k [tag conformed]]])
  (let [data
        (case tag
          :local-symbol    conformed
          :map-destructure (update conformed :as (fnil identity (symbol (name k))))
          :seq-destructure (update conformed :as-form (fnil identity {:as :as :as-sym (symbol (name k))})))]
    [tag data]))

(def -normalized-arglist-metadata
  (comp
   -arglist-metadata
   -normalized-binding-tuple))

(defn -args-metadata
  [[tag conformed]]
  (swap! trace-atom conj ["-args-metadata" [tag conformed]])
  (->metadata conformed [:fm/args tag]))

(defn -zipv-args
  "Zips outer `:fm/args` to match an inner arglist"
  [arglist args]
  (lib/zipvf vector? (fn [_ a] a) arglist args))

(def -spec-param-tags
  (hash-set
   :fm.context/nominal
   ::s/registry-keyword
   ::positional-binding-map))

(def -contains-spec-param?
  (partial lib/deep-some -spec-param-tags))


   ;;;
   ;;; NOTE: `->metadata` implementations
   ;;;


(defmethod ->metadata ::metadata
  [ctx _]
  (->metadata ctx (->signature-tag ctx)))

  ;; TODO: include `retv`
(defmethod ->metadata ::signature
  [ctx _]
  (let [definition (get ctx ::definition)
        sym        (when (symbol? (first definition)) (first definition))
        signature  (if sym (rest definition) definition)
        ctx        (into ctx {::signature-index 0 ::signature signature})
        outer      (merge
                    (meta sym)
                    (meta (first signature))
                    (->metadata ctx ::signature-index))
        ctx        (assoc ctx ::outer-metadata outer)
        tags       (into (hash-set :fm/ident :fm/arglists) (keys outer))
        metadata   (into (hash-map) (map (partial ->metadata ctx)) tags)]
    metadata))

(defmethod ->metadata ::signatures
  [ctx _]
  (let [definition (get ctx ::definition)
        sym        (when (symbol? (first definition)) (first definition))
        signatures (if sym (rest definition) definition)
        outer      (merge
                    (meta sym)
                    (meta (first signatures)))
        inners     (map-indexed
                    (fn [i signature]
                      (let [ctx (into ctx {::signature-index i ::signature signature})]
                        (merge
                         (meta (first signature))
                         (->metadata ctx ::signature-index))))
                    signatures)
        ctx        (assoc ctx ::outer-metadata outer ::inner-metadatas inners)
        tags       (into (hash-set :fm/ident :fm/arglists) (mapcat keys) (cons outer inners))
        metadata   (into (hash-map) (map (partial ->metadata ctx)) tags)]
    metadata))

(defmethod ->metadata ::signature-index
  [ctx _]
  (let [index     (get ctx ::signature-index)
        conformed (get-in ctx [::conformed-definition :fm.definition/rest 1])
        conformed (if (sequential? conformed) (get conformed index) conformed)
        argv      (get conformed :fm.signature/argv)
        retv      (get conformed :fm.signature/retv)]
    (merge
     (when (-contains-spec-param? argv) {:fm/args (->metadata argv [:fm/args ::conformed-specv])})
     (when (-contains-spec-param? retv) {:fm/ret (->metadata retv [:fm/args ::conformed-specv])}))))

(defmethod ->metadata :fm/ident
  [ctx _]
  (->form ctx :fm/ident))

(defmethod ->metadata :fm/arglists
  [ctx tag]
  (let [tag [tag (->signature-tag ctx)]]
    (->metadata ctx tag)))

(defmethod ->metadata [:fm/arglists ::signature]
  [ctx _]
  (let [argv    (get-in ctx [::conformed-definition :fm.definition/rest 1 :fm.signature/argv])
        arglist (->metadata argv [:fm/arglist ::conformed-specv])]
    (vector arglist)))

(defmethod ->metadata [:fm/arglists ::signatures]
  [ctx _]
  (let [argvs    (map :fm.signature/argv (get-in ctx [::conformed-definition :fm.definition/rest 1]))
        arglists (map (fn [argv] (->metadata argv [:fm/arglist ::conformed-specv])) argvs)]
    (vec arglists)))

(defmethod ->metadata [:fm/arglist ::conformed-specv]
  [[context data] _]
  (let [data (case context
               :fm.context/positional data
               :fm.context/nominal    (first data))]
    (->metadata data [:fm/arglist context ::conformed-param-list])))

(defmethod ->metadata [:fm/arglist :fm.context/positional ::conformed-param-list]
  [conformed tag]
  (let [params   (when-let [params (get conformed :params)]
                   (->metadata params (conj tag :params)))
        var-form (when-let [var-params (get conformed :var-params)]
                   (->metadata var-params (conj tag :var-params)))]
    `[~@params ~@var-form]))

(defmethod ->metadata [:fm/arglist :fm.context/nominal ::conformed-param-list]
  [conformed tag]
  (let [map-destructure (if-let [params (get conformed :params)]
                          (->metadata params (conj tag :params))
                          (hash-map))
        map-destructure (if-let [as-sym (get-in conformed [:as-form :as-sym])]
                          (assoc map-destructure :as as-sym)
                          map-destructure)]
    `[~map-destructure]))

(defmethod ->metadata [:fm/arglist :fm.context/positional ::conformed-param-list :params]
  [params _]
  (into (vector) (map -arglist-metadata) params))

(defmethod ->metadata [:fm/arglist :fm.context/nominal ::conformed-param-list :params]
  [params _]
  (into (hash-map) (map -arglist-metadata) params))

(defmethod ->metadata [:fm/arglist :fm.context/positional ::conformed-param-list :var-params]
  [var-params _]
  (let [var-form (-arglist-metadata (get var-params :var-form))]
    `(& ~var-form)))

(defmethod ->metadata [:fm/arglist :fm.context/positional ::conformed-param-list :as-form]
  [as-form _]
  (let [as-sym (get as-form :as-sym)]
    `(:as ~as-sym)))

(defmethod ->metadata [:fm/arglist ::s/registry-keyword]
  [k _]
  (symbol (name k)))

(defmethod ->metadata [:fm/arglist ::positional-binding-map]
  [m _]
  (-normalized-arglist-metadata (first m)))

(defmethod ->metadata [:fm/arglist ::core.specs/binding-form]
  [binding-form _]
  (-arglist-metadata binding-form))

(defmethod ->metadata [:fm/arglist :keyword]
  [k _]
  (hash-map (symbol (name k)) k))

(defmethod ->metadata [:fm/arglist ::nominal-binding-map]
  [m _]
  (into (hash-map) (map (juxt -normalized-arglist-metadata first)) m))

(defmethod ->metadata [:fm/arglist :local-symbol]    [sym _] sym)
(defmethod ->metadata [:fm/arglist :map-destructure] [m _] m)
(defmethod ->metadata [:fm/arglist :seq-destructure]
  [conformed _]
  (let [forms     (map -arglist-metadata (get conformed :forms))
        rest-form (when-let [rest-form (get-in conformed [:rest-forms :form])]
                    (let [form (-arglist-metadata rest-form)]
                      `(& ~form)))
        as-form   (when-let [as-sym (get-in conformed [:as-form :as-sym])]
                    `(:as ~as-sym))]
    `[~@forms ~@rest-form ~@as-form]))

(defmethod ->metadata [:fm/args ::signatures]
  [ctx _]
  (let [outer      (get-in ctx [::outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::inner-metadatas))
        _          (map
                    (partial lib/conform-throw :fm/args)
                    (remove nil? (cons outer inners)))
        signatures (get-in ctx [::conformed-definition :fm.definition/rest 1])
        f          (fn [argv] (->metadata argv [:fm/arglist ::conformed-specv]))
        arglists   (map (comp f :fm.signature/argv) signatures)
        args       (map-indexed
                    (fn [i inner]
                      (let [arglist (nth arglists i)]
                        (when-let [args (or inner outer)]
                          (-zipv-args arglist args))))
                    inners)]
    (vec args))) ; TODO: qualify, `s/explicate`?

(defmethod ->metadata [:fm/args ::conformed-specv]
  [[context data] tag]
  (->metadata data (conj tag context)))

(defmethod ->metadata [:fm/args ::conformed-specv :fm.context/nominal]
  [[data] _]
  (vector (->metadata data [:fm/args ::conformed-param-list])))

(defmethod ->metadata [:fm/args ::conformed-specv :fm.context/positional]
  [data _]
  (->metadata data [:fm/args ::conformed-param-list]))

(defmethod ->metadata [:fm/args ::conformed-param-list]
  [conformed tag]
  (let [params   (when-let [params (get conformed :params)]
                   (->metadata params (conj tag :params)))
        var-form (when-let [var-params (get conformed :var-params)]
                   (->metadata var-params (conj tag :var-params)))]
    `[~@params ~@var-form]))

(defmethod ->metadata [:fm/args ::conformed-param-list :params]
  [params _]
  (let [f (fn [x] (if (sequential? x) x (vector x)))]
    (into (vector) (mapcat (comp f -args-metadata)) params)))

(defmethod ->metadata [:fm/args ::conformed-param-list :var-params]
  [var-params _]
  (let [var-form (-args-metadata (get var-params :var-form))]
    `(& ~var-form)))

(defmethod ->metadata [:fm/args ::conformed-param-list :as-form]
  [as-form _]
  (let [as-sym (get as-form :as-sym)]
    `(:as ~as-sym)))

(defmethod ->metadata [:fm/args ::s/registry-keyword]      [k _] k)
(defmethod ->metadata [:fm/args ::positional-binding-map]  [m _] (first (keys m)))
(defmethod ->metadata [:fm/args ::core.specs/binding-form] [_ _] `any?) ; TODO: additional inference
(defmethod ->metadata [:fm/args :keyword]                  [k _] k)
(defmethod ->metadata [:fm/args ::nominal-binding-map]     [m _] (keys m))

  ;; NOTE: default for recognized keys
(defmethod ->metadata :fm.metadata/default
  [ctx tag]
  (let [tag [tag (->signature-tag ctx)]]
    (->metadata ctx tag)))

(defmethod ->metadata [:fm.metadata/default ::signature]
  [ctx [tag _]]
  (let [metadata (get-in ctx [::outer-metadata tag])
        _        (when (s/get-spec tag)
                   (lib/conform-throw tag metadata))]
    (vector metadata))) ; NOTE: vector for signature indexing

(defmethod ->metadata [:fm.metadata/fallback ::signatures]
  [ctx [tag _]]
  (let [outer  (get-in ctx [::outer-metadata tag])
        inners (map tag (get ctx ::inner-metadatas))
        _      (map
                (partial lib/conform-throw tag)
                (remove nil? (cons outer inners)))
        metas  (map (fn fallback [inner] (or inner outer)) inners)]
    (vec metas)))

  ;; NOTE: default for unrecognized keys and `:fm/doc`
(defmethod ->metadata :default
  [ctx tag]
  (let [ctx (into ctx {::tag tag})
        tag [:default (->signature-tag ctx)]]
    (->metadata ctx tag)))

(defmethod ->metadata [:default ::signature]
  [ctx _]
  (let [tag      (get ctx ::tag)
        metadata (get-in ctx [::outer-metadata tag])
        _        (when (s/get-spec tag)
                   (lib/conform-throw tag metadata))]
    metadata))

(defmethod ->metadata [:default ::signatures]
  [ctx _]
  (let [tag    (get ctx ::tag)
        outer  (get-in ctx [::outer-metadata tag])
        inners (map tag (get ctx ::inner-metadatas))
        _      (when (s/get-spec tag)
                 (map
                  (partial lib/conform-throw tag)
                  (remove nil? (cons outer inners))))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (vec inners) ; NOTE: at least one
                 :else                (vec (cons outer inners)))]
    form))


   ;;;
   ;;; NOTE: `->binding` machinery
   ;;;


(defmethod ->binding :default
  [ctx tag]
  {::symbol (gensym (name tag))
   ::form   (->form ctx tag)})

(defmethod ->binding ::metadata
  [ctx tag]
  (let [forms   (get-in ctx [::metadata tag])
        binding (::bindings
                 (reduce
                  (fn -deduplicate [acc form] ; NOTE: maintain signature index; trivialize binding deduplication
                    (let [f       (fnil identity (gensym (name tag)))
                          acc     (update-in acc [::form=>symbol form] f)
                          sym     (get-in acc [::form=>symbol form])
                          ctx     (into ctx acc)
                          form    (->form ctx tag)
                          binding {::symbol sym ::form form}
                          acc     (->
                                   acc
                                   (update ::bindings conj binding)
                                   (update ::signature-index inc))]
                      acc))
                  {::bindings [] ::form=>symbol {} ::signature-index 0}
                  forms))]
    binding))

(defmethod ->binding ::fn/args
  [ctx tag]
  (let [index     (get ctx ::signature-index 0)
        conformed (get-in ctx [::conformed-definition :fm.definition/rest 1])
        conformed (if (sequential? conformed) (get conformed index) conformed)
        as-sym    (lib/deep-get :as-sym conformed)]
    {::symbol (or as-sym (gensym (name tag)))
     ::form   (->form ctx tag)}))


   ;;;
   ;;; NOTE: `->form` helpers
   ;;;


(defn -arg-symbol
  [arg]
  (cond
    (vector? arg) (when (some #{:as} arg) (last arg))
    (map? arg)    (:as arg)
    :else         arg))


   ;;;
   ;;; NOTE: `->form` implementations
   ;;;


(defmethod ->form ::metadata
  [ctx _]
  (let [metadata (get ctx ::metadata)]
    `(quote ~metadata)))

(defmethod ->form :fm/ident
  [ctx _]
  (or
   (get-in ctx [::metadata :fm/ident])
   (keyword ; ALT: symbol
    (str (get ctx ::ns))
    (name
     (or
      (get-in ctx [::conformed-definition :fm.definition/simple-symbol])
      (gensym (name (get ctx ::ident)))))))) ; TODO: sequent

(defmethod ->form :fm/symbol
  [ctx _]
  (symbol (->form ctx :fm/ident)))

(defmethod ->form :fm/simple-symbol
  [ctx _]
  (symbol (name (->form ctx :fm/ident))))

(defmethod ->form :fm/spec
  [ctx tag]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings tag index ::symbol])
     (when-let [data (get-in ctx [::metadata tag index])]
       (let [ctx (into ctx {tag data})]
         (->form ctx [::s/form tag]))))))

(defmethod ->form [::s/form :fm/spec]
  [ctx [_ spec-tag :as tag]]
  (let [spec  (get ctx spec-tag)
        [t _] (lib/conform-throw :fm/spec spec)
        ctx   (if (= t :fm/specv) ctx spec)]
    (->form ctx (conj tag t))))

(defmethod ->form [::s/form :fm/spec :fm/specv]
  [ctx [_ spec-tag _ :as tag]]
  (let [specv       (get ctx spec-tag)
        [context _] (lib/conform-throw :fm/specv specv)
        ctx         (case context
                      :fm.context/positional specv
                      :fm.context/nominal    ctx)]
    (->form ctx (conj tag context))))

(defmethod ->form [::s/form :fm/spec :fm/specv :fm.context/positional]
  [specv tag]
  (let [parts      (partition-by (hash-set '&) specv)
        var?       (> (count parts) 1)
        var-only?  (= (count parts) 2)
        params     (when (not var-only?) (->forms (first parts) (conj tag :params)))
        var-params (when var? (->forms (first (last parts)) (conj tag :var-params)))]
    `(s/cat ~@params ~@var-params)))

(defmethod ->form [::s/form :fm/spec :fm/specv :fm.context/nominal]
  [ctx [_ spec-tag _ _]]
  (let [default-ns     (str (get ctx ::ns))
        ks             (first (get ctx spec-tag))
        {req    true
         req-un false} (group-by qualified-keyword? ks)
        req-forms      (when (seq req) `(:req ~req))
        req-un-forms   (when (seq req-un)
                         (let [xf     (comp (partial keyword default-ns) name)
                               req-un (into (vector) (map xf) req-un)]
                           `(:req-un ~req-un)))]
    `(s/keys ~@req-forms ~@req-un-forms)))

(defmethod ->form [::s/form :fm/spec ::s/registry-keyword] [k _] k)
(defmethod ->form [::s/form :fm/spec ::bound-fn] [sym _] sym) ; NOTE: may require `s/spec` in spec2
(defmethod ->form [::s/form :fm/spec ::fn-form] [form _] form)
(defmethod ->form [::s/form :fm/spec ::spec-form] [form _] form)

(defmethod ->form :fm/trace
  [ctx _]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings :fm/trace index ::symbol])
     (when-let [form (or (get-in ctx [::metadata :fm/trace index])
                         (get-in ctx [::defaults :fm/trace]))]
       (let [pred? (some-fn true? set?)
             fn?   (some-fn fn-form? bound-fn?)]
         (cond
           (pred? form)  (get-in ctx [::defaults :fm/trace-fn])
           (fn? form)    form
           (false? form) nil ; ALT: `form`, `false`
           :else         `(partial ~(get-in ctx [::defaults :fm/trace-fn]) ~form)))))))

(defmethod ->form :fm/handler
  [ctx _]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings :fm/handler index ::symbol])
     (let [form (get-in ctx [::metadata :fm/handler index])
           fn?  (some-fn fn-form? bound-fn?)]
       (cond
         (fn? form) form
         :else      (get-in ctx [::defaults :fm/handler])))))) ; TODO: no handler case

  ;; TODO: infer `:fm/handler?` e.g.
  ;; (when (deep-contains? #{:fm/anomaly} args))

(defmethod ->form ::fn/body
  [ctx _]
  (let [index   (get ctx ::signature-index 0)
        arglist (get-in ctx [::metadata :fm/arglists index])]
    (cond
      (not (throw? ctx)) (->form ctx ::fn/try)
      (seq arglist)      (->form ctx [::bind ::fn/args])
      :else              (->form ctx [::bind ::fn/ret]))))

(defmethod ->form ::fn/try
  [ctx _]
  (let [args    (->form ctx ::fn/args)
        body    (cond
                  (seq args) (->form ctx [::bind ::fn/args])
                  :else      (->form ctx [::bind ::fn/ret]))
        handler (->form ctx :fm/handler)
        ident   (->form ctx :fm/ident)]
    `(try
       ~body
       (catch Throwable thrown#
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/thrown
           ::anomaly/args   ~args
           ::anomaly/thrown thrown#})))))

(defmethod ->form ::fn/args
  [ctx _]
  (or
   (get-in ctx [::bindings ::fn/args ::symbol])
   (let [index   (get ctx ::signature-index 0)
         arglist (get-in ctx [::metadata :fm/arglists index])
         norm    (->form arglist ::normalized-arglist)
         form    (if (some #{'&} norm)
                   (let [xf   (comp (take-while (complement #{'&})) (map -arg-symbol))
                         args (into (vector) xf norm)
                         var  (-arg-symbol (last norm))]
                     `(into ~args ~var)) ; NOTE: append variadic arguments
                   (into (vector) (map -arg-symbol) norm))]
     form)))

(defmethod ->form ::normalized-arglist
  [arglist _]
  (into
   (vector)
   (map
    (fn [arg]
      (cond
        (vector? arg) (if (some #{:as} arg) arg (conj arg :as (gensym 'arg)))
        (map? arg)    (update arg :as (fnil identity (gensym 'arg)))
        :else         arg)))
   arglist))

(defmethod ->form [::bind ::fn/args]
  [ctx _]
  (let [ctx      (bind ctx [::fn/args])
        bindings (bindings ctx [::fn/args])
        trace    (when (trace? ctx :fm/args) (->forms ctx [::trace ::fn/args]))
        body     (cond
                   (not (handler? ctx))     (->form ctx ::anomaly/received)
                   (metadata? ctx :fm/args) (->form ctx [::validate ::fn/args])
                   :else                    (->form ctx [::bind ::fn/ret]))]
    `(let [~@bindings]
       ~@trace
       ~body)))

(defmethod ->form ::anomaly/received ; TODO: split by signature
  [ctx _]
  (let [args    (->form ctx ::fn/args)
        handler (->form ctx :fm/handler)
        ident   (->form ctx :fm/ident)
        body    (if (metadata? ctx :fm/args)
                  (->form ctx [::validate ::fn/args])
                  (->form ctx [::bind ::fn/ret]))]
    `(if (anomaly/anomalous? ~args)
       (~handler
        {:fm/ident       ~ident
         ::anomaly/ident ::anomaly/received
         ::anomaly/args  ~args})
       ~body)))

(defmethod ->form [::validate ::fn/args]
  [ctx _]
  (let [ctx       (bind ctx [::fn/conformed-args])
        bindings  (bindings ctx [::fn/conformed-args])
        trace     (->forms ctx [::trace ::fn/conformed-args])
        conformed (->form ctx ::fn/conformed-args)
        handler   (->form ctx :fm/handler)
        ident     (->form ctx :fm/ident)
        args-spec (->form ctx :fm/args)
        args      (->form ctx ::fn/args)
        body      (if (conform? ctx :fm/args)
                    (->form ctx [::bind ::fn/conformed-args])
                    (->form ctx [::bind ::fn/ret]))]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/args
           ::s/explain-data (s/explain-data ~args-spec ~args)})
         ~body))))

(defmethod ->form ::fn/conformed-args
  [ctx _]
  (or
   (get-in ctx [::bindings ::fn/conformed-args ::symbol])
   (let [spec (->form ctx :fm/args)
         args (->form ctx ::fn/args)]
     `(s/conform ~spec ~args))))

(defmethod ->form [::bind ::fn/conformed-args]
  [ctx _]
  (let [args      (get-in ctx [::bindings ::fn/args ::symbol])
        conformed (->form ctx ::fn/conformed-args)
        body      (->form ctx [::bind ::fn/ret])]
    `(let [~args ~conformed]
       ~body)))

(defmethod ->form [::bind ::fn/ret]
  [ctx _]
  (let [ctx      (bind ctx [::fn/ret])
        bindings (bindings ctx [::fn/ret])
        trace    (->forms ctx [::trace ::fn/ret])
        ret      (->form ctx ::fn/ret)
        handler  (->form ctx :fm/handler)
        ident    (->form ctx :fm/ident)
        args     (->form ctx ::fn/args)
        body     (cond
                   (metadata? ctx :fm/ret) (->form ctx [::validate ::fn/ret])
                   (metadata? ctx :fm/rel) (->form ctx [::validate ::fn/rel])
                   :else                   ret)]
    `(let [~@bindings]
       ~@trace
       (if (anomaly/anomalous? ~ret) ; TODO: `:fm.anomaly/deep-detect?`, `:fm/ignore`
         (~handler
          {:fm/ident       ~ident
           ::anomaly/ident ::anomaly/nested ; ALT: `:fm.anomaly/propagated`
           ::anomaly/args  ~args
           ::anomaly/ret   ~ret}) ; TODO: (if (map? ret) ret ,,,)
         ~body))))

(defmethod ->form ::fn/ret
  [ctx tag]
  (or
   (get-in ctx [::bindings ::fn/ret ::symbol])
   (->form ctx [tag (->signature-tag ctx)])))

(defmethod ->form [::fn/ret ::signature]
  [ctx _]
  (let [body (get-in ctx [::conformed-definition :fm.definition/rest 1 :fm.signature/body])]
    `(do ~@body)))

(defmethod ->form [::fn/ret ::signatures]
  [ctx _]
  (let [index (get ctx ::signature-index 0)
        body  (get-in ctx [::conformed-definition :fm.definition/rest 1 index :fm.signature/body])]
    `(do ~@body)))

(defmethod ->form [::validate ::fn/ret]
  [ctx _]
  (let [ctx       (bind ctx [::fn/conformed-ret])
        bindings  (bindings ctx [::fn/conformed-ret])
        trace     (->forms ctx [::trace ::fn/conformed-ret])
        conformed (->form ctx ::fn/conformed-ret)
        handler   (->form ctx :fm/handler)
        ident     (->form ctx :fm/ident)
        args      (->form ctx ::fn/args)
        ret-spec  (->form ctx :fm/ret)
        ret       (->form ctx ::fn/ret)
        body      (cond
                    (conform? ctx :fm/ret)  (->form ctx [::bind ::fn/conformed-ret])
                    (metadata? ctx :fm/rel) (->form ctx [::validate ::fn/rel])
                    :else                   ret)]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/ret
           ::anomaly/args   ~args
           ::s/explain-data (s/explain-data ~ret-spec ~ret)})
         ~body))))

(defmethod ->form ::fn/conformed-ret
  [ctx _]
  (or
   (get-in ctx [::bindings ::fn/conformed-ret ::symbol])
   (let [ret-spec (->form ctx :fm/ret)
         ret      (->form ctx ::fn/ret)]
     `(s/conform ~ret-spec ~ret))))

(defmethod ->form [::bind ::fn/conformed-ret]
  [ctx _]
  (let [ret       (->form ctx ::fn/ret)
        conformed (->form ctx ::fn/conformed-ret)
        body      (if (metadata? ctx :fm/rel)
                    (->form ctx [::validate ::fn/rel])
                    ret)]
    `(let [~ret ~conformed]
       ~body)))

(defmethod ->form [::validate ::fn/rel]
  [ctx _]
  (let [rel   (->form ctx :fm/rel)
        args  (->form ctx ::fn/args)
        ret   (->form ctx ::fn/ret)
        ident (->form ctx :fm/ident)]
    `(if (s/valid? ~rel {:args ~args :ret ~ret})
       ~ret
       {:fm/ident        ~ident
        ::anomaly/ident  ::anomaly/rel
        ::s/explain-data (s/explain-data ~rel {:args ~args :ret ~ret})})))


   ;;;
   ;;; NOTE: `->forms` implementations
   ;;;


(defmethod ->forms ::fn/definition
  [ctx tag]
  (let [sig-tag  (->signature-tag ctx)
        form-tag (or (get-in ctx [::metadata :fm/sequent :fm.sequent/ident])
                     (get ctx ::ident))
        tag      [tag sig-tag form-tag]]
    (->forms ctx tag)))

(defmethod ->forms [::fn/definition ::signature ::fn]
  [ctx _]
  (let [arglist (get-in ctx [::metadata :fm/arglists 0])
        body    (->form ctx ::fn/body) ; TODO: when `::fn/body`
        forms   (list arglist body)]
    forms))

(defmethod ->forms [::fn/definition ::signatures ::fn]
  [ctx _]
  (let [arglists (get-in ctx [::metadata :fm/arglists])
        forms    (map-indexed
                  (fn [i arglist]
                    (let [ctx  (assoc ctx ::signature-index i)
                          body (->form ctx ::fn/body)] ; TODO: when `::fn/body`
                      (list arglist body)))
                  arglists)]
    forms))

#_(defmethod ->forms [::fn/definition ::signature ::sequent]
    [ctx _]
    `([& argxs]
      (try
        (dispatch)
        (catch))))

(defmethod ->forms [::s/form :fm/spec :fm/specv :fm.context/positional :params]
  [params _]
  (into
   (vector)
   (comp
    (map-indexed (fn [i param] (vector (keyword (str i)) param)))
    (mapcat identity))
   params))

(defmethod ->forms [::s/form :fm/spec :fm/specv :fm.context/positional :var-params]
  [var-params _]
  `(:& ~var-params))

(defmethod ->forms [::trace :fm.trace/default]
  [ctx [_ form-tag]]
  (let [trace (->form ctx :fm/trace)
        ident (->form ctx :fm/ident)
        form  (->form ctx form-tag)
        data  {:fm/ident ident form-tag form}
        form  (list trace data)
        forms (list form)]
    forms))



#_(defmethod ->form ::var-symbol
    [_ ctx]
    (with-meta
      (->form ::fn/symbol ctx)
      (->form ::fn/var-metadata ctx)))

#_(defmethod ->form ::var-metadata
    [_ ctx]
    (into
     (hash-map)
     (map (fn [k] [k (->form [::fn/var-metadata k] ctx)]))
     (hash-set :fm/doc :fm/arglists)))

#_(defmethod ->def ::fn
    [parameters]
    (let [params (assoc parameters ::ident ::fn)
          ctx    (->context params)
          sym    (->form ::fn/var-symbol ctx)
          form   (->form ::fn ctx)]
      `(def ~sym ~form)))
