(ns fm.form
  (:refer-clojure :exclude [fn?])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as core.specs]
   [fm.anomaly :as anomaly]
   [fm.form.fn :as fn]
   [fm.form.sequent :as sequent]
   [fm.lib :as lib]))

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

  ;; TODO: `spec2` requires symbolic specs; no inline (fn ,,,), `comp`, etc.
  ;; TODO: unify with `clojure.core.specs.alpha/...`

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

  ;; TODO: generators
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

  ;; NOTE: see `::core.specs/param-list`, `::core.specs/binding-form`
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

  ;; NOTE: "kinds"?
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

(s/def ::outer-metadata (s/or ::metadata ::metadata :nil nil?))
(s/def ::inner-metadatas (s/* ::outer-metadata))

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

(s/def ::signature-index int?)

(s/def ::context
  (s/keys
   :opt
   [::ident
    ::ns
    ::defaults
    ::definition
    ::conformed-definition
    ::metadata
    ::outer-metadata
    ::inner-metadatas
    ::bindings
    ::signature-index]))


   ;;;
   ;;; NOTE: `fm` specs
   ;;;

(s/def :fm/ident
  qualified-keyword?)

(s/def :fm/arglists
  (s/* ::core.specs/param-list)) ; ALT: `::argv`

(s/def :fm/doc
  (s/or
   ::definition string?
   ::metadata (s/* (some-fn string? nil?))))

(s/def :fm/args
  (s/or
   ::definition :fm.form/argv
   ::metadata (s/* (s/or :fm.form/argv :fm.form/argv :nil nil?))))

(s/def :fm/ret
  (s/or
   ::definition (s/or :fm.form.fn/arg :fm.form.fn/arg :fm.form/argv :fm.form/argv)
   ::metadata (s/* (s/or :fm.form.fn/arg :fm.form.fn/arg :nil nil?))))

(s/def :fm/rel
  (s/or
   ::spec (some-fn spec-form? spec-keyword?)
   ::fn (some-fn fn-form? bound-fn?)
   ::metadata ,,,))

(s/def :fm/trace
  (s/or
   ::pred (some-fn true? set?)
   ::fn (some-fn fn-form? bound-fn?)
   ::metadata ,,,))

(s/def :fm/conform
  any?) ; bool, set

(s/def :fm/handler
  (s/or
   ::fn (some-fn fn-form? bound-fn?)
   ::metadata ,,,))

(s/def :fm/handler?
  boolean?)

(s/def :fm/throw any?) ; (fm/fn ^:fm/throw [a] (inc a)) == (fn [a] (inc a))

(s/def :fm/sequent
  (s/keys
   :opt
   [:fm.sequent/ident
    :fm.sequent/unit
    :fm.sequent/combine])) ; TODO: full definition


   ;;;
   ;;; NOTE: top-level multimethods, hierarchies
   ;;;

(defn ->signature-tag ; ALT: ident
  "Produces a tag that describes the signature of the `fm` form"
  [ctx]
  (get-in ctx [::conformed-definition :fm.definition/rest 0]))

(def form-hierarchy "Specifies an ontology of form tags"
  (->
   (make-hierarchy)
   (derive :fm.sequent/conse ::sequent)
   (derive :fm.sequent/nonse ::sequent)
   (derive :fm.sequent/merge ::sequent)))

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
    tag))

(def metadata-hierarchy
  "Specifies an ontology to concisely handle special cases of combining metadata
  forms.
  `:default` — keeps all unrecognized metadata keys exactly as-typed.
  `:fm.metadata/default` — catch-all for recognized keys. the resulting metadata
  will be either the same size as `:fm/arglists` or one element larger to
  include outer metadata.
  `:fm.metadata/fallback` — when a signature has no inner metadata, the outer
  metadata for that key will be used as a fallback. the resulting metadata form
  will always be the same size as `:fm/arglists`.
  `:fm.context/default` — useful for concisely expressing overlap between
  contexts."
  (->
   (make-hierarchy)
   (derive :fm.metadata/fallback  :fm.metadata/default)
   (derive :fm/throw!             :fm.metadata/fallback)
   (derive :fm/args               :fm.metadata/fallback)
   (derive :fm/ret                :fm.metadata/fallback)
   (derive :fm/rel                :fm.metadata/fallback)
   (derive :fm/trace              :fm.metadata/fallback)
   (derive :fm/conform            :fm.metadata/fallback)
   (derive :fm/handler            :fm.metadata/fallback)
   (derive :fm/handler?           :fm.metadata/fallback)
   (derive :fm.context/nominal    :fm.context/default)
   (derive :fm.context/positional :fm.context/default))) ; TODO: atom, `..metadata/hierarchy`

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
  (when (some? (::form binding))
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
        metadata  (get-in ctx [::metadata tag index])
        metadata? (if (sequential? metadata)
                    (seq metadata)
                    (some? metadata))]
    metadata?))

(defn trace? [ctx tag]
  (swap! trace-atom conj ["trace?" tag])
  (let [index    (get ctx ::signature-index 0)
        metadata (or
                  (get-in ctx [::metadata :fm/trace index])
                  (get-in ctx [::defaults :fm/trace]))
        trace?   (cond
                   (set? metadata) metadata
                   (not metadata)  (constantly false)
                   :else           (constantly true))]
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


   ;;;
   ;;; NOTE: top-level `->form` implementations
   ;;;

(defmethod ->form ::fn
  [ctx _]
  (let [conformed  (lib/conform-throw ::definition (get ctx ::definition))
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

#_(defmethod ->form ::fn
    [ctx _]
    (let [conformed  (lib/conform-throw ::fn/definition (get ctx ::definition))
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
        xf         (map (juxt identity (partial ->metadata ctx)))
        tags       (into (hash-set :fm/ident :fm/arglists) (keys outer))
        metadata   (into (hash-map) xf tags)]
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
        xf         (map (juxt identity (partial ->metadata ctx)))
        tags       (into (hash-set :fm/ident :fm/arglists) (mapcat keys) (cons outer inners))
        metadata   (into (hash-map) xf tags)]
    metadata))

(def spec-param-tags
  (hash-set
   :fm.context/nominal
   ::s/registry-keyword
   ::positional-binding-map))

(def contains-spec-param?
  (partial lib/deep-contains? spec-param-tags))

(defmethod ->metadata ::signature-index
  [ctx _]
  (let [index     (get ctx ::signature-index)
        conformed (get-in ctx [::conformed-definition :fm.definition/rest 1])
        conformed (if (sequential? conformed) (get conformed index) conformed)
        argv      (get conformed :fm.signature/argv)
        retv      (get conformed :fm.signature/retv)]
    (merge
     (when (contains-spec-param? argv) {:fm/args (->metadata argv [:fm/args ::conformed-specv])})
     (when (contains-spec-param? retv) {:fm/ret (->metadata retv [:fm/args ::conformed-specv])}))))

(defmethod ->metadata :fm/ident
  [ctx _]
  (->form ctx :fm/ident))

  ;; TODO: unify common cases of `:fm/args` and `:fm/arglists`
(defmethod ->metadata :fm/arglists
  [ctx tag]
  (let [tag [tag (->signature-tag ctx)]]
    (->metadata ctx tag)))

(defmethod ->metadata [:fm/arglists ::signature]
  [ctx _]
  (let [argv    (get-in ctx [::conformed-definition :fm.definition/rest 1 ::argv])
        arglist (->metadata argv [:fm/arglist ::conformed-specv])]
    (vector arglist)))

(defmethod ->metadata [:fm/arglists ::signatures]
  [ctx _]
  (let [argvs    (map ::argv (get-in ctx [::conformed-definition :fm.definition/rest 1]))
        arglists (map (fn [argv] (->metadata argv [:fm/arglist ::conformed-specv])) argvs)]
    (vec arglists)))

(defmethod ->metadata [:fm/arglist ::conformed-specv]
  [[context data] tag]
  (let [data (case context
               :fm.context/positional data
               :fm.context/nominal    (first data))]
    (->metadata data [:fm/arglist context ::conformed-param-list])))

(defmethod ->metadata [:fm/arglist :fm.context/positional ::conformed-param-list]
  [conformed tag]
  (let [params   (when-let [params (get conformed :params)] (->metadata params (conj tag :params)))
        var-form (when-let [var-params (get conformed :var-params)] (->metadata var-params (conj tag :var-params)))
        as-form  (when-let [as-form (get conformed :as-form)] (->metadata as-form (conj tag :as-form)))]
    `[~@params ~@var-form ~@as-form]))

(defmethod ->metadata [:fm/arglist :fm.context/nominal ::conformed-param-list]
  [conformed tag]
  (let [map-destructure (->metadata (get conformed :params) (conj tag :params))
        map-destructure (if-let [as-sym (get-in conformed [:as-form :as-sym])]
                          (update map-destructure :as as-sym)
                          map-destructure)]
    `[~map-destructure]))

(defn -arglist-metadata
  [[tag data]]
  (->metadata data [:fm/arglist tag]))

(defn -normalized-binding-tuple
  [[k [tag conformed]]]
  (let [data (case tag
               :local-symbol    conformed
               :map-destructure (update conformed :as (fnil identity (symbol (name k))))
               :seq-destructure (update conformed :as-form (fnil identity {:as :as :as-sym (symbol (name k))})))]
    [tag data]))

(def -normalized-arglist-metadata
  (comp
   -arglist-metadata
   -normalized-binding-tuple))

(defmethod ->metadata [:fm/arglist :fm.context/positional ::conformed-param-list :params]
  [params _]
  (into
   (vector)
   (map -arglist-metadata)
   params))

(defmethod ->metadata [:fm/arglist :fm.context/nominal ::conformed-param-list :params]
  [params _]
  (into
   (hash-map)
   (map -arglist-metadata)
   params))

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
  (into
   (hash-map)
   (map (juxt -normalized-arglist-metadata first))
   m))

(defmethod ->metadata [:fm/arglist :local-symbol]    [sym _] sym)
(defmethod ->metadata [:fm/arglist :map-destructure] [m _] m)
(defmethod ->metadata [:fm/arglist :seq-destructure]
  [conformed _]
  (let [forms   (map -arglist-metadata (get conformed :forms))
        as-form (get-in conformed [:as-form :as-sym])]
    `[~@forms ~@as-form]))

  ;; TODO: argv -> args; args -> param-list
(defn zipv-args
  "Zips outer `:fm/args` to match an inner param-list"
  [argv args]
  (lib/zipvf vector? (fn [_ a] a) argv args))

(defmethod ->metadata [:fm/args ::signatures]
  [ctx _]
  (let [outer      (get-in ctx [::outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::inner-metadatas))
        _          (map
                    (partial lib/conform-throw :fm/args)
                    (remove nil? (cons outer inners)))
        signatures (get-in ctx [::conformed-definition :fm.definition/rest 1])
        argvs      (map ::argv signatures)
        args       (map-indexed
                    (fn [i inner]
                      (let [argv (nth argvs i)]
                        (when-let [args (or inner outer)]
                          (zipv-args argv args))))
                    inners)]
    (vec args))) ; TODO: qualify, `s/explicate`

(defmethod ->metadata [:fm/args ::conformed-specv]                        [[context data] tag] (->metadata data (conj tag context)))
(defmethod ->metadata [:fm/args ::conformed-specv :fm.context/nominal]    [[data] _] (vector (->metadata data [:fm/args ::conformed-param-list])))
(defmethod ->metadata [:fm/args ::conformed-specv :fm.context/positional] [data _] (->metadata data [:fm/args ::conformed-param-list]))

(defmethod ->metadata [:fm/args ::conformed-param-list]
  [conformed tag]
  (let [params   (when-let [params (get conformed :params)] (->metadata params (conj tag :params)))
        var-form (when-let [var-params (get conformed :var-params)] (->metadata var-params (conj tag :var-params)))
        as-form  (when-let [as-form (get conformed :as-form)] (->metadata as-form (conj tag :as-form)))]
    `[~@params ~@var-form ~@as-form]))

(defmethod ->metadata [:fm/args ::conformed-param-list :params]
  [params _]
  (into
   (vector)
   (mapcat
    (fn [[tag conformed]]
      (let [form (->metadata conformed [:fm/args tag])]
        (if (sequential? form) form (vector form)))))
   params))

(defmethod ->metadata [:fm/args ::conformed-param-list :var-params]
  [var-params _]
  (let [[tag conformed] (get var-params :var-form)
        var-form        (->metadata conformed [:fm/args tag])]
    `(& ~var-form)))

(defmethod ->metadata [:fm/args ::conformed-param-list :as-form]
  [as-form _]
  (let [as-sym (get as-form :as-sym)]
    `(:as ~as-sym)))


  ;; NOTE: positional
(defmethod ->metadata [:fm/args ::s/registry-keyword]      [k _] k)
(defmethod ->metadata [:fm/args ::positional-binding-map]  [m _] (first (keys m)))
(defmethod ->metadata [:fm/args ::core.specs/binding-form] [_ _] `any?) ; TODO: additional inference

  ;; NOTE: nominal
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
  {::symbol (last (get ctx ::fn/normalized-argv))
   ::form   (->form ctx tag)})


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
   (keyword
    (str (get ctx ::ns))
    (name
     (or
      (get-in ctx [::conformed-definition :fm.definition/simple-symbol])
      (gensym (name (get ctx ::ident))))))))

(defmethod ->form :fm/symbol
  [ctx _]
  (symbol (->form ctx :fm/ident)))

(defmethod ->form :fm/simple-symbol
  [ctx _]
  (symbol (name (->form ctx :fm/ident))))

(defmethod ->form :fm/args
  [ctx _]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings :fm/args index ::symbol])
     (let [args (get-in ctx [::metadata :fm/args index])
           form (->form args [:fm/args ::fn/args])]
       form))))

(defmethod ->form [:fm/args ::fn/args]
  [args _]
  (let [parts (partition-by (hash-set '&) args)
        args  (when (not= (count parts) 2) (->forms (first parts) [:fm/args ::fn/args])) ; NOTE: count `2` implies [& ,,,]
        var   (when (>    (count parts) 1) (->forms (first (last parts)) [:fm/args ::fn/variadic-arg]))]
    `(s/cat ~@args ~@var)))

(defmethod ->form [:fm/args ::fn/arg]
  [arg _]
  (let [[tag _] (lib/conform-throw ::fn/arg arg)]
    (case tag
      ::fn/args `(s/spec ~(->form arg [:fm/args ::fn/args]))
      arg)))

(defmethod ->form [:fm/args ::fn/variadic-arg ::fn/arg]
  [arg _]
  (let [[tag _] (lib/conform-throw ::fn/arg arg)]
    (case tag
      ::fn/args (->form arg [:fm/args ::fn/variadic-arg ::fn/args])
      `(s/* ~arg))))

(defmethod ->form [:fm/args ::fn/variadic-arg ::fn/args]
  [args _]
  (let [ctx {::args args ::index 0}]
    (->form ctx [:fm/args ::fn/variadic-arg ::fn/args ::recur])))

(defmethod ->form [:fm/args ::fn/variadic-arg ::fn/args ::recur]
  [ctx tag]
  (let [i    (get ctx ::index)
        args (get ctx ::args)
        arg  (nth args i)] ; NOTE: seq
    (if (= arg '&)
      (->form (update ctx ::index inc) tag) ; NOTE: skip `&`
      (let [index-tag (keyword (str i))
            arg       (->form arg [:fm/args ::fn/arg])
            last?     (or (= i (dec (count args))) (= (nth args (inc i)) '&))
            variadic? (some #{'&} args)
            rest      (if last?
                        (if variadic?
                          (->forms (last args) [:fm/args ::fn/variadic-arg])
                          (list :rest `(s/* any?))) ; TODO: revisit tags
                        (list :rest (->form (update ctx ::index inc) tag)))]
        `(s/? (s/cat ~index-tag ~arg ~@rest))))))

  ;; ALT: `s/keys*`; drop `strs`, `syms` support
(defmethod ->form [:fm/args ::fn/variadic-arg ::fn/keyword-args-map]
  [arg _]
  (let [->form (fn [[k arg]] ; TODO: revisit tags
                 (let [form (->form arg [:fm/args ::fn/arg])]
                   `(s/cat :k #{~k} :form ~form)))
        ->tag  (comp keyword key)
        forms  (mapcat (juxt ->tag ->form) arg)]
    `(s/* (s/alt ~@forms))))

(defmethod ->form :fm/ret
  [ctx _]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings :fm/ret index ::symbol])
     (when-let [ret (get-in ctx [::metadata :fm/ret index])]
       (let [[tag _] (lib/conform-throw ::fn/arg ret)]
         (case tag
           ::fn/args (->form ret [:fm/args ::fn/args])
           ret))))))

(defmethod ->form :fm/rel
  [ctx _]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings :fm/rel index ::symbol])
     (when-let [form (get-in ctx [::metadata :fm/rel index])]
       (let [spec? (some-fn spec-form? spec-keyword?)
             fn?   (some-fn fn-form? bound-fn?)]
         (cond
           (spec? form) `(fn [rel#] (s/valid? ~form rel#))
           (fn? form)   form))))))

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
           (pred? form) (get-in ctx [::defaults :fm/trace-fn])
           (fn? form)   form
           :else        `(partial ~(get-in ctx [::defaults :fm/trace-fn]) ~form)))))))

(defmethod ->form :fm/handler
  [ctx _]
  (let [index (get ctx ::signature-index 0)]
    (or
     (get-in ctx [::bindings :fm/handler index ::symbol])
     (let [form (get-in ctx [::metadata :fm/handler index])
           fn?  (some-fn fn-form? bound-fn?)]
       (cond
         (fn? form) form
         :else      (get-in ctx [::defaults :fm/handler]))))))

  ;; TODO: infer `:fm/handler?` (when #{:fm/anomaly} ret)
  ;; NOTE: introduces dependency ordering

(defmethod ->form ::fn/normalized-argv
  [argv _]
  (let [argv (if (some #{:as} argv) argv (conj argv :as (gensym 'argv)))
        argv (mapv
              (fn [arg]
                (cond
                  (vector? arg) (if (some #{:as} arg) arg (conj arg :as (gensym 'arg)))
                  (map? arg)    (update arg :as (fnil identity (gensym 'arg)))
                  :else         arg))
              argv)]
    argv))

(defmethod ->form ::fn/body
  [ctx _]
  (let [args    (->form ctx ::fn/args)
        body    (if (seq args)
                  (->form ctx [::bindings ::fn/args])
                  (->form ctx [::bindings ::fn/ret]))
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
   (let [argv (drop-last 2 (get ctx ::fn/normalized-argv))
         form (if (some #{'&} argv)
                (let [xf   (comp (take-while (complement #{'&})) (map fn/arg->symbol))
                      args (into (vector) xf argv)
                      var  (fn/arg->symbol (last argv))]
                  `(into ~args ~var)) ; NOTE: append variadic arguments
                (into (vector) (map fn/arg->symbol) argv))]
     form)))

(defmethod ->form [::bindings ::fn/args]
  [ctx _]
  (let [ctx      (bind ctx [::fn/args])
        bindings (bindings ctx [::fn/args])
        trace    (->forms ctx :fm.trace/args)
        body     (cond
                   (not (handler? ctx))     (->form ctx ::anomaly/received)
                   (metadata? ctx :fm/args) (->form ctx [::validate ::fn/args])
                   :else                    (->form ctx [::bindings ::fn/ret]))]
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
                  (->form ctx [::bindings ::fn/ret]))]
    `(if (anomaly/anomalous? ~args)
       (~handler
        {:fm/ident         ~ident
         :fm.anomaly/ident :fm.anomaly/received
         :fm.anomaly/args  ~args}) ; NOTE: basically a worse stack trace
       ~body)))

(defmethod ->form [::validate ::fn/args]
  [ctx _]
  (let [ctx       (bind ctx [::fn/conformed-args])
        bindings  (bindings ctx [::fn/conformed-args])
        trace     (->forms ctx :fm.trace/conformed-args)
        conformed (->form ctx ::fn/conformed-args)
        handler   (->form ctx :fm/handler)
        ident     (->form ctx :fm/ident)
        args-spec (->form ctx :fm/args)
        args      (->form ctx ::fn/args)
        body      (if (conform? ctx :fm/args)
                    (->form ctx [::bindings ::fn/conformed-args])
                    (->form ctx [::bindings ::fn/ret]))]
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
   (let [args-spec (->form ctx :fm/args)
         args      (->form ctx ::fn/args)]
     `(s/conform ~args-spec ~args))))

(defmethod ->form [::bindings ::fn/conformed-args]
  [ctx _]
  (let [args      (last (get ctx ::fn/normalized-argv))
        conformed (->form ctx ::fn/conformed-args)
        body      (->form ctx [::bindings ::fn/ret])]
    `(let [~args ~conformed]
       ~body)))

(defmethod ->form [::bindings ::fn/ret]
  [ctx _]
  (let [ctx      (bind ctx [::fn/ret])
        bindings (bindings ctx [::fn/ret])
        trace    (->forms ctx :fm.trace/ret)
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

(defmethod ->form [::fn/ret ::fn/signature]
  [ctx _]
  (let [body (get-in ctx [::conformed-definition :fm.definition/rest 1 ::body])]
    `(do ~@body))) ; TODO: additional analysis?

(defmethod ->form [::fn/ret ::fn/signatures]
  [ctx _]
  (let [index (get ctx ::signature-index 0)
        body  (get-in ctx [::conformed-definition :fm.definition/rest 1 index ::body])]
    `(do ~@body))) ; TODO: additional analysis?

(defmethod ->form [::validate ::fn/ret]
  [ctx _]
  (let [ctx       (bind ctx [::fn/conformed-ret])
        bindings  (bindings ctx [::fn/conformed-ret])
        trace     (->forms ctx :fm.trace/conformed-ret)
        conformed (->form ctx ::fn/conformed-ret)
        handler   (->form ctx :fm/handler)
        ident     (->form ctx :fm/ident)
        args      (->form ctx ::fn/args)
        ret-spec  (->form ctx :fm/ret)
        ret       (->form ctx ::fn/ret)
        body      (cond
                    (conform? ctx :fm/ret)  (->form ctx [::bindings ::fn/conformed-ret])
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

(defmethod ->form [::bindings ::fn/conformed-ret]
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
    `(if (~rel {:args ~args :ret ~ret})
       ~ret
       {:fm/ident        ~ident
        ::anomaly/ident  ::anomaly/rel
        ::s/explain-data (s/explain-data ~rel {:args ~args :ret ~ret})})))


   ;;;
   ;;; NOTE: `->forms` implementations
   ;;;

(defmethod ->forms [:fm/args ::fn/args]
  [args _]
  (let [->form   (fn [arg] (->form arg [:fm/args ::fn/arg]))
        ->tagged (fn [i form]
                   (let [tag (keyword (str i))] ; TODO: zip `argv`
                     (vector tag form)))
        ->forms  (comp
                  (map ->form)
                  (map-indexed ->tagged)
                  (mapcat identity))
        forms    (sequence ->forms args)]
    forms))

(defmethod ->forms [:fm/args ::fn/variadic-arg]
  [arg _]
  (let [[tag _] (lib/conform-throw ::fn/variadic-arg arg)
        form    (case tag
                  ::fn/arg                 (->form arg [:fm/args ::fn/variadic-arg ::fn/arg])
                  ::fn/keyword-args-map    (->form arg [:fm/args ::fn/variadic-arg ::fn/keyword-args-map])
                  ::sequence-spec-form arg)
        forms   (list :& form)]
    forms))

(defmethod ->forms ::fn/definition
  [ctx tag]
  (let [tag [tag (->signature-tag ctx)]]
    (->forms ctx tag)))

(defmethod ->forms [::fn/definition ::fn/signature]
  [ctx _]
  (let [argv  (get-in ctx [::conformed-definition :fm.definition/rest 1 ::argv])
        norm  (->form argv ::fn/normalized-argv)
        argv  (if (some #{:as} argv) (vec (drop-last 2 argv)) argv) ; TODO: `::fn/argv`
        ctx   (assoc ctx ::signature-index 0 ::fn/normalized-argv norm)
        body  (->form ctx ::fn/body)
        forms (list argv body)]
    forms))

(defmethod ->forms [::fn/definition ::fn/signatures]
  [ctx _]
  (let [signatures (get-in ctx [::conformed-definition :fm.definition/rest 1])
        forms      (map-indexed
                    (fn [i signature]
                      (let [argv (get signature ::argv)
                            norm (->form argv ::fn/normalized-argv)
                            argv (if (some #{:as} argv) (vec (drop-last 2 argv)) argv)
                            ctx  (assoc ctx ::signature-index i ::fn/normalized-argv norm)
                            body (->form ctx ::fn/body)] ; TODO: when `::fn/body`
                        (list argv body)))
                    signatures)]
    forms))

(defmethod ->forms :fm.trace/args
  [ctx _]
  (when (trace? ctx :fm/args)
    (let [trace (->form ctx :fm/trace)
          ident (->form ctx :fm/ident)
          args  (->form ctx ::fn/args)
          forms [(list trace {:fm/ident ident :fm.trace/args args})]] ; TODO: revisit tags
      forms)))

(defmethod ->forms :fm.trace/conformed-args
  [ctx _]
  (when (and (trace? ctx :fm/args) (conform? ctx :fm/args))
    (let [trace (->form ctx :fm/trace)
          ident (->form ctx :fm/ident)
          args  (->form ctx ::fn/conformed-args)
          forms [(list trace {:fm/ident ident :fm.trace/conformed-args args})]]
      forms)))

(defmethod ->forms :fm.trace/ret
  [ctx _]
  (when (trace? ctx :fm/ret)
    (let [trace (->form ctx :fm/trace)
          ident (->form ctx :fm/ident)
          ret   (->form ctx ::fn/ret)
          forms [(list trace {:fm/ident ident :fm.trace/ret ret})]]
      forms)))

(defmethod ->forms :fm.trace/conformed-ret
  [ctx _]
  (when (and (trace? ctx :fm/ret) (conform? ctx :fm/ret))
    (let [trace (->form ctx :fm/trace)
          ident (->form ctx :fm/ident)
          ret   (->form ctx ::fn/conformed-ret)
          forms [(list trace {:fm/ident ident :fm.trace/conformed-ret ret})]]
      forms)))





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

#_(defn defm
    [parameters]
    (let [params (assoc parameters ::ident ::fn)
          ctx    (->context params)
          sym    (->form ::fn/var-symbol ctx)
          form   (->form ::fn ctx)]
      `(def ~sym ~form)))
