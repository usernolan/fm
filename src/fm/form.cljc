(ns fm.form
  (:refer-clojure :exclude [fn?])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
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
   ;;; NOTE: `fm.form` helpers
   ;;;

  ;; TODO: `spec2` requires symbolic specs; no inline (fn ,,,), `comp`, etc.
  ;; TODO: unify with `clojure.core.specs.alpha/...`

(def multi?
  (partial instance? clojure.lang.MultiFn))

(def fn?
  (some-fn clojure.core/fn? multi?))

  ;; TODO: refactor; (comp #{fn ,,,} resolve)
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
    'some-fn `some-fn}) ; TODO: include `fm` forms

(s/def ::fn-symbol
  (fn [x]
    (*fn-symbol-set* x))) ; NOTE: `s/def` evaluates set, breaks rebinding

(def fn-symbol?
  (partial s/valid? ::fn-symbol))

(s/def ::fn-form
  (s/and
   seq?
   not-empty
   (comp fn-symbol? first))) ; ALT: (comp fn?? eval), (comp #{fn ,,,} deref resolve)

(def fn-form?
  (partial s/valid? ::fn-form))

(s/def ::bound-fn
  (s/and
   symbol?
   (comp fn? deref resolve)))

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

(s/def ::arg-symbol ; TODO: `clojure.core.specs.alpha/local-name`
  (s/and
   symbol?
   (complement #{'&})))


   ;;;
   ;;; NOTE: `fm` specs
   ;;;

(comment ; NOTE: from deprecated `fm.form.lib`

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

  ;;;
  )

(s/def :fm/ident
  qualified-keyword?)

(s/def :fm/arglists
  (s/* vector?)) ; ALT: `s/coll-of`, `s/*`, `argv?`, `::argv`

(s/def :fm/doc
  (s/or
   ::definition string?
   ::metadata (s/* (some-fn string? nil?))))

(s/def :fm/args
  (s/or
   ::definition ::fn/args
   ::metadata (s/* (s/or ::fn/args ::fn/args :nil nil?))))

(s/def :fm/ret
  (s/or
   ::definition ::fn/arg
   ::metadata (s/* (s/or ::fn/arg ::fn/arg :nil nil?))))

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


   ;;;
   ;;; NOTE: `fm.form` specs
   ;;;

(s/def ::ident
  qualified-keyword?)

(s/def ::ns
  (partial instance? clojure.lang.Namespace))

(s/def ::defaults
  any?)

(s/def ::definition
  (s/or
   ::fn/definition ::fn/definition
   ::sequent/definition ::sequent/definition))

(s/def ::conformed-definition
  (s/keys
   :opt [:fm.definition/simple-symbol]
   :req [:fm.definition/rest]))

(s/def ::metadata
  (s/keys
   :opt
   [::ident
    :fm/ident
    :fm/arglists
    :fm/doc
    :fm/args
    :fm/ret
    :fm/rel
    :fm/trace
    :fm/conform
    :fm/handler
    :fm/handler?
    :fm/context]))

(s/def ::outer-metadata (s/or ::metadata ::metadata :nil nil?))
(s/def ::inner-metadatas (s/* ::outer-metadata))

(s/def ::binding
  (s/or
   :default (s/keys :req [::symbol ::form])
   ::metadata (s/* (s/keys :req [::symbol ::form]))))

(s/def ::tag
  (s/or
   :qualified-keyword qualified-keyword?
   :compound (s/coll-of qualified-keyword? :kind vector?)))

(s/def ::bindings
  (s/map-of ::tag ::binding)) ; ALT: `any?` => `::binding`

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
    ::bindings
    ::outer-metadata
    ::inner-metadatas
    ::signature-index]))

  ;; TODO: refine
(s/def ::argv vector?)
(s/def ::seqv
  (s/or
   ::sequent/positional (s/coll-of ::sequent/arg :kind vector?)
   ::sequent/nominal (s/tuple (s/coll-of ::sequent/arg :kind vector?))))


   ;;;
   ;;; NOTE: top-level multimethods, hierarchies
   ;;;

(defn ->signature-tag ; ALT: ident
  "Produces a tag that describes the signature of the `fm` form"
  [ctx]
  (get-in ctx [::conformed-definition :fm.definition/rest 0]))

(defmulti ->form
  "Produces a form to be evaluated as with `eval` or combined with other forms"
  (fn [_ctx tag]
    (swap! trace-atom conj ["->form" tag])
    tag))

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
  will always be the same size as `:fm/arglists`."
  (->
   (make-hierarchy)
   (derive :fm.metadata/fallback :fm.metadata/default)
   (derive :fm/args              :fm.metadata/fallback)
   (derive :fm/ret               :fm.metadata/fallback)
   (derive :fm/rel               :fm.metadata/fallback)
   (derive :fm/trace             :fm.metadata/fallback)
   (derive :fm/conform           :fm.metadata/fallback)
   (derive :fm/handler           :fm.metadata/fallback)
   (derive :fm/handler?          :fm.metadata/fallback)
   (derive ::fn/signature        ::signature)
   (derive ::fn/signatures       ::signatures))) ; TODO: atom, `..metadata/hierarchy`

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

#_(defmethod ->form ::conse [ctx _])
#_(defmethod ->form ::nonse [ctx _])
#_(defmethod ->form ::merge [ctx _])
#_(defmethod ->form ::iso [ctx _])


   ;;;
   ;;; NOTE: `->metadata` helpers
   ;;;

(defn -signature-metadata
  [ctx tag]
  (swap! trace-atom conj ["-signature-metadata" tag])
  (let [outer    (merge
                  (meta (get-in ctx [::conformed-definition :fm.definition/simple-symbol]))
                  (meta (get-in ctx [::conformed-definition :fm.definition/rest 1 tag])))
        ctx      (assoc ctx ::outer-metadata outer)
        xf       (map (juxt identity (partial ->metadata ctx)))
        tags     (into #{:fm/ident :fm/arglists} (keys outer))
        metadata (into {} xf tags)]
    metadata))

(defn -signatures-metadata
  [ctx tag]
  (swap! trace-atom conj ["-signatures-metadata" tag])
  (let [definition (get ctx ::definition)
        signatures (if (symbol? (first definition)) (rest definition) definition)
        outer      (merge
                    (meta (get-in ctx [::conformed-definition :fm.definition/simple-symbol]))
                    (meta (first signatures)))
        inners     (map
                    (comp meta tag)
                    (get-in ctx [::conformed-definition :fm.definition/rest 1]))
        ctx        (assoc ctx ::outer-metadata outer ::inner-metadatas inners)
        xf         (map (juxt identity (partial ->metadata ctx)))
        tags       (into #{:fm/ident :fm/arglists} (mapcat keys) (cons outer inners))
        metadata   (into {} xf tags)]
    metadata))


   ;;;
   ;;; NOTE: `->metadata` implementations
   ;;;

(defmethod ->metadata ::metadata      [ctx _] (->metadata ctx (->signature-tag ctx)))
(defmethod ->metadata ::fn/signature  [ctx _] (-signature-metadata ctx ::argv))
(defmethod ->metadata ::fn/signatures [ctx _] (-signatures-metadata ctx ::argv))
#_(defmethod ->metadata :fm.form.sequent/signature [ctx _] (-signature-metadata ctx ::sequent/left))
#_(defmethod ->metadata :fm.form.sequent/signatures [ctx _] (-signatures-metadata ctx ::sequent/left))

(defmethod ->metadata :fm/ident
  [ctx _]
  (->form ctx :fm/ident))

(defmethod ->metadata :fm/arglists
  [ctx tag]
  (let [tag [tag (->signature-tag ctx)]]
    (->metadata ctx tag)))

(defmethod ->metadata [:fm/arglists ::fn/signature]
  [ctx _]
  (let [signature (get-in ctx [::conformed-definition :fm.definition/rest 1])
        argv      (get signature ::argv)]
    (vector argv)))

#_(defmethod ->metadata [:fm/arglists :fm.form.sequent/signature]
    [ctx _]
    (let [signature (get-in ctx [::conformed-definition :fm.definition/rest 1])
          argv      (get signature ::sequent/left)]
      (vector argv)))

(defmethod ->metadata [:fm/arglists ::fn/signatures]
  [ctx _]
  (let [signatures (get-in ctx [::conformed-definition :fm.definition/rest 1])
        argvs      (map ::argv signatures)]
    (vec argvs)))

(defmethod ->metadata [:fm/args ::fn/signatures]
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
                          (fn/zipv-args argv args))))
                    inners)]
    (vec args))) ; TODO: qualify, `s/explicate`

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
        xs     (map (fn fallback [inner] (or inner outer)) inners)]
    (vec xs)))


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
        argv  (if (some #{:as} argv) (vec (drop-last 2 argv)) argv)
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
