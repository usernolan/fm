(ns fm.form
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [fm.anomaly :as anomaly]
   [fm.form.fn :as fn]
   [fm.form.lib :as lib]))

(comment

  (def trace-atom (atom []))

  ;;;
  )

  ;; TODO: revisit tags
  ;; TODO: runtime `*ignore*`, `s/*compile-asserts*`, etc.; config
  ;; TODO: clarify contextual dependencies?
  ;; TODO: "cut elimination"

(def ^:dynamic *trace*
  false)

(def ^:dynamic *conform*
  false)

  ;; ALT: form?, runtime?
(def ^:dynamic *default-anomaly-handler-symbol*
  `identity)

(def ^:dynamic *default-trace-fn-symbol*
  `prn)


(s/def :fm/ident
  qualified-keyword?)

(s/def :fm/arglists
  (s/coll-of vector?))

#_(s/def :fm/doc
    (s/or
     :fm/definition string?
     :fm/context (s/coll-of string?)))

#_(s/def :fm/args
    (s/or
     :fm/definition ::fn/args
     :fm/context (s/coll-of ::fn/args)))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident
    :fm/arglists]
   :opt
   [:fm/doc
    :fm/args
    :fm/ret
    :fm/rel
    :fm/trace
    :fm/conform
    :fm.anomaly/handler
    :fm.anomaly/handler?
    :fm.sequent/ident
    :fm.sequent/context]))

(s/def ::bindings any?
  #_{:fm/tag1 [{::symbol ,,, ::form ,,,} ,,,]
     :fm/tag2 [{::symbol ,,, ::form ,,,} ,,,]
     ::tag1   {::symbol ,,, ::form ,,,}})

(s/def ::signature-index int?)

(s/def ::context
  (s/keys
   :opt
   [::ns
    ::definition
    ::metadata
    ::bindings
    ::signature-index
    ::fn/conformed-definition ; ALT: `::conformed-definition`
    ::fn/metadata
    ::fn/outer-metadata
    ::fn/inner-metadatas
    ::fn/normalized-argv]))

(defmulti  <<conformed-definition ::ident)
(defmethod <<conformed-definition ::fn
  [{::keys [definition] :as ctx}]
  (assoc
   ctx
   ::fn/conformed-definition
   (lib/conform-throw ::fn/definition definition)))

(defmulti  ->definition-tag ::ident)
(defmethod ->definition-tag ::fn
  [ctx]
  (get-in ctx [::fn/conformed-definition ::fn/rest 0]))

(defmulti  ->ident ::ident)
(defmethod ->ident ::fn
  [ctx]
  (keyword
   (str (get ctx ::ns))
   (name
    (or
     (get-in ctx [::fn/conformed-definition ::fn/simple-symbol?])
     (gensym 'fm)))))

(def ->symbol
  (comp symbol ->ident))

(def ->simple-symbol
  (comp symbol name ->ident))

(defmulti  ->default-metadata-form (fn [tag ctx] (swap! trace-atom conj ["->default-metadata-form" tag]) (->definition-tag ctx)))
(defmethod ->default-metadata-form ::fn/signature
  [tag ctx]
  (let [metadata (get-in ctx [::fn/metadata tag])
        _        (when (s/get-spec tag)
                   (lib/conform-throw tag metadata))]
    metadata))

(defmethod ->default-metadata-form ::fn/signatures
  [tag ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata tag]) ; TODO: `::fn/...`
        inners (map tag (get ctx ::fn/inner-metadatas))
        _      (when (s/get-spec tag)
                 (map
                  (partial lib/conform-throw tag)
                  (remove nil? (cons outer inners))))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (vec inners) ; NOTE: at least one
                 :else                (vec (cons outer inners)))]
    form))

(def ->metadata-form-hierarchy
  "Specifies an ontology to help concisely handle special cases of combining
  metadata forms.

  `:default` — keeps all metadata. delegates to `->default-metadata-form` which
  keeps unrecognized keys exactly as-typed.

  `:fm.metadata/default` — keeps all metadata. when the key is recognized, the
  resulting metadata form will be either the same size as `:fm/arglists` or one
  element larger to include outer metadata.

  `:fm.metadata/fallback` — when a signature has no inner metadata, the outer
  metadata for that key will be used as a fallback. the resulting metadata form
  will always be the same size as `:fm/arglists`."
  (->
   (make-hierarchy)
   (derive :fm/args              :fm.metadata/fallback)
   (derive :fm/ret               :fm.metadata/fallback)
   (derive :fm/rel               :fm.metadata/fallback)
   (derive :fm/trace             :fm.metadata/fallback)
   (derive :fm/conform           :fm.metadata/fallback)
   (derive :fm.anomaly/handler   :fm.metadata/fallback)
   (derive :fm.anomaly/handler?  :fm.metadata/fallback)
   (derive :fm.metadata/fallback :fm.metadata/default)))

(defmulti  ->metadata-form (fn [tag _ctx] (swap! trace-atom conj ["->metadata-form" tag]) tag) :hierarchy #'->metadata-form-hierarchy)
(defmethod ->metadata-form :default ; NOTE: default for keys we don't recognize and `:fm/doc`
  [tag ctx]
  (->default-metadata-form tag ctx))

(defmethod ->metadata-form :fm.metadata/default ; NOTE: default for keys in the ontology
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm.metadata/default ::fn/signature]
  [[tag _] ctx]
  (let [metadata (get-in ctx [::fn/metadata tag])
        _        (when (s/get-spec tag)
                   (lib/conform-throw tag metadata))]
    (vector metadata))) ; NOTE: vector for signature indexing

(defmethod ->metadata-form [:fm.metadata/fallback ::fn/signatures]
  [[tag _] ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata tag])
        inners (map tag (get ctx ::fn/inner-metadatas))
        _      (map
                (partial lib/conform-throw tag)
                (remove nil? (cons outer inners)))
        xs     (map (fn [inner] (or inner outer)) inners)]
    (vec xs)))

(defmethod ->metadata-form :fm/ident
  [_ ctx]
  (->ident ctx))

(defmethod ->metadata-form :fm/arglists
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/arglists ::fn/signature]
  [_ ctx]
  (let [signature (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argv      (get signature ::fn/argv)]
    (vector argv)))

(defmethod ->metadata-form [:fm/arglists ::fn/signatures]
  [_ ctx]
  (let [signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argvs      (map ::fn/argv signatures)]
    (vec argvs)))

(defmethod ->metadata-form [:fm/args ::fn/signatures]
  [_ ctx]
  (let [outer      (get-in ctx [::fn/outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::fn/inner-metadatas))
        _          (map
                    (partial lib/conform-throw :fm/args)
                    (remove nil? (cons outer inners)))
        signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argvs      (map ::fn/argv signatures)
        args       (map-indexed
                    (fn [i inner]
                      (let [argv (nth argvs i)]
                        (when-let [args (or inner outer)]
                          (fn/zipv-args argv args))))
                    inners)]
    (vec args)))

(defmulti  ->metadata ->definition-tag)
(defmethod ->metadata ::fn/signature
  [{::fn/keys [conformed-definition] :as ctx}]
  (let [metadata  (merge
                   (meta (::fn/simple-symbol? conformed-definition))
                   (meta (::fn/argv (second (::fn/rest conformed-definition)))))
        ctx       (assoc ctx ::fn/metadata metadata)
        xf        (map (fn [k] [k (->metadata-form k ctx)]))
        meta-keys (into #{:fm/ident :fm/arglists} (keys metadata))
        metadata  (into {} xf meta-keys)]
    metadata))

(defmethod ->metadata ::fn/signatures
  [{::fn/keys [conformed-definition]
    ::keys    [definition]
    :as       ctx}]
  (let [signatures (if (symbol? (first definition)) (next definition) definition)
        outer      (merge
                    (meta (first signatures))
                    (meta (::fn/simple-symbol? conformed-definition)))
        inners     (map
                    (comp meta ::fn/argv)
                    (second (::fn/rest conformed-definition)))
        ctx        (assoc ctx ::fn/outer-metadata outer ::fn/inner-metadatas inners)
        xf         (map (fn [k] [k (->metadata-form k ctx)]))
        meta-keys  (into #{:fm/ident :fm/arglists} (mapcat keys) (cons outer inners))
        metadata   (into {} xf meta-keys)]
    metadata))

#_(defmethod ->metadata-form [:fm/arglists ::sequent/signature]
    [tag ctx]
    (let [tag (conj tag (->sequent-context-tag ctx))]
      (->metadata-form tag ctx)))
#_(defmethod ->metadata-form [:fm/arglists ::sequent/signature ::sequent/positional] [_ ctx]) #_[::a ::b] #_-> #_([a b])
#_(defmethod ->metadata-form [:fm/arglists ::sequent/signature ::sequent/associative] [_ ctx]) #_[[::a ::b]] #_-> #_([{a ::a b ::b}])
#_(defmethod ->metadata-form [:fm/arglists ::sequent/signatures] [_ ctx]) ; TODO: delegate
#_(defmethod ->metadata-form [:fm/arglists ::sequent/signatures ::sequent/positional] [_ ctx])
#_(defmethod ->metadata-form [:fm/arglists ::sequent/signatures ::sequent/associative] [_ ctx])

(defmulti  <<metadata ::ident)
(defmethod <<metadata ::fn
  [ctx]
  (assoc ctx ::metadata (->metadata ctx)))

(s/def ::parameters
  (s/keys
   :req
   [::ns
    ::definition]))

  ;; ALT: `interpret`, `analyze`, `read`
(defmulti  ->context ::ident)
(defmethod ->context ::fn
  [parameters]
  (->>
   parameters
   <<conformed-definition
   <<metadata))

(def ->binding-hierarchy
  "Specifies an ontology for concisely handling special cases in binding, such
  as binding `::metadata` forms"
  (->
   (make-hierarchy)
   (derive :fm/args            ::metadata)
   (derive :fm/ret             ::metadata)
   (derive :fm/rel             ::metadata)
   (derive :fm/trace           ::metadata)
   (derive :fm.anomaly/handler ::metadata)))

(defmulti  ->form    (fn [tag _ctx] (swap! trace-atom conj ["->form" tag]) tag))
(defmulti  ->forms   (fn [tag _ctx] (swap! trace-atom conj ["->forms" tag]) tag))
(defmulti  ->binding (fn [tag _ctx] (swap! trace-atom conj ["->binding" tag]) tag) :hierarchy #'->binding-hierarchy)
(defmethod ->binding :default
  [tag ctx]
  {::symbol (gensym (name tag))
   ::form   (->form tag ctx)})

(defmethod ->binding ::metadata
  [tag ctx]
  (let [forms   (get-in ctx [::metadata tag])
        binding (::bindings
                 (reduce
                  (fn -deduplicate [acc form] ; NOTE: maintain signature index; trivialize binding deduplication
                    (let [f       (fnil identity (gensym (name tag)))
                          acc     (update-in acc [::form=>symbol form] f)
                          sym     (get-in acc [::form=>symbol form])
                          ctx     (into ctx acc)
                          form    (->form [::bindings tag] ctx)
                          binding {::symbol sym ::form form}
                          acc     (->
                                   acc
                                   (update ::bindings conj binding)
                                   (update ::signature-index inc))]
                      acc))
                  {::bindings [] ::form=>symbol {} ::signature-index 0}
                  forms))]
    binding))

  ;; TODO: zip `::fn/normalized-argv` tags, respect `:as`
  ;; TODO: detect regex-op, insert `s/spec`
(defmethod ->form [::bindings :fm/args]
  [_ ctx]
  (->form ::fn/args-spec ctx)) ; TODO: generalize

(defmethod ->form [::bindings :fm/ret]
  [_ ctx]
  (->form ::fn/ret-spec ctx))

(defmethod ->form [::bindings :fm/rel]
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)
        form  (get-in ctx [::metadata :fm/rel index])
        spec? (some-fn lib/spec-form? lib/spec-keyword?)
        fn?   (some-fn lib/fn-form? lib/bound-fn?)]
    (cond
      (spec? form) `(fn [rel#] (s/valid? ~form rel#))
      (fn? form)   form
      (nil? form)  form)))

(defmethod ->form [::bindings :fm/trace]
  [_ ctx]
  (let [index (or (get-in ctx ::signature-index) 0)
        form  (get-in ctx [::metadata :fm/trace index])
        pred? (some-fn true? set?)
        fn?   (some-fn lib/fn-form? lib/bound-fn?)]
    (cond
      (pred? form) *default-trace-fn-symbol*
      (fn? form)   form
      (nil? form)  form
      :else        `(partial *default-trace-fn-symbol* ~form))))

(defmethod ->form [::bindings :fm.anomaly/handler]
  [_ ctx]
  (let [fn? (some-fn lib/fn-form? lib/bound-fn?)]
    (cond
      (fn? form) form
      :else      *default-anomaly-handler-symbol*)))

(defn bind
  [ctx tags]
  (swap! trace-atom conj ["bind" tags])
  (reduce
   (fn [ctx tag]
     (let [binding (->binding tag ctx)]
       (update ctx ::bindings assoc tag binding)))
   ctx
   tags))

(defn binding->tuple
  [binding]
  (when (some? (::form binding))
    ((juxt ::symbol ::form) binding)))

(defn bindings
  [ctx tags]
  (swap! trace-atom conj ["->bindings" tags])
  (mapcat
   (fn [tag]
     (when-let [binding (get-in ctx [::bindings tag])]
       (if (sequential? binding)
         (mapcat binding->tuple (distinct binding))
         (binding->tuple binding))))
   tags))

(defmethod ->form ::fn
  [_ ctx]
  (let [tags       [:fm/args :fm/ret :fm/rel :fm/trace :fm.anomaly/handler]
        ctx        (bind ctx tags)
        bindings   (bindings ctx tags)
        sym        (->form ::fn/symbol ctx)
        definition (->forms ::fn/definition ctx)
        metadata   (->form ::fn/metadata ctx)]
    `(let [~@bindings]
       (with-meta
         (fn ~sym ~@definition)
         ~metadata))))

(defmethod ->form ::fn/symbol
  [_ ctx]
  (symbol (name (->form ::fn/ident ctx))))

(defmethod ->forms ::fn/definition
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->forms tag ctx)))

(defmethod ->forms [::fn/definition ::fn/signature]
  [_ ctx]
  (let [argv (get-in ctx [::fn/conformed-definition ::fn/rest 1 ::fn/argv])
        argv (->form ::fn/normalized-argv argv)
        ctx  (assoc ctx ::fn/normalized-argv argv ::signature-index 0)
        body (->form ::fn/body ctx)]
    (list argv body)))

(defmethod ->forms [::fn/definition ::fn/signatures]
  [_ ctx]
  (let [signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        forms      (map-indexed
                    (fn [i signature]
                      (let [argv (get signature ::fn/argv)
                            argv (->form ::fn/normalized-argv argv)
                            ctx  (assoc ctx ::fn/normalized-argv argv ::signature-index i)
                            body (->form ::fn/body ctx)] ; TODO: when `::fn/body`
                        (list argv body)))
                    signatures)]
    forms))

(defmethod ->form ::fn/metadata
  [_ ctx]
  (let [metadata (get ctx ::metadata)]
    `(quote ~metadata)))

(defmethod ->form ::fn/normalized-argv
  [_ argv]
  (mapv
   (fn [arg]
     (cond
       (vector? arg) (if (some #{:as} arg) arg (conj arg :as (gensym 'arg)))
       (map? arg)    (update arg :as (fnil identity (gensym 'arg)))
       :else         arg))
   argv))

(defmethod ->form ::fn/body
  [_ ctx]
  (let [args    (->form ::fn/args ctx)
        body    (if (seq args)
                  (->form ::fn/args-binding ctx)
                  (->form ::fn/ret-binding ctx))
        handler (->form ::fn/handler ctx)
        ident   (->form ::fn/ident ctx)]
    `(try
       ~body
       (catch Throwable thrown#
         (~handler
          {:fm/ident          ~ident
           :fm.anomaly/ident  :fm.anomaly/thrown
           :fm.anomaly/args   ~args
           :fm.anomaly/thrown thrown#})))))

(defn ->trace?
  ([ctx]
   (swap! trace-atom conj "->trace?")
   (let [index (or (get ctx ::signature-index) 0)
         form  (->form :fm/trace ctx)]
     (cond
       (set? form) form
       (not form)  (constantly false)
       :else       (constantly true))))
  ([tag ctx]
   (swap! trace-atom conj ["->trace?" tag])
   (let [trace? (->trace? ctx)]
     (trace? tag))))

(defmulti  ->handler? (fn [ctx] (swap! trace-atom conj "->handler?") :default))
(defmethod ->handler? :default
  [ctx]
  (let [index    (or (get ctx ::signature-index) 0)
        handler? (get-in ctx [::metadata :fm.anomaly/handler? index])]
    handler?))

(defmulti  ->spec? (fn [tag _ctx] (swap! trace-atom conj ["->spec?" tag]) tag))
(defmethod ->spec? :fm/args
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (seq (get-in ctx [::metadata :fm/args index]))))

(defmethod ->form ::fn/args-binding
  [_ ctx]
  (let [ctx      (bind ctx [::fn/args])
        bindings (bindings ctx [::fn/args])
        trace    (->forms :fm.trace/args ctx)
        handler? (->handler? ctx)
        body     (cond
                   (not handler?)         (->form ::fn/received-anomaly? ctx)
                   (->spec? :fm/args ctx) (->form ::fn/args-anomaly? ctx)
                   :else                  (->form ::fn/ret-binding ctx))]
    `(let [~@bindings]
       ~@trace
       ~body)))

(defmethod ->forms :fm.trace/args
  [_ ctx]
  (when (->trace? :fm/args ctx)
    (let [trace (->form ::fn/trace ctx)
          ident (->form ::fn/ident ctx)
          args  (->form ::fn/args ctx)]
      [(list trace {:fm/ident ident :fm.trace/args args})]))) ; NOTE: `->forms`

(defmethod ->form ::fn/received-anomaly?
  [_ ctx]
  (let [args    (->form ::fn/args ctx)
        handler (->form ::fn/handler ctx)
        ident   (->form ::fn/ident ctx)
        body    (if (->spec? :fm/args ctx)
                  (->form ::fn/args-anomaly? ctx)
                  (->form ::fn/ret-binding ctx))]
    `(if (anomaly/anomalous? ~args)
       (~handler
        {:fm/ident         ~ident
         :fm.anomaly/ident :fm.anomaly/received
         :fm.anomaly/args  ~args}) ; NOTE: basically a worse stack trace
       ~body)))

(defn ->conform?
  ([ctx]
   (swap! trace-atom conj "->conform?")
   (let [index (or (get ctx ::signature-index) 0)
         form  (or (get-in ctx [::metadata :fm/conform index]) *conform*)]
     (cond
       (set? form) form
       (not form)  (constantly false)
       :else       (constantly true))))
  ([tag ctx]
   (swap! trace-atom conj ["->conform?" tag])
   (let [conform? (->conform? ctx)]
     (conform? tag))))

(defmethod ->form ::fn/args-anomaly?
  [_ ctx]
  (let [ctx       (bind ctx [::fn/conformed-args])
        bindings  (bindings ctx [::fn/conformed-args])
        trace     (->forms :fm.trace/conformed-args ctx)
        conformed (->form ::fn/conformed-args ctx)
        handler   (->form ::fn/handler ctx)
        ident     (->form ::fn/ident ctx)
        args-spec (->form ::fn/args-spec ctx)
        args      (->form ::fn/args ctx)
        body      (if (->conform? :fm/args ctx)
                    (->form ::fn/conformed-args-binding ctx)
                    (->form ::fn/ret-binding ctx))]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident         ~ident
           :fm.anomaly/ident :fm.anomaly/args
           ::s/explain-data  (s/explain-data ~args-spec ~args)})
         ~body))))

(defmethod ->forms :fm.trace/conformed-args
  [_ ctx]
  (when (and (->trace? :fm/args ctx) (->conform? :fm/args ctx))
    (let [trace (->form ::fn/trace ctx)
          ident (->form ::fn/ident ctx)
          args  (->form ::fn/conformed-args ctx)]
      [(list trace {:fm/ident ident :fm.trace/conformed-args args})])))

(defmethod ->form ::fn/conformed-args-binding
  [_ ctx]
  (let [argv      (get ctx ::fn/normalized-argv)
        conformed (->form ::fn/conformed-args ctx)
        body      (->form ::fn/ret-binding ctx)]
    `(let [~argv ~conformed]
       ~body)))

(defmethod ->spec? :fm/ret
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (some? (get-in ctx [::metadata :fm/ret index]))))

(defmethod ->spec? :fm/rel
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (some? (get-in ctx [::metadata :fm/rel index]))))

(defmethod ->form ::fn/ret-binding
  [_ ctx]
  (let [ctx      (bind ctx [::fn/ret])
        bindings (bindings ctx [::fn/ret])
        trace    (->forms :fm.trace/ret ctx)
        ret      (->form ::fn/ret ctx)
        handler  (->form ::fn/handler ctx)
        ident    (->form ::fn/ident ctx)
        args     (->form ::fn/args ctx)
        body     (cond
                   (->spec? :fm/ret ctx) (->form ::fn/ret-anomaly? ctx)
                   (->spec? :fm/rel ctx) (->form ::fn/rel-anomaly? ctx)
                   :else                 ret)]
    `(let [~@bindings]
       ~@trace
       (if (anomaly/anomalous? ~ret) ; TODO: `:fm.anomaly/deep-detect?`, `:fm/ignore`
         (~handler
          {:fm/ident         ~ident
           :fm.anomaly/ident :fm.anomaly/nested ; ALT: `:fm.anomaly/propagated`
           :fm.anomaly/args  ~args
           :fm.anomaly/ret   ~ret}) ; TODO: (if (map? ret) ret ,,,)
         ~body))))

(defmethod ->forms :fm.trace/ret
  [_ ctx]
  (when (->trace? :fm/ret ctx)
    (let [trace (->form ::fn/trace ctx)
          ident (->form ::fn/ident ctx)
          ret   (->form ::fn/ret ctx)]
      [(list trace {:fm/ident ident :fm.trace/ret ret})])))

(defmethod ->form ::fn/ret
  [_ ctx]
  (or
   (get-in ctx [::bindings ::fn/ret ::symbol])
   (let [index (or (get ctx ::signature-index) 0)
         body  (get-in ctx [::fn/conformed-definition ::fn/rest 1 index ::fn/body])]
     `(do ~@body)))) ; TODO: additional static analysis?

(defmethod ->form ::fn/ret-anomaly?
  [_ ctx]
  (let [ctx       (bind ctx [::fn/conformed-ret])
        bindings  (bindings ctx [::fn/conformed-ret])
        trace     (->forms :fm.trace/conformed-ret ctx)
        conformed (->form ::fn/conformed-ret ctx)
        handler   (->form ::fn/handler ctx)
        ident     (->form ::fn/ident ctx)
        args      (->form ::fn/args ctx)
        ret-spec  (->form ::fn/ret-spec ctx)
        ret       (->form ::fn/ret ctx)
        body      (cond
                    (->conform? :fm/ret ctx) (->form ::fn/conformed-ret-binding ctx)
                    (->spec? :fm/rel ctx)    (->form ::fn/rel-anomaly? ctx)
                    :else                    ret)]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident         ~ident
           :fm.anomaly/ident :fm.anomaly/ret
           :fm.anomaly/args  ~args
           ::s/explain-data  (s/explain-data ~ret-spec ~ret)})
         ~body))))

(defmethod ->forms :fm.trace/conformed-ret
  [_ ctx]
  (when (and (->trace? :fm/ret ctx) (->conform? :fm/ret ctx))
    (let [trace (->form ::fn/trace ctx)
          ident (->form ::fn/ident ctx)
          ret   (->form ::fn/conformed-ret ctx)]
      [(list trace {:fm/ident ident :fm.trace/conformed-ret ret})])))

(defmethod ->form ::fn/ret-spec
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (or
     (get-in ctx [::bindings :fm/ret index ::symbol])
     (let [ret     (get-in ctx [::metadata :fm/ret index])
           [tag _] (lib/conform-throw ::fn/arg ret)]
       (case tag
         ::fn/args (->form [::fn/args-spec ::fn/args] ret)
         ret)))))

(defmethod ->form ::fn/conformed-ret
  [_ ctx]
  (or
   (get-in ctx [::bindings ::fn/conformed-ret ::symbol])
   (let [ret-spec (->form ::fn/ret-spec ctx)
         ret      (->form ::fn/ret ctx)]
     `(s/conform ~ret-spec ~ret))))

(defmethod ->form ::fn/conformed-ret-binding
  [_ ctx]
  (let [ret       (->form ::fn/ret ctx)
        conformed (->form ::fn/conformed-ret ctx)
        body      (if (->spec? :fm/rel ctx)
                    (->form ::fn/rel-anomaly? ctx)
                    ret)]
    `(let [~ret ~conformed]
       ~body)))

(defmethod ->form ::fn/rel-anomaly?
  [_ ctx]
  (let [rel   (->form ::fn/rel ctx)
        args  (->form ::fn/args ctx)
        ret   (->form ::fn/ret ctx)
        ident (->form ::fn/ident ctx)]
    `(if (~rel {:args ~args :ret ~ret})
       ~ret
       {:fm/ident         ~ident
        :fm.anomaly/ident :fm.anomaly/rel
        ::s/explain-data  (s/explain-data ~rel {:args ~args :ret ~ret})})))

(defmethod ->form ::fn/rel
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (get-in ctx [::bindings :fm/rel index ::symbol])))

(defmethod ->form ::fn/handler
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (or
     (get-in ctx [::bindings :fm.anomaly/handler index ::symbol])
     *default-anomaly-handler-symbol*)))

(defmethod ->form ::fn/ident
  [_ ctx]
  (or
   (get-in ctx [::metadata :fm/ident])
   (->ident ctx)))

(defmethod ->form ::fn/args
  [_ ctx]
  (or
   (get-in ctx [::bindings ::fn/args ::symbol])
   (let [argv (get ctx ::fn/normalized-argv)
         form (if (some #{'&} argv)
                (let [xf   (comp (take-while (complement #{'&})) (map fn/arg->symbol))
                      args (into (vector) xf argv)
                      var  (fn/arg->symbol (last argv))]
                  `(into ~args ~var))
                (into (vector) (map fn/arg->symbol) argv))]
     form)))

(defmethod ->form ::fn/trace
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (or
     (get-in ctx [::bindings :fm/trace index ::symbol])
     *default-trace-fn-symbol*)))

(defmethod ->form ::fn/argv
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->form tag ctx)))

(defmethod ->form [::fn/argv ::fn/signature]
  [_ ctx]
  (get-in ctx [::fn/conformed-definition ::fn/rest 1 ::fn/argv]))

(defmethod ->form [::fn/argv ::fn/signatures]
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (get-in ctx [::fn/conformed-definition ::fn/rest 1 index ::fn/argv])))

  ;; TODO: zip `::fn/normalized-argv` tags, respect `:as`
  ;; TODO: detect regex-op, insert `s/spec`
(defmethod ->form ::fn/args-spec
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (or
     (get-in ctx [::bindings :fm/args index ::symbol])
     (let [args (get-in ctx [::metadata :fm/args index])
           form (->form [::fn/args-spec ::fn/args] args)]
       form))))

(defmethod ->form [::fn/args-spec ::fn/args]
  [_ args]
  (let [parts (partition-by (hash-set '&) args)
        args  (when (not= (count parts) 2) (->forms [::fn/args-spec ::fn/args]         (first parts))) ; NOTE: count `2` implies [& ,,,]
        var   (when (>    (count parts) 1) (->forms [::fn/args-spec ::fn/variadic-arg] (first (last parts))))]
    `(s/cat ~@args ~@var)))

(defmethod ->forms [::fn/args-spec ::fn/args]
  [_ args]
  (let [->form   (partial ->form [::fn/args-spec ::fn/arg])
        ->tagged (fn [i form]
                   (let [tag (keyword (str i))] ; TODO: zip `::fn/normalized-argv`
                     [tag form]))
        ->forms  (comp
                  (map ->form)
                  (map-indexed ->tagged)
                  (mapcat identity))
        forms    (sequence ->forms args)]
    forms))

(defmethod ->form [::fn/args-spec ::fn/arg]
  [_ arg]
  (let [[tag _] (lib/conform-throw ::fn/arg arg)]
    (case tag
      ::fn/args `(s/spec ~(->form [::fn/args-spec ::fn/args] arg))
      arg)))

(defmethod ->forms [::fn/args-spec ::fn/variadic-arg]
  [_ arg]
  (let [[tag _] (lib/conform-throw ::fn/variadic-arg arg)
        form    (case tag
                  ::fn/arg                 (->form [::fn/args-spec ::fn/variadic-arg ::fn/arg]              arg) ; ALT: [,,, tag], (conj ,,, tag)
                  ::fn/keyword-args-map    (->form [::fn/args-spec ::fn/variadic-arg ::fn/keyword-args-map] arg)
                  ::lib/sequence-spec-form arg)
        forms   (list :& form)]
    forms))

(defmethod ->form [::fn/args-spec ::fn/variadic-arg ::fn/arg]
  [_ arg]
  (let [[tag _] (lib/conform-throw ::fn/arg arg)]
    (case tag
      ::fn/args (->form [::fn/args-spec ::fn/variadic-arg ::fn/args] arg)
      `(s/* ~arg))))

(defmethod ->form [::fn/args-spec ::fn/variadic-arg ::fn/args]
  [_ args]
  (let [ctx {::args args ::index 0}]
    (->form [::fn/args-spec ::fn/variadic-arg ::fn/args ::recur] ctx)))

(defmethod ->form [::fn/args-spec ::fn/variadic-arg ::fn/args ::recur]
  [tag ctx]
  (let [i    (get ctx ::index)
        args (get ctx ::args)
        arg  (nth args i)] ; NOTE: seq
    (if (= arg '&)
      (->form tag (update ctx ::index inc))
      (let [index-tag (keyword (str i))
            arg       (->form [::fn/args-spec ::fn/arg] arg)
            last?     (or (= i (dec (count args))) (= (nth args (inc i)) '&))
            variadic? (some #{'&} args)
            rest      (if last?
                        (if variadic?
                          (->forms [::fn/args-spec ::fn/variadic-arg] (last args))
                          (list :rest `(s/* any?)))
                        (list :rest (->form tag (update ctx ::index inc))))]
        `(s/? (s/cat ~index-tag ~arg ~@rest))))))

(defmethod ->form [::fn/args-spec ::fn/variadic-arg ::fn/keyword-args-map]
  [_ arg]
  (let [->form   (fn [[k arg]]
                   (let [form (->form [::fn/args-spec ::fn/arg] arg)]
                     `(s/cat :k #{~k} :form ~form))) ; TODO: revisit tags
        ->tag    (comp keyword key)
        ->tagged (juxt ->tag ->form)
        forms    (mapcat ->tagged arg)]
    `(s/* (s/alt ~@forms))))

(defmethod ->form ::fn/conformed-args
  [_ ctx]
  (or
   (get-in ctx [::bindings ::fn/conformed-args ::symbol])
   (let [args-spec (->form ::fn/args-spec ctx)
         args      (->form ::fn/args ctx)]
     `(s/conform ~args-spec ~args))))

#_(defmethod ->form ::fn/var-symbol
    [_ ctx]
    (with-meta
      (->form ::fn/symbol ctx)
      (->form ::fn/var-metadata ctx)))

#_(defmethod ->form ::fn/var-metadata
    [_ ctx]
    (into
     (hash-map)
     (map (fn [k] [k (->form [::fn/var-metadata k] ctx)]))
     (hash-set :fm/doc :fm/arglists)))

#_(defmethod ->form ::sequent/body [_ ctx])

#_(defn fm
    [parameters]
    (let [params (assoc parameters ::ident ::fn)
          ctx    (->context params)
          form   (->form ::fn ctx)]
      form))

#_(defn defm
    [parameters]
    (let [params (assoc parameters ::ident ::fn)
          ctx    (->context params)
          sym    (->form ::fn/var-symbol ctx)
          form   (->form ::fn ctx)]
      `(def ~sym ~form)))

(comment

  ;; TODO: dynamic `:fm/args`
  (def args1 [int?])
  (fm ^{:fm/args args1} [x] (+ x 1))

  ;; TODO: warnings (log level?), `conformsplain`
  (fm ^{:fm/args [int?]} [x1 x2])

  ;;;
  )

(comment

  (def f1 (fn [a b c] (mapv inc [a b c])))
  (def f2 (fn [d e f] (mapv inc [d e f])))

  (defn comp1
    [& fs]
    (fn [& args] (reduce (fn [acc f] (apply f acc)) args fs)))

  ((comp1
    f1
    f2) 1 2 3)

  (def f3 (fn [[a b c]] (mapv inc [a b c])))
  (def f4 (fn [[d e f]] (mapv inc [d e f])))

  (defn comp2
    [& fs]
    (fn [& args]
      (let [args (if (and (= (count args) 1) (sequential? (first args))) (first args) args)]
        (reduce
         (fn [acc f] (f acc)) args fs))))

  ((comp2
    f3
    f4) {:a 1 :b 2 :c 3})

  ;;;
  )

(comment

  (fm/,,,
   {:fm/args            [[int?]
                         [int? & int?]]
    :fm/ret             [[int?]]
    :fm/trace           (fn [t] ,,,)
    :fm.anomaly/handler (fn [a] ,,,)}
   f1)

  (fm/,,,
   {:fm/f               f1
    :fm/args            [[int?]
                         [int? & int?]]
    :fm/ret             [[int?]]
    :fm/trace           (fn [t] ,,,)
    :fm.anomaly/handler (fn [a] ,,,)})

  ;;;
  )
