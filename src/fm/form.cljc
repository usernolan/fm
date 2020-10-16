(ns fm.form
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [fm.anomaly :as anomaly]
   [fm.form.fn :as fn]
   [fm.form.lib :as lib]))

(comment

  (def last-tag-atom (atom nil))

  ;;;
  )

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
     ::fm/signature  string?
     ::fm/signatures (s/coll-of string?)))

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
  #_{:fm/k1 [{::symbol ::form}]
     :fm/k2 [{::symbol ::form}]})

(s/def ::signature-index int?)

(s/def ::context
  (s/keys
   :opt
   [::ns
    ::definition
    ::metadata
    ::bindings
    ::signature-index
    ::fn/conformed-definition
    ::fn/metadata
    ::fn/outer-metadata
    ::fn/inner-metadatas]))

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

(defmulti  ->default-metadata-form (fn [_tag _ctx] :default))
(defmethod ->default-metadata-form :default
  [tag ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata tag])
        inners (map tag (get ctx ::fn/inner-metadatas))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (vec inners) ; NOTE: at least one
                 :else                (vec (cons outer inners)))]
    form))

(defmulti  ->metadata-form (fn [tag _ctx] (reset! last-tag-atom tag) tag))
(defmethod ->metadata-form :default
  [tag ctx]
  (->default-metadata-form tag ctx)) ; NOTE: keep all metadata

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
    (vector `(quote ~argv))))

(defmethod ->metadata-form [:fm/arglists ::fn/signatures]
  [_ ctx]
  (let [signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argvs      (map (comp (fn [argv] `(quote ~argv)) ::fn/argv) signatures)]
    (vec argvs)))

(defmethod ->metadata-form :fm/doc
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/doc ::fn/signature]
  [_ ctx]
  (let [doc (get-in ctx [::fn/metadata :fm/doc])
        _   (lib/conform-throw ::fn/doc doc)]
    doc))

(defmethod ->metadata-form [:fm/doc ::fn/signatures]
  [_ ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata :fm/doc])
        inners (map :fm/doc (get ctx ::fn/inner-metadatas))
        _      (map
                (partial lib/conform-throw ::fn/doc)
                (remove nil? (cons outer inners)))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (vec inners) ; NOTE: at least one
                 :else                (vec (cons outer inners)))]
    form))

(defmethod ->metadata-form :fm/args
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/args ::fn/signature]
  [_ ctx]
  (let [args (get-in ctx [::fn/metadata :fm/args])
        _    (lib/conform-throw ::fn/args args)]
    (vector args)))

(defmethod ->metadata-form [:fm/args ::fn/signatures]
  [_ ctx]
  (let [outer      (get-in ctx [::fn/outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::fn/inner-metadatas))
        _          (map
                    (partial lib/conform-throw ::fn/args)
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

(defmethod ->metadata-form :fm/ret
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/ret ::fn/signature]
  [_ ctx]
  (let [ret (get-in ctx [::fn/metadata :fm/ret])
        _   (lib/conform-throw ::fn/ret ret)]
    (vector ret)))

(defmethod ->metadata-form [:fm/ret ::fn/signatures]
  [_ ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata :fm/ret])
        inners (map :fm/ret (get ctx ::fn/inner-metadatas))
        _      (map
                (partial lib/conform-throw ::fn/ret)
                (remove nil? (cons outer inners)))
        rets   (map (fn [inner] (or inner outer)) inners)]
    (vec rets)))

(defmethod ->metadata-form :fm/rel
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/rel ::fn/signature]
  [_ ctx]
  (let [rel (get-in ctx [::fn/metadata :fm/rel])
        _   (lib/conform-throw ::fn/rel rel)]
    (vector rel)))

(defmethod ->metadata-form [:fm/rel ::fn/signatures]
  [_ ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata :fm/rel])
        inners (map :fm/rel (get ctx ::fn/inner-metadatas))
        _      (map
                (partial lib/conform-throw ::fn/rel)
                (remove nil? (cons outer inners)))
        rels   (map (fn [inner] (or inner outer)) inners)]
    (vec rels)))

(defmethod ->metadata-form :fm/trace
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/trace ::fn/signature]
  [_ ctx]
  (let [trace (get-in ctx [::fn/metadata :fm/trace])
        _     (lib/conform-throw ::fn/trace trace)]
    (vector trace)))

(defmethod ->metadata-form [:fm/trace ::fn/signatures]
  [_ ctx]
  (let [outer  (get-in ctx [::fn/outer-metadata :fm/trace])
        inners (map :fm/trace (get ctx ::fn/inner-metadatas))
        _      (map
                (partial lib/conform-throw ::fn/trace)
                (remove nil? (cons outer inners)))
        traces (map (fn [inner] (or inner outer)) inners)]
    (vec traces)))

(defmethod ->metadata-form :fm/conform
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm/conform ::fn/signature]
  [_ ctx]
  (let [conform (get-in ctx [::fn/metadata :fm/conform])
        _       (lib/conform-throw ::fn/conform conform)]
    (vector conform)))

(defmethod ->metadata-form [:fm/conform ::fn/signatures]
  [_ ctx]
  (let [outer    (get-in ctx [::fn/outer-metadata :fm/conform])
        inners   (map :fm/conform (get ctx ::fn/inner-metadatas))
        _        (map
                  (partial lib/conform-throw ::fn/conform) ; TODO: generalize
                  (remove nil? (cons outer inners)))
        conforms (map (fn [inner] (or inner outer)) inners)]
    (vec conforms)))

(defmethod ->metadata-form :fm.anomaly/handler
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm.anomaly/handler ::fn/signature]
  [_ ctx]
  (let [handler (get-in ctx [::fn/metadata :fm.anomaly/handler])
        _       (lib/conform-throw ::fn/handler handler)]
    (vector handler)))

(defmethod ->metadata-form [:fm.anomaly/handler ::fn/signatures]
  [_ ctx]
  (let [outer    (get-in ctx [::fn/outer-metadata :fm.anomaly/handler])
        inners   (map :fm.anomaly/handler (get ctx ::fn/inner-metadatas))
        _        (map
                  (partial lib/conform-throw ::fn/handler)
                  (remove nil? (cons outer inners)))
        handlers (map (fn [inner] (or inner outer)) inners)]
    (vec handlers)))

(defmethod ->metadata-form :fm.anomaly/handler?
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form tag ctx)))

(defmethod ->metadata-form [:fm.anomaly/handler? ::fn/signature]
  [_ ctx]
  (let [handler? (get-in ctx [::fn/metadata :fm.anomaly/handler?])
        _        (lib/conform-throw ::fn/handler? handler?)]
    (vector handler?)))

(defmethod ->metadata-form [:fm.anomaly/handler? ::fn/signatures]
  [_ ctx]
  (let [outer     (get-in ctx [::fn/outer-metadata :fm.anomaly/handler?])
        inners    (map :fm.anomaly/handler? (get ctx ::fn/inner-metadatas))
        _         (map
                   (partial lib/conform-throw ::fn/handler?)
                   (remove nil? (cons outer inners)))
        handler?s (map (fn [inner] (or inner outer)) inners)]
    (vec handler?s)))

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

(defmulti  ->metadata-bindings ::ident)
(defmethod ->metadata-bindings ::fn
  [ctx]
  (reset! last-tag-atom "->metadata-bindings")
  (let [metadata (get ctx ::metadata)
        xf       (map (fn [[k form]]
                        [k (if (sequential? form)
                             (::bindings
                              (reduce
                               (fn [acc form]
                                 (let [f       (fnil identity (gensym (name k)))
                                       acc     (update-in acc [::form=>symbol form] f)
                                       sym     (get-in acc [::form=>symbol form])
                                       binding {::symbol sym ::form form}
                                       acc     (update acc ::bindings conj binding)]
                                   acc))
                               {::form=>symbol {} ::bindings []}
                               form))
                             (vector {::symbol (gensym (name k)) ::form form}))]))
        bindings (into {} xf metadata)]
    bindings))

(defmulti  <<bindings ::ident)
(defmethod <<bindings ::fn
  [ctx]
  (assoc ctx ::bindings (->metadata-bindings ctx)))

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
   <<metadata
   <<bindings))

(defmulti  ->forms (fn [tag _ctx] (reset! last-tag-atom tag) tag))
(defmulti  ->form  (fn [tag _ctx] (reset! last-tag-atom tag) tag))
(defmethod ->form ::fn
  [_ ctx]
  (let [bindings   (->forms ::fn/bindings ctx)
        sym        (->form ::fn/symbol ctx)
        definition (->forms ::fn/definition ctx)
        metadata   (->form ::fn/metadata ctx)]
    `(let [~@bindings]
       (with-meta
         (fn ~sym ~@definition)
         ~metadata))))

(defmethod ->forms ::fn/bindings
  [_ ctx]
  (->>
   (get ctx ::bindings) ; TODO: warn on conflict
   (vals)
   (flatten)
   (distinct) ; TODO: distinct forms
   (mapcat (juxt ::symbol ::form))))

(defmethod ->form ::fn/symbol
  [_ ctx]
  (symbol (name (->form ::fn/ident ctx))))

(defmethod ->forms ::fn/definition
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->forms tag ctx)))

(defmethod ->forms [::fn/definition ::fn/signature]
  [_ ctx]
  (let [argv (get-in ctx [::fn/conformed-definition ::fn/rest 1 ::fn/argv]) ; ALT: `->form`
        ctx  (assoc ctx ::signature-index 0)
        body (->form ::fn/body ctx)]
    (list argv body)))

(defmethod ->forms [::fn/definition ::fn/signatures]
  [_ ctx]
  (let [signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        forms      (map-indexed
                    (fn [i signature]
                      (let [argv (get signature ::fn/argv) ; ALT: `->form`
                            ctx  (assoc ctx ::signature-index i)
                            body (->form ::fn/body ctx)]
                        (list argv body)))
                    signatures)]
    forms))

(defmethod ->form ::fn/metadata
  [_ ctx]
  (into
   (hash-map)
   (map
    (fn [[k forms]]
      [k (if (sequential? forms) ; NOTE: keep all metadata
           (into
            (vector)
            (map-indexed
             (fn [i form]
               (or
                (get-in ctx [::bindings k i ::symbol])
                form)))
            forms)
           (or
            (get-in ctx [::bindings k 0 ::symbol])
            forms))])) ; TODO: simplify
   (get ctx ::metadata)))

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
          {:fm/ident         ~ident
           :fm.anomaly/ident :fm.anomaly/thrown
           :fm.anomaly/args  ~args
           :fm.anomaly/data  thrown#})))))

(defn bind
  [ctx tags]
  (reset! last-tag-atom ["bind" tags])
  (reduce
   (fn [ctx tag]
     (let [binding {::symbol (gensym (name tag)) ::form (->form tag ctx)}]
       (update ctx ::bindings assoc tag binding)))
   ctx
   tags))

  ;; TODO: refactor `->form`?
(defn ->bindings
  [ctx tags]
  (reset! last-tag-atom ["->bindings" tags])
  (mapcat
   (fn [tag]
     [(get-in ctx [::bindings tag ::symbol])
      (get-in ctx [::bindings tag ::form])])
   tags))

(defn ->trace?
  [ctx]
  (reset! last-tag-atom "->trace?")
  (let [index (or (get ctx ::signature-index) 0)
        form  (or (get-in ctx [::bindings :fm/trace index ::form]) *trace*)]
    (cond
      (set? form) form
      (not form)  (constantly false)
      :else       (constantly true))))

  ;; TODO: refactor `->form`?
(defmulti  ->trace (fn [tag _ctx] (reset! last-tag-atom ["->trace" tag]) tag))
(defmethod ->trace :fm.trace/args
  [_ ctx]
  (let [trace? (->trace? ctx)]
    (when (trace? :fm/args)
      (let [trace (->form ::fn/trace ctx)
            ident (->form ::fn/ident ctx)
            args  (->form ::fn/args ctx)]
        [(list trace {:fm/ident ident :fm.trace/args args})])))) ; NOTE: splice

(defmulti  ->spec? (fn [tag _ctx] (reset! last-tag-atom ["->spec?" tag]) tag))
(defmethod ->spec? :fm/args
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)]
    (seq (get-in ctx [::metadata :fm/args index]))))

(defmethod ->form ::fn/args-binding
  [_ ctx]
  (let [ctx      (bind ctx [::fn/args])
        bindings (->bindings ctx [::fn/args])
        trace    (->trace :fm.trace/args ctx)
        args     (->form ::fn/args ctx)
        handler  (->form ::fn/handler ctx)
        ident    (->form ::fn/ident ctx)
        body     (if (->spec? :fm/args ctx)
                   (->form ::fn/args-anomaly? ctx)
                   (->form ::fn/ret-binding ctx))]
    `(let [~@bindings]
       ~@trace
       (if (anomaly/anomalous? ~args)
         (~handler
          {:fm/ident         ~ident
           :fm.anomaly/ident :fm.anomaly/received
           :fm.anomaly/args  ~args})
         ~body))))

(defn ->conform?
  [ctx]
  (reset! last-tag-atom "->conform?")
  (let [index (or (get ctx ::signature-index) 0)
        form  (or (get-in ctx [::bindings :fm/conform index ::form]) *conform*)]
    (cond
      (set? form) form
      (not form)  (constantly false)
      :else       (constantly true))))

(defmethod ->trace :fm.trace/conformed-args
  [_ ctx]
  (let [trace?   (->trace? ctx)
        conform? (->conform? ctx)]
    (when (and (trace? :fm/args) (conform? :fm/args))
      (let [trace (->form ::fn/trace ctx)
            ident (->form ::fn/ident ctx)
            args  (->form ::fn/conformed-args ctx)]
        [(list trace {:fm/ident ident :fm.trace/conformed-args args})]))))

(defmethod ->form ::fn/args-anomaly?
  [_ ctx]
  (let [ctx       (bind ctx [::fn/args-spec ::fn/conformed-args])
        bindings  (->bindings ctx [::fn/args-spec ::fn/conformed-args])
        trace     (->trace :fm.trace/conformed-args ctx)
        conformed (->form ::fn/conformed-args ctx)
        handler   (->form ::fn/handler ctx)
        ident     (->form ::fn/ident ctx)
        args-spec (->form ::fn/args-spec ctx)
        args      (->form ::fn/args ctx)
        conform?  (->conform? ctx)
        body      (if (conform? :fm/args)
                    (->form ::fn/conformed-args-binding ctx)
                    (->form ::fn/ret-binding ctx))]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident         ~ident
           :fm.anomaly/ident :fm.anomaly/args
           :fm.anomaly/args  ~args
           :fm.anomaly/data  (s/explain-data ~args-spec ~args)})
         ~body))))

(defmethod ->form ::fn/conformed-args-binding
  [_ ctx]
  (let [argv      (->form ::fn/argv ctx)
        conformed (->form ::fn/conformed-args ctx)
        body      (->form ::fn/ret-binding ctx)]
    `(let [~argv ~conformed]
       ~body)))

(defmethod ->form ::fn/ret-binding
  [_ ctx]
  ::fn/ret-binding
  #_(let []
      `(let [~@bindings]
         ~@trace
         (if (anomaly/anomalous? ~ret)
           (~handler ~ret)
           ~body))))

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
   (let [argv (->form ::fn/argv ctx)
         form (if (some #{'&} argv)
                (->form [::fn/args ::fn/variadic] argv)
                argv)]
     form)))

(defmethod ->form [::fn/args ::fn/variadic]
  [_ argv]
  (let [xf   (comp (take-while (complement #{'&})) (map fn/arg->symbol))
        args (into (vector) xf argv)
        var  (fn/arg->symbol (last argv))] ; TODO: `normalize` argv
    `(into ~args ~var)))

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

  ;; TODO: detect regex-op, insert `s/spec`
  ;; TODO: delegate to meta->spec-obj fn, `s/form`; (s/form (fn/meta->args-spec-obj {:fm/args args :fm/arglist argv}))
  ;; TODO: respect `:as`
  ;; TODO: `loop`?
(defmethod ->form ::fn/args-spec
  [_ ctx]
  (let [index (or (get ctx ::signature-index) 0)
        args  (get-in ctx [::metadata :fm/args index])
        form  (->form [::fn/args-spec ::fn/args] args)]
    form))

(defmethod ->form [::fn/args-spec ::fn/args]
  [_ args]
  (let [parts (partition-by (hash-set '&) args)
        args  (when (not= (count parts) 2) (->forms [::fn/args-spec ::fn/args]         (first parts))) ; NOTE: count `2` implies [& ,,,]
        var   (when (>    (count parts) 1) (->forms [::fn/args-spec ::fn/variadic-arg] (last parts)))]
    `(s/cat ~@args ~@var)))

(defmethod ->forms [::fn/args-spec ::fn/args]
  [_ args]
  (let [->form   (partial ->form [::fn/args-spec ::fn/arg])
        ->tagged (fn [i form]
                   (let [tag (keyword (str i))] ; TODO: zip argv
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
                  ::fn/arg                 (->form [::fn/args-spec ::fn/variadic-arg ::fn/arg]              arg) ; ALT: [,,, tag], (conj t tag)
                  ::fn/keyword-args-map    (->form [::fn/args-spec ::fn/variadic-arg ::fn/keyword-args-map] arg)
                  ::lib/sequence-spec-form arg)
        forms   (list :& form)] ; ALT: `::fn/variadic`
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
        arg  (get args i)]
    (if (= arg '&)
      (->form tag (update ctx ::index inc))
      (let [index-tag (keyword (str i))
            arg       (->form [::fn/args-spec ::fn/arg] arg)
            rest?     (< i (dec (count args)))
            variadic? (some #{'&} args)
            rest      (if rest?
                        (list :rest (->form tag (update ctx ::index inc)))
                        (if variadic?
                          (list :& (->form [::fn/args-spec ::fn/variadic-arg] arg))
                          (list :rest `(s/* any?))))]
        `(s/? (s/cat ~index-tag ~arg ~@rest))))))

(defmethod ->form [::fn/args-spec ::fn/variadic-arg ::fn/keyword-args-map]
  [_ arg]
  (let [->form   (fn [[k arg]]
                   (let [form (->form [::fn/args-spec ::fn/arg] arg)]
                     `(s/cat :k #{~k} ::fn/arg ~form)))
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
