(ns fm.form.fn
  (:require
   [clojure.core.specs.alpha :as core.specs]
   [clojure.spec.alpha :as s]
   [fm.anomaly :as anomaly]
   [fm.form :as form]
   [fm.lib :as lib]))


   ;;;
   ;;; NOTE: form specs
   ;;;


  ;; NOTE: empty-body `fm.core/fn` is invalid at current
(s/def ::signature
  (s/cat
   ::argv ::form/specv
   ::retv (s/? ::form/specv)
   ::body (s/+ any?))) ; ALT: `s/*`

(s/def ::signatures
  (s/+
   (s/spec ::signature)))

(s/def ::definition
  (s/cat
   :fm.definition/simple-symbol (s/? simple-symbol?)
   :fm.definition/rest
   (s/alt
    :fm.signature/singular ::signature
    :fm.signature/plural ::signatures)))


   ;;;
   ;;; NOTE: metadata specs
   ;;;


(s/def ::metadata-fn-form
  (s/or ::form/bound-fn ::form/bound-fn
        ::form/fn-form ::form/fn-form))

(s/def ::metadata-specv-form
  (s/or :fm.context/nominal (s/tuple vector?)
        :fm.context/positional vector?))

(s/def ::metadata-spec-form
  (s/or ::s/registry-keyword ::s/registry-keyword
        ::form/spec-form ::form/spec-form
        ::metadata-fn-form ::metadata-fn-form
        ::metadata-specv-form ::metadata-specv-form))


   ;;;
   ;;; NOTE: internal concepts, shapes
   ;;;


(comment

  (s/def ::defaults
    (s/keys
     :opt
     [:fm/throw!
      :fm/trace
      :fm/trace-fn
      :fm/handler]))

    ;; NOTE: follows from `::definition` etc.
  (s/def ::conformed-definition
    (s/keys
     :opt [:fm.definition/simple-symbol]
     :req [:fm.definition/rest]))

  (s/def ::conformed-signature
    (s/keys
     :req [::argv ::body]
     :opt [::retv]))

  (s/def ::signature-index int?)

  (s/def ::signature-contexts
    (s/+
     (s/keys :req [:fm/args :fm/ret])))

  (s/def ::metadata-pred-form
    (s/or
     :boolean boolean?
     :set set?))

  (s/def :fm/ident
    qualified-keyword?)

  (s/def :fm/arglists
    (s/* ::core.specs/param-list))

  (s/def :fm/doc
    (s/or
     ::definition string?
     ::signature-indexed (s/* (s/alt :fm/doc :fm/doc :nil nil?))))

  (s/def :fm/throw!
    (s/or
     ::definition boolean?
     ::signature-indexed (s/* (s/alt :fm/throw! :fm/throw! :nil nil?))))

  (s/def :fm/args
    (s/or
     ::definition ::metadata-spec-form
     ::signature-indexed (s/* (s/alt :fm/args :fm/args :nil nil?))))

  (s/def :fm/ret
    (s/or
     ::definition ::metadata-spec-form
     ::signature-indexed (s/* (s/alt :fm/ret :fm/ret :nil nil?))))

  (s/def :fm/rel
    (s/or
     ::definition ::metadata-spec-form
     ::signature-indexed (s/* (s/alt :fm/rel :fm/rel :nil nil?))))

  (s/def :fm/trace
    (s/or
     ::definition (s/or ::metadata-pred-form ::metadata-pred-form
                        ::metadata-fn-form ::metadata-fn-form)
     ::signature-indexed (s/* (s/alt :fm/trace :fm/trace :nil nil?))))

  (s/def :fm/conform
    (s/or
     ::definition ::metadata-pred-form
     ::signature-indexed (s/* (s/alt :fm/conform :fm/conform :nil nil?))))

  (s/def :fm/handler
    (s/or
     ::definition ::metadata-fn-form
     ::signature-indexed (s/* (s/alt :fm/handler :fm/handler :nil nil?))))

  (s/def :fm/handler?
    (s/or
     ::definition boolean?
     ::signature-indexed (s/* (s/alt :fm/handler? :fm/handler? :nil nil?))))

  (s/def :fm/sequent
    (s/or
     ::definition (s/keys :req [:fm.sequent/ident :fm.sequent/combine])
     ::signature-indexed (s/* (s/alt :fm/sequent :fm/sequent :nil nil?)))) ; TODO: revisit; full definition

  (s/def ::metadata
    (s/keys
     :opt
     [:fm/ident
      :fm/arglists
      :fm/doc
      :fm/throw!
      :fm/args
      :fm/ret
      :fm/rel
      :fm/trace
      :fm/conform
      :fm/handler
      :fm/handler?
      :fm/sequent
      #_:fm/ignore
      #_:fm/memoize]))

  (s/def ::outer-metadata (s/or ::metadata ::metadata :nil nil?))
  (s/def ::inner-metadatas (s/* ::outer-metadata)) ; ALT: inner-metadata

  (s/def ::ctx
    (s/merge
     ::form/ctx
     (s/keys
      :opt
      [::defaults
       ::conformed-definition
       ::signature-index
       ::signature-contexts
       ::metadata
       ::outer-metadata
       ::inner-metadatas])))

  ;;;
  )


   ;;;
   ;;; NOTE: multimethods, hierarchies
   ;;;


(def form-hierarchy
  (swap!
   form/form-hierarchy-atom
   (fn [hierarchy]
     (->
      hierarchy
      (derive :fm/ident                      :fm.metadata/default)
      (derive :fm/arglists                   :fm.metadata/signature-indexed)
      #_(derive :fm/doc                        :default)
      (derive :fm/throw!                     :fm.metadata/signature-indexed)
      (derive :fm/args                       :fm.metadata/signature-indexed)
      (derive :fm/ret                        :fm.metadata/signature-indexed)
      (derive :fm/rel                        :fm.metadata/signature-indexed)
      (derive :fm/trace                      :fm.metadata/signature-indexed)
      (derive :fm/conform                    :fm.metadata/signature-indexed)
      (derive :fm/handler                    :fm.metadata/signature-indexed)
      (derive :fm/handler?                   :fm.metadata/signature-indexed)
      (derive :fm/sequent                    :fm.metadata/signature-indexed)
      #_(derive :fm/ignore                     :fm.metadata/signature-indexed)
      #_(derive :fm/memoize                    :fm.metadata/signature-indexed)
      (derive :fm.metadata/signature-indexed :fm.metadata/default)
      (derive :fm/args                       :fm/spec)
      (derive :fm/ret                        :fm/spec)
      (derive :fm/rel                        :fm/spec)
      (derive :fm.sequent/conse              :fm/sequent)
      (derive :fm.sequent/nonse              :fm/sequent)
      (derive :fm.sequent/merge              :fm/sequent)
      (derive :fm.sequent/iso                :fm/sequent)
      (derive :fm.sequent/combine            :fm.metadata/signature-indexed)
      (derive ::args                         :fm.binding/default)
      (derive ::ret                          :fm.binding/default)
      (derive ::conformed-args               :fm.binding/default)
      (derive ::conformed-ret                :fm.binding/default)))))


   ;;;
   ;;; NOTE: ctx helpers
   ;;;


(defn conformed-definition [ctx]
  (swap! form/trace-atom conj ["conformed-definition" ctx])
  (try
    (lib/conform-throw! ::definition (get ctx ::definition))
    (catch Throwable t
      (let [msg  (str "Are all positional spec params in the registry?\n\n"
                      (ex-message t))
            data (merge ctx (ex-data t))]
        (form/invalid-definition! msg data)))))

(defn signature-tag [ctx]
  (swap! form/trace-atom conj ["signature-tag" ctx])
  (get-in ctx [::conformed-definition :fm.definition/rest 0]))

(defn count-signatures [ctx]
  (let [conformed (get-in ctx [::conformed-definition :fm.definition/rest 1])]
    (if (sequential? conformed) (count conformed) 1)))

(defn signature-contexts [ctx]
  (swap! form/trace-atom conj ["contexts" ctx])
  (let [conformed (get-in ctx [::conformed-definition :fm.definition/rest 1])
        f         (fn [conformed-signature]
                    (hash-map
                     :fm/args (get-in conformed-signature [::argv 0])
                     :fm/ret (get-in conformed-signature [::retv 0])))
        contexts  (if (sequential? conformed)
                    (into (vector) (map f) conformed)
                    (vector (f conformed)))]
    contexts))

(defn signature-index [ctx]
  (get ctx ::signature-index 0))

(defn conformed-signature [ctx]
  (swap! form/trace-atom conj ["conformed-signature" ctx])
  (let [conformed (get-in ctx [::conformed-definition :fm.definition/rest 1])
        signature (if (sequential? conformed)
                    (nth conformed (signature-index ctx))
                    conformed)]
    signature))

(defn metadata [ctx]
  (swap! form/trace-atom conj ["metadata" ctx])
  (let [tag [(get ctx ::form/ident) (signature-tag ctx)]]
    (form/->metadata ctx tag)))

(defn handler? [ctx]
  (swap! form/trace-atom conj "handler?")
  (get-in ctx [::metadata :fm/handler? (signature-index ctx)]))

(defn metadata? [ctx tag]
  (swap! form/trace-atom conj ["metadata?" tag])
  (let [form      (get-in ctx [::metadata tag (signature-index ctx)])
        metadata? (if (sequential? form)
                    (seq form)
                    (some? form))]
    metadata?))

(defn trace? [ctx tag]
  (swap! form/trace-atom conj ["trace?" tag])
  (let [form   (or (get-in ctx [::metadata :fm/trace (signature-index ctx)])
                   (get-in ctx [::defaults :fm/trace]))
        trace? (cond
                 (set? form) form
                 (not form)  (constantly false)
                 :else       (constantly true))]
    (trace? tag)))

(defn conform? [ctx tag]
  (swap! form/trace-atom conj ["conform?" tag])
  (let [form     (or (get-in ctx [::metadata :fm/conform (signature-index ctx)])
                     (get-in ctx [::defaults :fm/conform]))
        conform? (cond
                   (set? form) form
                   (not form)  (constantly false)
                   :else       (constantly true))]
    (conform? tag)))

(defn throw? [ctx]
  (swap! form/trace-atom conj "throw?")
  (let [form   (or (get-in ctx [::metadata :fm/throw! (signature-index ctx)])
                   (get-in ctx [::defaults :fm/throw!]))
        throw? (boolean form)]
    throw?))

(defn sequent? [ctx]
  (swap! form/trace-atom conj "sequent?")
  (let [form     (or (get-in ctx [::metadata :fm/sequent (signature-index ctx)])
                     (get-in ctx [::defaults :fm/sequent]))
        sequent? (boolean form)]
    sequent?))


   ;;;
   ;;; NOTE: `form/->metadata` helpers
   ;;;


(def spec-param-tags
  (hash-set
   :fm.context/nominal
   ::s/registry-keyword
   ::form/positional-binding-map))


   ;;;
   ;;; NOTE: `form/->metadata` implementations
   ;;;


  ;; TODO: include `retv`
(defmethod form/->metadata [::form/fn :fm.signature/singular]
  [ctx _]
  (let [definition (get ctx ::definition)
        sym        (when (symbol? (first definition)) (first definition))
        signature  (if sym (rest definition) definition)
        ctx        (assoc ctx ::signature signature ::signature-index 0)
        outer      (merge
                    (meta sym)
                    (meta (first signature))
                    (form/->metadata ctx ::signature-index))
        ctx        (assoc ctx ::outer-metadata outer)
        tags       (into (hash-set :fm/ident :fm/arglists) (keys outer))
        metadata   (into (hash-map) (map (partial form/->metadata ctx)) tags)]
    metadata))

(defmethod form/->metadata [::form/fn :fm.signature/plural]
  [ctx _]
  (let [definition (get ctx ::definition)
        sym        (when (symbol? (first definition)) (first definition))
        signatures (if sym (rest definition) definition)
        outer      (merge
                    (meta sym)
                    (meta (first signatures)))
        inners     (map-indexed
                    (fn [i sig]
                      (let [ctx (assoc ctx ::signature sig ::signature-index i)]
                        (merge
                         (meta (first sig))
                         (form/->metadata ctx ::signature-index))))
                    signatures)
        ctx        (assoc ctx ::outer-metadata outer ::inner-metadatas inners)
        tags       (into (hash-set :fm/ident :fm/arglists) (mapcat keys) (cons outer inners))
        metadata   (into (hash-map) (map (partial form/->metadata ctx)) tags)]
    metadata))

(defmethod form/->metadata ::signature-index
  [ctx _]
  (let [sig  (conformed-signature ctx)
        argv (get sig ::argv)
        retv (get sig ::retv)]
    (merge
     (when (lib/deep-some spec-param-tags argv)
       (let [tag  [::metadata :fm/args ::form/conformed-specv]
             form (form/->form argv tag)]
         (hash-map :fm/args form)))
     (when (lib/deep-some spec-param-tags retv)
       (let [tag  [::metadata :fm/ret ::form/conformed-specv]
             form (form/->form retv tag)]
         (hash-map :fm/ret form))))))

  ;;
  ;; TODO: special case `:fm/args`; infer `:fm/handler?`
  ;; e.g. (when (lib/deep-some #{:fm/anomaly} args) ,,,)
  ;;

(defmethod form/->metadata :fm/sequent
  [ctx tag]
  (let [form (form/->form ctx [::form/metadata ::form/fn tag])]
    (hash-map :fm/sequent form))) ; NOTE: normalize tag


   ;;;
   ;;; NOTE: `form/->binding` implementations
   ;;;


(defmethod form/->binding ::args
  [ctx _]
  (let [context (get-in ctx [::conformed-signature ::argv 0])]
    (form/->binding ctx [::args context])))

(defmethod form/->binding [::args :fm.context/positional]
  [ctx [form-tag _]]
  (let [sym  (or (get-in ctx [::conformed-signature ::argv 1 :as-form :as-sym])
                 (gensym (name form-tag)))
        form (form/->form ctx [::form/binding ::form/fn form-tag])]
    (hash-map ::form/destructure sym ::form/form form)))

(defmethod form/->binding [::args :fm.context/nominal]
  [ctx _]
  (let [sym (get-in ctx [::normalized-arglist 0 :as])]
    (hash-map ::form/destructure sym)))

(defmethod form/->binding ::argxs
  [ctx _]
  (hash-map ::form/destructure (gensym 'argxs)))

(defmethod form/->binding :fm.metadata/signature-indexed
  [ctx tag]
  (into
   (vector)
   (map
    (fn [i]
      (let [ctx (assoc ctx ::signature-index i)]
        (form/default-binding ctx tag))))
   (range
    (count-signatures ctx))))


   ;;;
   ;;; NOTE: `form/->form`, `form/->forms` helpers
   ;;;


(defn dispatch-kv
  ([f [k v]]
   (swap! form/trace-atom conj ["dispatch-kv" [f k v]])
   (f v k))
  ([f t [k v]]
   (swap! form/trace-atom conj ["dispatch-kv" [f t k v]])
   (let [tag (lib/positional-combine [t k])]
     (f v tag))))

(def dispatch-form-kv
  (partial dispatch-kv form/->form))

(def arglist-metadata
  (partial dispatch-form-kv [::metadata :fm/arglist]))

(def args-metadata
  (partial dispatch-form-kv [::metadata :fm/args]))

(defn normalized-binding-tuple [[k [tag conformed]]]
  (swap! form/trace-atom conj ["normalized-binding-tuple" [k [tag conformed]]])
  (let [norm (case tag
               :local-symbol    conformed
               :map-destructure (update conformed :as (fnil identity (symbol (name k))))
               :seq-destructure (update conformed :as-form (fnil identity {:as :as :as-sym (symbol (name k))})))]
    (vector tag norm)))

(def normalized-arglist-metadata
  (comp
   arglist-metadata
   normalized-binding-tuple))

(defn zipv-args
  "Zip `:fm/args` to match a target `shape`, e.g. an inner `arglist`"
  [args shape]
  (swap! form/trace-atom conj ["zipv-args" [args shape]])
  (lib/zipvf vector? (fn [_ a] a) shape args))

(defn arg-symbol [arg]
  (swap! form/trace-atom conj ["arg-symbol" [arg]])
  (cond
    (vector? arg) (when (some #{:as} arg) (last arg))
    (map? arg)    (:as arg)
    :else         arg))


   ;;;
   ;;; NOTE: `form/->form` implementations
   ;;;


  ;; INTRO: everything in this file is demand-driven by this method
(defmethod form/->form ::form/fn
  [ctx tag]
  (let [ctx        (assoc ctx ::form/ident tag)
        ctx        (assoc ctx ::conformed-definition (conformed-definition ctx))
        ctx        (assoc ctx ::signature-contexts (signature-contexts ctx))
        ctx        (assoc ctx ::metadata (metadata ctx))
        tags       (vector :fm/args :fm/ret :fm/rel :fm/trace :fm/handler
                           :fm.sequent/combine)
        ctx        (form/bind ctx tags)
        bindings   (form/bindings ctx tags)
        sym        (form/->form ctx ::simple-symbol)
        definition (form/->forms ctx ::definition)
        metadata   (form/->form ctx ::metadata)
        body       `(with-meta (fn ~sym ~@definition) ~metadata)
        form       (if (seq bindings) `(let [~@bindings] ~body) body)] ; ALT: split
    form))

(defmethod form/->form ::simple-symbol
  [ctx _]
  (symbol (name (form/->form ctx [::form/metadata ::form/fn :fm/ident]))))

(defmethod form/->form ::metadata
  [ctx _]
  (let [metadata (get ctx ::metadata)]
    `(quote ~metadata)))

(defmethod form/->form ::body
  [ctx _]
  (let [arglist (get-in ctx [::metadata :fm/arglists (signature-index ctx)])]
    (cond
      (not (throw? ctx)) (form/->form ctx ::try)
      (seq arglist)      (form/->form ctx [::bind ::args])
      :else              (form/->form ctx [::bind ::ret]))))

(defmethod form/->form ::try
  [ctx _]
  (let [args    (form/->form ctx [::form/binding ::form/fn ::args])
        args?   (or (symbol? args) (and (sequential? args) (seq args)))
        body    (if args?
                  (form/->form ctx [::bind ::args])
                  (form/->form ctx [::bind ::ret]))
        handler (form/->form ctx [::form/binding ::form/fn :fm/handler])
        ident   (form/->form ctx [::form/metadata ::form/fn :fm/ident])]
    `(try
       ~body
       (catch Throwable thrown#
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/thrown
           ::anomaly/args   ~args
           ::anomaly/thrown thrown#})))))

(defmethod form/->form ::args
  [ctx _]
  (let [context (get-in ctx [::conformed-signature ::argv 0])]
    (form/->form ctx [::args context])))

(defmethod form/->form [::args :fm.context/positional]
  [ctx _]
  (let [arglist (get ctx ::normalized-arglist)
        form    (if (some #{'&} arglist)
                  (let [xf   (comp (take-while (complement #{'&})) (map arg-symbol))
                        args (into (vector) xf arglist)
                        var  (arg-symbol (last arglist))]
                    `(into ~args ~var))
                  (into (vector) (map arg-symbol) arglist))]
    form))

(defmethod form/->form [::args :fm.context/nominal]
  [ctx _]
  (get-in ctx [::normalized-arglist 0 :as]))

(defmethod form/->form [::bind ::args]
  [ctx _]
  (let [ctx      (form/bind ctx [::args])
        bindings (form/bindings ctx [::args])
        trace    (when (trace? ctx :fm/args) (form/->forms ctx [::trace ::args]))
        body     (cond
                   (not (handler? ctx))     (form/->form ctx [::validate ::received])
                   (metadata? ctx :fm/args) (form/->form ctx [::validate ::args])
                   :else                    (form/->form ctx [::bind ::ret]))
        form     (cond
                   (seq bindings) `(let [~@bindings] ~@trace ~body)
                   (some? trace)  `(do ~@trace ~body)
                   :else          body)]
    form))

  ;; TODO: `:fm.anomaly/deep-detect`
(defmethod form/->form [::validate ::received]
  [ctx _]
  (let [ident   (form/->form ctx [::form/metadata ::form/fn :fm/ident])
        handler (form/->form ctx [::form/binding ::form/fn :fm/handler])
        args    (form/->form ctx [::form/binding ::form/fn ::args])
        body    (if (metadata? ctx :fm/args)
                  (form/->form ctx [::validate ::args])
                  (form/->form ctx [::bind ::ret]))]
    `(if (some anomaly/anomaly? ~args)
       (~handler
        {:fm/ident       ~ident
         ::anomaly/ident ::anomaly/received
         ::anomaly/args  ~args})
       ~body)))

(defmethod form/->form [::validate ::args]
  [ctx _]
  (let [ctx       (form/bind ctx [::conformed-args])
        bindings  (form/bindings ctx [::conformed-args])
        trace     (when (and (trace? ctx :fm/args) (conform? ctx :fm/args))
                    (form/->forms ctx [::trace ::conformed-args]))
        ident     (form/->form ctx [::form/metadata ::form/fn :fm/ident])
        handler   (form/->form ctx [::form/binding ::form/fn :fm/handler])
        args-spec (form/->form ctx [::form/binding ::form/fn :fm/args])
        args      (form/->form ctx [::form/binding ::form/fn ::args])
        conformed (form/->form ctx [::form/binding ::form/fn ::conformed-args])
        body      (if (conform? ctx :fm/args)
                    (form/->form ctx [::bind ::conformed-args])
                    (form/->form ctx [::bind ::ret]))]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/args
           ::s/explain-data (s/explain-data ~args-spec ~args)})
         ~body))))

(defmethod form/->form ::conformed-args
  [ctx _]
  (let [spec (form/->form ctx [::form/binding ::form/fn :fm/args])
        args (form/->form ctx [::form/binding ::form/fn ::args])]
    `(s/conform ~spec ~args)))

(defmethod form/->form [::bind ::conformed-args]
  [ctx _]
  (let [args      (form/->form ctx [::form/binding ::form/fn ::args])
        conformed (form/->form ctx [::form/binding ::form/fn ::conformed-args])
        body      (form/->form ctx [::bind ::ret])]
    `(let [~args ~conformed]
       ~body)))

  ;; TODO: `:fm.anomaly/deep-detect`, `:fm/ignore`
(defmethod form/->form [::bind ::ret]
  [ctx _]
  (let [ctx      (form/bind ctx [::ret])
        bindings (form/bindings ctx [::ret])
        trace    (when (trace? ctx :fm/ret) (form/->forms ctx [::trace ::ret]))
        handler  (form/->form ctx [::form/binding ::form/fn :fm/handler])
        ret      (form/->form ctx [::form/binding ::form/fn ::ret])
        body     (cond
                   (metadata? ctx :fm/ret) (form/->form ctx [::validate ::ret])
                   (metadata? ctx :fm/rel) (form/->form ctx [::validate ::rel])
                   :else                   ret)]
    `(let [~@bindings]
       ~@trace
       (if (anomaly/anomaly? ~ret)
         (~handler ~ret)
         ~body))))

(defmethod form/->form ::ret
  [ctx _]
  (let [body (get-in ctx [::conformed-signature ::body])
        form (if (lib/singular? body) (first body) `(do ~@body))]
    form))

(defmethod form/->form [::validate ::ret]
  [ctx _]
  (let [ctx       (form/bind ctx [::conformed-ret])
        bindings  (form/bindings ctx [::conformed-ret])
        trace     (when (and (trace? ctx :fm/ret) (conform? ctx :fm/ret))
                    (form/->forms ctx [::trace ::conformed-ret]))
        ident     (form/->form ctx [::form/metadata ::form/fn :fm/ident])
        handler   (form/->form ctx [::form/binding ::form/fn :fm/handler])
        ret-spec  (form/->form ctx [::form/binding ::form/fn :fm/ret])
        args      (form/->form ctx [::form/binding ::form/fn ::args])
        ret       (form/->form ctx [::form/binding ::form/fn ::ret])
        conformed (form/->form ctx [::form/binding ::form/fn ::conformed-ret])
        body      (cond
                    (conform? ctx :fm/ret)  (form/->form ctx [::bind ::conformed-ret])
                    (metadata? ctx :fm/rel) (form/->form ctx [::validate ::rel])
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

(defmethod form/->form ::conformed-ret
  [ctx _]
  (let [spec (form/->form ctx [::form/binding ::form/fn :fm/ret])
        ret  (form/->form ctx [::form/binding ::form/fn ::ret])]
    `(s/conform ~spec ~ret)))

(defmethod form/->form [::bind ::conformed-ret]
  [ctx _]
  (let [ret       (form/->form ctx [::form/bidning ::form/fn ::ret])
        conformed (form/->form ctx [::form/bidning ::form/fn ::conformed-ret])
        body      (if (metadata? ctx :fm/rel)
                    (form/->form ctx [::validate ::rel])
                    ret)]
    `(let [~ret ~conformed]
       ~body)))

(defmethod form/->form [::validate ::rel]
  [ctx _]
  (let [ident (form/->form ctx [::form/metadata ::form/fn :fm/ident])]
        rel   (form/->form ctx [::form/binding ::form/fn :fm/rel])
        args  (form/->form ctx [::form/binding ::form/fn ::args])
        ret   (form/->form ctx [::form/binding ::form/fn ::ret])
    `(if (s/valid? ~rel {:args ~args :ret ~ret})
       ~ret
       {:fm/ident        ~ident
        ::anomaly/ident  ::anomaly/rel
        ::s/explain-data (s/explain-data ~rel {:args ~args :ret ~ret})})))


  ;;
  ;; NOTE: `form/->metadata` form implementations
  ;;


(defmethod form/->form [::form/metadata ::form/fn :fm/ident]
  [ctx _]
  (or
   (get-in ctx [::metadata :fm/ident])
   (keyword
    (str (get ctx ::form/ns))
    (name
     (or
      (get-in ctx [::conformed-definition :fm.definition/simple-symbol])
      (gensym (name (get ctx ::form/ident)))))))) ; TODO: sequent

(defmethod form/->form [::form/metadata ::form/fn :fm.metadata/default]
  [ctx tag]
  (form/->form ctx (conj tag (signature-tag ctx))))

  ;; TODO: quote arglists individually for direct metadata `eval`
(defmethod form/->form [::form/metadata ::form/fn :fm/arglists
                        :fm.signature/singular]
  [ctx _]
  (let [argv    (get-in ctx [::conformed-definition :fm.definition/rest 1 ::argv])
        arglist (form/->form argv [::metadata :fm/arglist ::form/conformed-specv])]
    (vector arglist)))

(defmethod form/->form [::form/metadata ::form/fn :fm/arglists
                        :fm.signature/plural]
  [ctx _]
  (let [argvs    (map ::argv (get-in ctx [::conformed-definition :fm.definition/rest 1]))
        arglists (map (fn [argv] (form/->form argv [::metadata :fm/arglist ::form/conformed-specv])) argvs)]
    (vec arglists)))

(defmethod form/->form [::form/metadata ::form/fn :fm/args
                        :fm.signature/plural]
  [ctx _]
  (let [outer      (get-in ctx [::outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::inner-metadatas))
        _          (when (s/get-spec :fm/args)
                     (doseq [form (remove nil? (cons outer inners))]
                       (lib/conform-throw! :fm/args form)))
        signatures (get-in ctx [::conformed-definition :fm.definition/rest 1])
        ->arglist  (fn [argv] (form/->form argv [::metadata :fm/arglist ::form/conformed-specv]))
        arglists   (map (comp ->arglist ::argv) signatures)
        args       (map-indexed
                    (fn [i inner]
                      (let [arglist (nth arglists i)]
                        (when-let [args (or inner outer)]
                          (zipv-args args arglist))))
                    inners)]
    (vec args))) ; TODO: qualify, `s/explicate`?

(defmethod form/->form [::form/metadata ::form/fn :fm.metadata/default
                        :fm.signature/singular]
  [ctx [_ _ meta-tag _]]
  (let [metadata (get-in ctx [::outer-metadata meta-tag])
        _        (when (s/get-spec meta-tag)
                   (lib/conform-throw! meta-tag metadata))]
    (vector metadata))) ; NOTE: vector for signature indexing

(defmethod form/->form [::form/metadata ::form/fn :fm.metadata/signature-indexed
                        :fm.signature/plural]
  [ctx [_ _ meta-tag _]]
  (let [outer  (get-in ctx [::outer-metadata meta-tag])
        inners (map meta-tag (get ctx ::inner-metadatas))
        _      (when (s/get-spec meta-tag)
                 (doseq [data (remove nil? (cons outer inners))]
                   (lib/conform-throw! meta-tag data)))
        metas  (map (fn fallback [inner] (or inner outer)) inners)]
    (vec metas)))

  ;; NOTE: default for unrecognized tags and `:fm/doc`
(defmethod form/->form [::form/metadata ::form/fn :default]
  [ctx tag]
  (form/->form ctx (conj tag (signature-tag ctx))))

(defmethod form/->form [::form/metadata ::form/fn :default
                        :fm.signature/singular]
  [ctx _]
  (let [tag  (get ctx ::form/tag)
        form (get-in ctx [::outer-metadata tag])
        _    (when (s/get-spec tag)
               (lib/conform-throw! tag form))]
    form))

(defmethod form/->form [::form/metadata ::form/fn :default
                        :fm.signature/plural]
  [ctx _]
  (let [tag    (get ctx ::form/tag)
        outer  (get-in ctx [::outer-metadata tag])
        inners (map tag (get ctx ::inner-metadatas))
        _      (when (s/get-spec tag)
                 (doseq [data (remove nil? (cons outer inners))]
                   (lib/conform-throw! tag data)))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (vec inners) ; NOTE: at least one
                 :else                (vec (cons outer inners)))]
    form))


  ;;
  ;; NOTE: `form/->binding` form implementations
  ;;


(defmethod form/->form [::form/binding ::form/fn :fm/spec]
  [ctx [_ _ spec-tag]]
  (or (get-in ctx [::form/bindings spec-tag (signature-index ctx) ::form/destructure])
      (form/->form ctx [::s/form spec-tag])))

(defmethod form/->form [::form/binding ::form/fn :fm/trace]
  [ctx _]
  (let [index (signature-index ctx)]
    (or (get-in ctx [::form/bindings :fm/trace index ::form/destructure])
        (when-let [form (or (get-in ctx [::metadata :fm/trace index])
                            (get-in ctx [::defaults :fm/trace]))]
          (let [pred? (some-fn true? set?)
                fn?   (some-fn form/fn-form? form/bound-fn?)]
            (cond
              (pred? form)  (get-in ctx [::defaults :fm/trace-fn])
              (fn? form)    form
              (false? form) nil ; ALT: `form`, `false`
              :else         `(partial ~(get-in ctx [::defaults :fm/trace-fn]) ~form)))))))

  ;; ALT: anomaly map literal when `nil`
(defmethod form/->form [::form/binding ::form/fn :fm/handler]
  [ctx _]
  (let [index (signature-index ctx)]
    (or (get-in ctx [::form/bindings :fm/handler index ::form/destructure])
        (let [form (get-in ctx [::metadata :fm/handler index])
              fn?  (some-fn form/fn-form? form/bound-fn?)]
          (cond
            (fn? form) form
            :else      (get-in ctx [::defaults :fm/handler])))))) ; TODO: nil handler case

(defmethod form/->form [::form/binding ::form/fn :fm.binding/default]
  [ctx [_ _ form-tag]]
  (or (get-in ctx [::form/bindings form-tag ::form/destructure])
      (form/->form ctx form-tag)))


  ;;
  ;; NOTE: nested form implementations, ctx-free form transformations
  ;;


(defmethod form/->form ::normalized-arglist
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

(defmethod form/->form [::metadata :fm/arglist ::form/conformed-specv]
  [[context data] _]
  (let [tag       [::metadata :fm/arglist ::form/conformed-param-list context]
        conformed (case context
                    :fm.context/positional data
                    :fm.context/nominal    (first data))]
    (form/->form conformed tag)))

(defmethod form/->form [::metadata :fm/arglist ::form/conformed-param-list
                        :fm.context/positional]
  [conformed tag]
  (let [params     (when-let [params (get conformed :params)]
                     (form/->forms params (conj tag :params)))
        var-params (when-let [var-params (get conformed :var-params)]
                     (form/->forms var-params (conj tag :var-params)))]
    `[~@params ~@var-params]))

(defmethod form/->form [::metadata :fm/arglist ::form/conformed-param-list
                        :fm.context/nominal]
  [conformed tag]
  (let [map-destructure (if-let [params (get conformed :params)]
                          (form/->form params (conj tag :params))
                          (hash-map))
        map-destructure (if-let [as-sym (get-in conformed [:as-form :as-sym])]
                          (assoc map-destructure :as as-sym)
                          map-destructure)]
    `[~map-destructure]))

(defmethod form/->form [::metadata :fm/arglist ::form/conformed-param-list
                        :fm.context/nominal :params]
  [params _]
  (into (hash-map) (map arglist-metadata) params))

(defmethod form/->form [::metadata :fm/arglist ::s/registry-keyword]
  [k _]
  (symbol (name k)))

(defmethod form/->form [::metadata :fm/arglist ::form/positional-binding-map]
  [m _]
  (normalized-arglist-metadata (first m)))

(defmethod form/->form [::metadata :fm/arglist ::core.specs/binding-form]
  [conformed _]
  (arglist-metadata conformed))

(defmethod form/->form [::metadata :fm/arglist :keyword]
  [k _]
  (let [sym (symbol (name k))]
    (hash-map sym k)))

(defmethod form/->form [::metadata :fm/arglist ::form/nominal-binding-map]
  [m _]
  (into (hash-map) (map (juxt normalized-arglist-metadata first)) m))

(defmethod form/->form [::metadata :fm/arglist :local-symbol] [sym _] sym)
(defmethod form/->form [::metadata :fm/arglist :map-destructure] [m _] m)
(defmethod form/->form [::metadata :fm/arglist :seq-destructure]
  [conformed _]
  (let [forms      (map arglist-metadata (get conformed :forms))
        rest-forms (when-let [rest-form (get-in conformed [:rest-forms :form])]
                     (let [form (arglist-metadata rest-form)]
                       `(& ~form)))
        as-forms   (when-let [as-sym (get-in conformed [:as-form :as-sym])]
                     `(:as ~as-sym))] ; ALT: `form/->forms`
    `[~@forms ~@rest-forms ~@as-forms]))

(defmethod form/->form [::metadata :fm/spec :keyword] [k _] k)
(defmethod form/->form [::metadata :fm/spec ::s/registry-keyword] [k _] k)
(defmethod form/->form [::metadata :fm/spec ::core.specs/binding-form] [_ _] `any?) ; TODO: additional inference
(defmethod form/->form [::metadata :fm/spec ::form/positional-binding-map] [m _] (first (keys m)))
(defmethod form/->form [::metadata :fm/spec ::form/nominal-binding-map] [m _] (keys m))
(defmethod form/->form [::metadata :fm/spec ::form/conformed-specv]
  [[context data] [form-tag spec-tag _]]
  (let [tag       [form-tag spec-tag ::form/conformed-param-list context]
        conformed (case context
                    :fm.context/positional data
                    :fm.context/nominal    (first data))]
    (form/->form conformed tag)))

(defmethod form/->form [::metadata :fm/spec ::form/conformed-param-list
                        :fm.context/positional]
  [conformed [form-tag spec-tag data-tag _]]
  (form/->form conformed [form-tag spec-tag data-tag]))

(defmethod form/->form [::metadata :fm/spec ::form/conformed-param-list
                        :fm.context/nominal]
  [conformed [form-tag spec-tag data-tag _]]
  (vector ; NOTE: retain outer `[]` ; ALT: `hash-set` "keyset"
   (vec
    (sort
     (form/->form conformed [form-tag spec-tag data-tag])))))

(defmethod form/->form [::metadata :fm/spec ::form/conformed-param-list]
  [conformed tag]
  (let [params   (when-let [params (get conformed :params)]
                   (form/->forms params (conj tag :params)))
        var-form (when-let [var-params (get conformed :var-params)]
                   (form/->forms var-params (conj tag :var-params)))]
    `[~@params ~@var-form]))


  ;;
  ;; NOTE: spec forms
  ;;


(defmethod form/->form [::s/form :fm/spec]
  [ctx [_ spec-tag :as tag]]
  (when-let [data (get-in ctx [::metadata spec-tag (signature-index ctx)])]
    (let [[t _] (lib/conform-throw! ::metadata-spec-form data)
          ctx   (if (= t ::metadata-specv-form) (assoc ctx spec-tag data) data)]
      (form/->form ctx (conj tag t)))))

(defmethod form/->form [::s/form :fm/spec ::s/registry-keyword] [k _] k)
(defmethod form/->form [::s/form :fm/spec ::form/spec-form] [form _] form)
(defmethod form/->form [::s/form :fm/spec ::metadata-fn-form] [f _] f) ; NOTE: may require `s/spec` in spec2
(defmethod form/->form [::s/form :fm/spec ::metadata-specv-form]
  [ctx [_ spec-tag _ :as tag]]
  (let [specv       (get ctx spec-tag)
        [context _] (lib/conform-throw! ::metadata-specv-form specv)
        ctx         (case context
                      :fm.context/positional specv
                      :fm.context/nominal    ctx)]
    (form/->form ctx (conj tag context))))

(defmethod form/->form [::s/form :fm/spec ::metadata-specv-form
                        :fm.context/positional]
  [specv tag]
  (let [parts      (partition-by (hash-set '&) specv)
        var?       (> (count parts) 1)
        var-only?  (= (count parts) 2)
        params     (when (not var-only?) (form/->forms (first parts) (conj tag :params)))
        var-params (when var? (form/->forms (first (last parts)) (conj tag :var-params)))]
    `(s/cat ~@params ~@var-params)))

(defmethod form/->form [::s/form :fm/spec ::metadata-specv-form
                        :fm.context/nominal]
  [ctx [_ spec-tag _ _]]
  (let [default-ns     (str (get ctx ::form/ns))
        ks             (first (get ctx spec-tag))
        {req    true
         req-un false} (group-by qualified-keyword? ks)
        req-forms      (when (seq req) `(:req ~req))
        req-un-forms   (when (seq req-un)
                         (let [xf     (comp (partial keyword default-ns) name)
                               req-un (into (vector) (map xf) req-un)]
                           `(:req-un ~req-un)))]
    `(s/keys ~@req-forms ~@req-un-forms)))


   ;;;
   ;;; NOTE: `form/->forms` implementations
   ;;;


(defmethod form/->forms ::definition
  [ctx tag]
  (let [tag (if (sequent? ctx)
              [tag :fm/sequent]
              [tag (signature-tag ctx)])]
    (form/->forms ctx tag)))

  ;; ALT: optional `::fn-body`; NOTE: currently `s/+`
  ;; ALT: upstream (normalized-arglists ctx)
(defmethod form/->forms [::definition :fm.signature/singular]
  [ctx _]
  (let [sig     (get-in ctx [::conformed-definition :fm.definition/rest 1])
        arglist (get-in ctx [::metadata :fm/arglists 0])
        norm    (form/->form arglist ::normalized-arglist)
        ctx     (assoc ctx ::conformed-signature sig ::normalized-arglist norm)
        body    (form/->form ctx ::body)
        forms   (list norm body)]
    forms))

(defmethod form/->forms [::definition :fm.signature/plural]
  [ctx _]
  (let [arglists (get-in ctx [::metadata :fm/arglists])
        forms    (map-indexed
                  (fn [i arglist]
                    (let [sig  (get-in ctx [::conformed-definition :fm.definition/rest 1 i])
                          norm (form/->form arglist ::normalized-arglist)
                          ctx  (assoc
                                ctx
                                ::conformed-signature sig
                                ::normalized-arglist norm
                                ::signature-index i)
                          body (form/->form ctx ::body)]
                      (list norm body)))
                  arglists)]
    forms))

(defmethod form/->forms [::metadata :fm/arglist ::form/conformed-param-list
                         :fm.context/positional :params]
  [params _]
  (map arglist-metadata params))

(defmethod form/->forms [::metadata :fm/arglist ::form/conformed-param-list
                         :fm.context/positional :var-params]
  [var-params _]
  (let [var-form (arglist-metadata (get var-params :var-form))]
    `(& ~var-form)))

(defmethod form/->forms [::metadata :fm/spec ::form/conformed-param-list
                         :params]
  [params _]
  (mapcat (comp lib/ensure-sequential args-metadata) params))

(defmethod form/->forms [::metadata :fm/spec ::form/conformed-param-list
                         :var-params]
  [var-params _]
  (let [var-form (args-metadata (get var-params :var-form))]
    `(& ~var-form)))

(defmethod form/->forms [::s/form :fm/spec ::metadata-specv-form
                         :fm.context/positional :params]
  [params _]
  (into
   (vector)
   (comp
    (map-indexed (fn [i param] (vector (keyword (str i)) param)))
    (mapcat identity))
   params))

(defmethod form/->forms [::s/form :fm/spec ::metadata-specv-form
                         :fm.context/positional :var-params]
  [var-params _]
  (if (= var-params `any?)
    `(:& (s/* ~var-params))
    `(:& ~var-params)))

(defmethod form/->forms [::trace :fm.binding/default]
  [ctx [_ form-tag]]
  (let [ident (form/->form ctx [::form/metadata ::form/fn :fm/ident])
        trace (form/->form ctx [::form/binding ::form/fn :fm/trace])
        form  (form/->form ctx [::form/binding ::form/fn form-tag])
        data  (hash-map :fm/ident ident form-tag form)
        form  (list trace data)
        forms (list form)]
    forms))


   ;;;
   ;;; NOTE: WIP
   ;;;


(comment

  (defmethod form/->form [::form/metadata ::form/fn :fm/sequent]
    [ctx tag]
    (form/->form ctx (conj tag (signature-tag ctx))))

  (defmethod form/->form [::form/metadata ::form/fn :fm/sequent
                          :fm.signature/singular]
    [ctx [_ _ sequent-tag _ :as tag]]
    (let [data (get-in ctx [::outer-metadata sequent-tag])
          form (if (true? data)
                 (let [tag (assoc tag (dec (count tag)) :fm.signature/index)]
                   (form/->form ctx tag))
                 data)
          _    (when (s/get-spec sequent-tag)
                 (lib/conform-throw! sequent-tag form))]
      (vector form)))

  (defmethod form/->form [::form/metadata ::form/fn :fm/sequent
                          :fm.signature/plural]
    [ctx tag]
    (let [outer  (form/finda (get ctx ::outer-metadata) :fm/sequent)
          inners (map
                  (fn [metadata] (form/finda metadata :fm/sequent))
                  (get ctx ::inner-metadatas))
          forms  (map-indexed
                  (fn fallback [i inner]
                    (let [[k data] (or inner outer)]
                      (if (true? data)
                        (let [ctx (assoc ctx ::signature-index i)
                              tag (assoc tag (dec (count tag)) :fm.signature/index)]
                          (form/->form ctx tag))
                        data)))
                  inners)
          _      (doseq [form forms]
                   (let [tag (get form :fm.sequent/ident)]
                     (when (s/get-spec tag)
                       (lib/conform-throw! tag form))))]
      (vec forms)))

    ;; TODO: warn unspecified mismatch ret context in `:fm.sequent/merge`
    ;; TODO: `normalized-sequent-ident`
  (defmethod form/->form [::form/metadata ::form/fn :fm/sequent
                          :fm.signature/index]
    [ctx [sequent-tag _]]
    (let [index   (signature-index ctx)
          context (get-in ctx [::signature-contexts index :fm/args])
          combine (case context
                    :fm.context/positional `lib/positional-combine
                    :fm.context/nominal    `lib/nominal-combine)]
      (hash-map
       :fm.sequent/ident sequent-tag
       :fm.sequent/combine combine)))

  (defmethod form/->form [::form/binding ::form/fn :fm.sequent/combine]
    [ctx _]
    (let [index (signature-index ctx)]
      (or (get-in ctx [::form/bindings :fm.sequent/combine index ::form/destructure])
          (get-in ctx [::metadata :fm/sequent index :fm.sequent/combine])))) ; TODO: validate

  (defmethod form/->forms [::definition :fm/sequent]
    [ctx _]
    (let [ctx   (form/bind ctx [::argxs])
          argxs (form/->form ctx ::argxs)
          body  ::body #_(form/->form ctx ::body)]
      `([& ~argxs] ~body)))

  (defmethod form/->form ::argxs
    [ctx _]
    (get-in ctx [::form/bindings ::argxs ::form/destructure]))


   ;;;
   ;;; NOTE: sketch
   ;;;


  (defmethod form/->form [::body :fm/sequent]
    [ctx _]
    (let [throw? (every? boolean (get-in ctx [::metadata :fm/throw!]))]
      (if throw?
        (form/->form ctx [::bind ::args])
        (form/->form ctx ::try))))

  (defmethod form/->form [::try :fm/sequent]
    [ctx _]
    (let [body     (form/->form ctx [::bind ::args])
          handlers (form/->form ctx ::handlers)
          throws?  (form/->form ctx ::throws?)
          ident    (form/->form ctx [::form/metadata ::form/fn :fm/ident])
          argxs    (form/->form ctx ::argxs)]
      `(try
         ~body
         (catch Throwable thrown#
           (let [data#  (ex-data thrown#)
                 index# (get data# ::signature-index)]
             (if (~throws? index#)
               (throw (get data# ::anomaly/thrown thrown#))
               (let [handler# (get ~handlers index# ~(first handlers))]
                 (handler#
                  (if (anomaly/anomaly? data#)
                    data#
                    {:fm/ident        ~ident
                     ::anomaly/ident  ::anomaly/thrown
                     ::anomaly/args   (vec ~argxs)
                     ::anomaly/thrown thrown#})))))))))

  (defmethod form/->form [::bind ::args :fm/sequent]
    [ctx _]
    (let [ctx      (bind ctx [::args])
          bindings (bindings ctx [::args])
          trace    (when (trace? ctx :fm/args)
                     (form/->forms ctx [::trace ::args]))
          body     (form/->form ctx [::bind ::conformed-args])
          form     (cond
                     (seq bindings) `(let [~@bindings] ~@trace ~body)
                     (some? trace)  `(do ~@trace ~body)
                     :else          body)]
      form))

  (defmethod form/->form ::handlers
    [ctx _]
    (vec (get-in ctx [::metadata :fm/handler])))

  (defmethod form/->form ::throws?
    [ctx _]
    (into
     (vector)
     (map boolean)
     (get-in ctx [::metadata :fm/throw!])))

  (defmethod form/->form [::args :fm/sequent]
    [ctx _]
    (or (get-in ctx [::form/bindings ::args ::form/destructure])
        (let [contexts (set (map :fm/args (get ctx ::signature-contexts)))
              _        (when-not (lib/singular? contexts)
                         (invalid-definition!
                          "Sequent args context must be signature total." ctx))
              combine  (form/->form ctx ::combine)
              argxs    (form/->form ctx ::argxs)]
          `(~combine ~argxs))))

  (defmethod form/->form ::combine
    [ctx _]
    (let [index (signature-index ctx)]
      (get-in ctx [::form/bindings :fm.sequent/combine index ::form/destructure])))

  (defmethod form/->form [::bind ::conformed-args :fm/sequent]
    [ctx _])

  (defmethod form/->form [:fm.sequent/iso :fm.signature/index]
    [ctx [sequent-tag _]]
    (let [combine (hash-map :fm/args args-combine :fm/ret ret-combine)]
      (hash-map
       :fm.sequent/ident sequent-tag
       :fm.sequent/combine combine)))

  #_`([& argxs]
      (try
        (let [args (into (emptyctxstruct) maybectxxf argxs)]
          (if (seq args)
            (if (anomalous? args)
              ::anomaly/received
              (let [conformed (conform args)]
                (if (invalid? conformed)
                  ::anomaly/args
                  (let [args conformed])
                  (let [ret 'body
                        ret (case (first conformed)
                              :0 'body0
                              :1 'body1)]
                    (if (anomalous? ret)
                      ::anomaly/ret
                      (let [ret]))))))))
        (catch)))

  #_(defmethod form/->form ::var-symbol
      [_ ctx]
      (with-meta
        (form/->form ::fn-symbol ctx)
        (form/->form ::fn-var-metadata ctx)))

  #_(defmethod form/->form ::var-metadata
      [_ ctx]
      (into
       (hash-map)
       (map (fn [k] [k (form/->form [::fn-var-metadata k] ctx)]))
       (hash-set :fm/doc :fm/arglists)))

  #_(defmethod form/->form ::defn
      [parameters]
      (let [params (assoc parameters ::ident ::fn)
            ctx    (->context params)
            sym    (form/->form ::fn-var-symbol ctx)
            form   (form/->form ::fn ctx)]
        `(def ~sym ~form)))

  ;;;
  )


   ;;;
   ;;; NOTE: WIP
   ;;;


(defmethod form/->form ::try
  [ctx _]
  (if (sequent? ctx)
    (form/->form ctx [::try :fm/sequent])
    (form/->form ctx [::try ::form/default])))

(defmethod form/->form [::try :fm/sequent]
  [ctx _]
  (let [body     (->form ctx [::bind ::args])
        handlers (->form ctx ::handlers)
        throws?  (->form ctx ::throws?)
        ident    (->form ctx [::form/metadata ::form/fn :fm/ident])
        argxs    (->form ctx ::argxs)]
    `(try
       ~body
       (catch Throwable thrown#
         (let [data#  (ex-data thrown#)
               index# (get data# ::signature-index)]
           (if (~throws? index#)
             (throw (get data# ::anomaly/thrown thrown#))
             (let [handler# (get ~handlers index# ~(first handlers))]
               (handler#
                (if (anomaly/anomaly? data#)
                  data#
                  {:fm/ident        ~ident
                   ::anomaly/ident  ::anomaly/thrown
                   ::anomaly/args   (vec ~argxs)
                   ::anomaly/thrown thrown#})))))))))

(defmethod form/->form [::try ::form/default]
  [ctx _]
  (let [args    (->form ctx ::args)
        args?   (or (symbol? args) (and (sequential? args) (seq args)))
        body    (if args?
                  (->form ctx [::bind ::args])
                  (->form ctx [::bind ::ret]))
        handler (->form ctx [::form/binding ::form/fn :fm/handler])
        ident   (->form ctx [::form/metadata ::form/fn :fm/ident])]
    `(try
       ~body
       (catch Throwable thrown#
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/thrown
           ::anomaly/args   ~args
           ::anomaly/thrown thrown#})))))
