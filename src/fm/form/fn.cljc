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
  (s/or :list list?
        :symbol symbol?))

(s/def ::metadata-specv-form
  (s/or :fm.context/nominal (s/tuple vector?)
        :fm.context/positional vector?))

(s/def ::metadata-spec-form
  (s/or ::s/registry-keyword ::s/registry-keyword
        ::metadata-fn-form ::metadata-fn-form
        ::metadata-specv-form ::metadata-specv-form))


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
      (derive :fm.metadata/signature-indexed :fm.metadata/default)
      (derive :fm.metadata/var-metadata      :fm.metadata/default)
      (derive :fm/arglists                   :fm.metadata/signature-indexed)
      (derive :fm/throw!                     :fm.metadata/signature-indexed)
      (derive :fm/args                       :fm.metadata/signature-indexed)
      (derive :fm/ret                        :fm.metadata/signature-indexed)
      (derive :fm/rel                        :fm.metadata/signature-indexed)
      (derive :fm/trace                      :fm.metadata/signature-indexed)
      (derive :fm/conform                    :fm.metadata/signature-indexed)
      (derive :fm/handler                    :fm.metadata/signature-indexed)
      (derive :fm/sequent                    :fm.metadata/signature-indexed)
      (derive :fm.sequent/combine            :fm.metadata/signature-indexed)
      #_(derive :fm/ignore                     :fm.metadata/signature-indexed)
      #_(derive :fm/memoize                    :fm.metadata/signature-indexed)
      (derive :fm/arglists                   :fm.metadata/var-metadata)
      (derive :fm/doc                        :fm.metadata/var-metadata)
      (derive :fm/args                       :fm/spec)
      (derive :fm/ret                        :fm/spec)
      (derive :fm/rel                        :fm/spec)
      (derive :fm.sequent/conse              :fm/sequent)
      (derive :fm.sequent/nonse              :fm/sequent)
      (derive :fm.sequent/merge              :fm/sequent)
      (derive ::args                         :fm.form.fn/argx)
      (derive ::combined-argxs               :fm.form.fn/argx)
      (derive ::argxs                        :fm.binding/implicitly-bound)
      (derive ::thrown                       :fm.binding/implicitly-bound)
      (derive ::destructured-argx            :fm.binding/default)
      (derive ::ret                          :fm.binding/default)
      (derive ::ex-data                      :fm.binding/default)
      (derive ::ex-signature-index           :fm.binding/default)
      (derive ::ex-handler                   :fm.binding/default)
      (derive :fm.form.fn/argx               :fm.binding/default)
      (derive :fm.binding/implicitly-bound   :fm.binding/default)
      (derive :fm.throw/none                 :fm.throw/default)
      (derive :fm.throw/plural               :fm.throw/default)
      (derive :fm.handler/singular           :fm.handler/default)
      (derive :fm.handler/plural             :fm.handler/default)))))


   ;;;
   ;;; NOTE: ctx helpers
   ;;;


(defn conformed-definition [ctx]
  (try
    (lib/conform-throw! ::definition (get ctx ::definition))
    (catch #?(:clj Throwable :cljs :default) t
      (let [msg  (str "Are all positional spec params in the registry?\n\n"
                      (ex-message t))
            data (merge ctx (ex-data t))]
        (form/invalid-definition! msg data)))))

(defn signature-tag [ctx]
  (get-in ctx [::conformed-definition :fm.definition/rest 0]))

(defn conformed-signatures [ctx]
  (let [conformed  (get-in ctx [::conformed-definition :fm.definition/rest 1])
        signatures (vec (lib/ensure-sequential conformed))]
    signatures))

(def count-signatures
  (comp count conformed-signatures))

(defn signature-contexts [ctx]
  (let [signatures (conformed-signatures ctx)
        f          (fn [signature]
                     (hash-map
                      :fm/args (get-in signature [::argv 0])
                      :fm/ret (get-in signature [::retv 0])))
        contexts   (into (vector) (map f) signatures)]
    contexts))

(defn metadata [ctx]
  (let [tag [(get ctx ::form/ident) (signature-tag ctx)]]
    (form/metadata ctx tag)))

(defn normalized-arglist [arglist]
  (into
   (vector)
   (map
    (fn [arg]
      (cond
        (vector? arg) (if (some #{:as} arg) arg (conj arg :as (gensym 'arg)))
        (map? arg)    (update arg :as (fnil identity (gensym 'arg)))
        :else         arg)))
   arglist))

(defn normalized-arglists [ctx]
  (into
   (vector)
   (map normalized-arglist)
   (get-in ctx [::metadata :fm/arglists])))

(defn arglist-symbols [ctx]
  (let [contexts (map :fm/args (get ctx ::signature-contexts))
        ixf      (fn [i context]
                   (or (case context
                         :fm.context/positional (get-in ctx [::conformed-signatures i ::argv 1 :as-form :as-sym])
                         :fm.context/nominal    (get-in ctx [::normalized-arglists i 0 :as])
                         nil)
                       (gensym 'args)))
        syms     (into (vector) (map-indexed ixf) contexts)]
    syms))

(defn ensure-ctx [ctx]
  (let [ctx (if (get ctx ::conformed-definition) ctx (assoc ctx ::conformed-definition (conformed-definition ctx)))
        ctx (if (get ctx ::conformed-signatures) ctx (assoc ctx ::conformed-signatures (conformed-signatures ctx)))
        ctx (if (get ctx ::signature-contexts) ctx (assoc ctx ::signature-contexts (signature-contexts ctx)))
        ctx (if (get ctx ::metadata) ctx (assoc ctx ::metadata (metadata ctx)))
        ctx (if (get ctx ::normalized-arglists) ctx (assoc ctx ::normalized-arglists (normalized-arglists ctx)))
        ctx (if (get ctx ::arglist-symbols) ctx (assoc ctx ::arglist-symbols (arglist-symbols ctx)))]
    ctx))

(defn signature-index [ctx]
  (get ctx ::signature-index 0)) ; ALT: `rand`

(defn metadata? [ctx tag]
  (let [form      (get-in ctx [::metadata tag (signature-index ctx)])
        metadata? (if (sequential? form)
                    (seq form)
                    (some? form))]
    metadata?))

(defn trace? [ctx tag]
  (let [form   (or (get-in ctx [::metadata :fm/trace (signature-index ctx)])
                   (get-in ctx [::defaults :fm/trace]))
        trace? (cond
                 (set? form) form
                 (not form)  (constantly false)
                 :else       (constantly true))]
    (trace? tag))) ; TODO: revisit defaults

(defn conform? [ctx tag]
  (let [form     (or (get-in ctx [::metadata :fm/conform (signature-index ctx)])
                     (get-in ctx [::defaults :fm/conform]))
        conform? (cond
                   (set? form) form
                   (not form)  (constantly false)
                   :else       (constantly true))]
    (conform? tag)))

(defn throw? [ctx]
  (let [form   (or (get-in ctx [::metadata :fm/throw! (signature-index ctx)])
                   (get-in ctx [::defaults :fm/throw!]))
        throw? (boolean form)]
    throw?))

(defn sequent? [ctx]
  (or (get-in ctx [::metadata :fm/sequent])
      (get-in ctx [::defaults :fm/sequent])))

(defn sequent-tag [ctx]
  (get-in ctx [::metadata :fm/sequent (signature-index ctx)
               :fm.sequent/ident]))


   ;;;
   ;;; NOTE: [[Intro]] everything in this file is demand-driven by these methods
   ;;;


(defmethod form/form ::form/fn
  [ctx tag]
  (let [ctx        (ensure-ctx (assoc ctx ::form/ident tag))
        tags       (vector :fm/args :fm/ret :fm/rel :fm/trace :fm/handler
                           :fm.sequent/combine)
        ctx        (form/bind ctx tags)
        bindings   (form/bindings ctx tags)
        sym        (form/form ctx ::simple-symbol)
        definition (form/forms ctx ::definition)
        metadata   (form/form ctx ::metadata)
        body       `(with-meta (fn ~sym ~@definition) ~metadata)
        form       (if (seq bindings) `(let [~@bindings] ~body) body)]
    form))

(defmethod form/form ::form/defn
  [ctx _]
  (let [ctx  (ensure-ctx (assoc ctx ::form/ident ::form/fn))
        sym  (form/form ctx ::var-symbol)
        form (form/form ctx ::form/fn)]
    `(def ~sym ~form)))


   ;;;
   ;;; NOTE: `form/metadata` helpers
   ;;;


(def spec-param-tags
  (hash-set
   :fm.context/nominal
   ::s/registry-keyword
   ::form/positional-binding-map))


   ;;;
   ;;; NOTE: `form/metadata` implementations
   ;;;


(defmethod form/metadata [::form/fn :fm/sequent]
  [ctx [_ sequent-tag]]
  (let [form (form/form ctx [::metadata sequent-tag])]
    (hash-map :fm/sequent form))) ; NOTE: normalize tag

(defmethod form/metadata [::form/fn :fm.metadata/default]
  [ctx [_ tag]]
  (let [form (form/form ctx [::metadata tag])]
    (hash-map tag form)))

(defmethod form/metadata [::form/fn :default]
  [ctx _]
  (let [tag  (get ctx ::form/tag)
        form (form/form ctx [::metadata :default])]
    (hash-map tag form)))

(defmethod form/metadata [::form/fn :fm.signature/singular]
  [ctx _]
  (let [definition (get ctx ::definition)
        sym        (when (symbol? (first definition)) (first definition))
        signature  (if sym (rest definition) definition)
        ctx        (assoc ctx ::signature signature)
        outer      (merge
                    (meta sym)
                    (meta (first signature))
                    (form/metadata ctx ::signature))
        ctx        (assoc ctx ::outer-metadata outer)
        tags       (into (hash-set :fm/ident :fm/arglists) (keys outer))
        metadata   (into (hash-map) (map (partial form/metadata ctx)) tags)]
    metadata)) ; TODO: include `retv`; TODO: capture defaults

(defmethod form/metadata [::form/fn :fm.signature/plural]
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
                         (form/metadata ctx ::signature))))
                    signatures)
        ctx        (assoc ctx ::outer-metadata outer ::inner-metadatas inners)
        tags       (into (hash-set :fm/ident :fm/arglists) (mapcat keys) (cons outer inners))
        metadata   (into (hash-map) (map (partial form/metadata ctx)) tags)]
    metadata))

(defmethod form/metadata ::signature
  [ctx _]
  (let [argv (get-in ctx [::conformed-signatures (signature-index ctx) ::argv])
        retv (get-in ctx [::conformed-signatures (signature-index ctx) ::retv])]
    (merge
     (when (lib/deep-some spec-param-tags argv)
       (let [tag  [::metadata :fm/args ::form/conformed-specv]
             form (form/form argv tag)]
         (hash-map :fm/args form)))
     (when (lib/deep-some spec-param-tags retv)
       (let [tag  [::metadata :fm/ret ::form/conformed-specv]
             form (form/form retv tag)]
         (hash-map :fm/ret form))))))

(defmethod form/metadata [::var-metadata :fm/arglists]
  [ctx [_ meta-tag]]
  (let [tag  (keyword (name meta-tag))
        form (get-in ctx [::metadata meta-tag])]
    (hash-map tag `(quote ~(seq form)))))

(defmethod form/metadata [::var-metadata :fm.metadata/var-metadata]
  [ctx [_ meta-tag]]
  (let [tag  (keyword (name meta-tag))
        form (get-in ctx [::metadata meta-tag])]
    (hash-map tag form)))

(defmethod form/metadata [::var-metadata :fm.metadata/default]
  [_ _]
  nil) ; NOTE: exclude recognized metadata tags on var by default

(defmethod form/metadata [::var-metadata :line]
  [ctx _]
  (let [data (get-in ctx [::metadata :line])
        form (if (sequential? data) (first data) data)]
    (hash-map :line form))) ; NOTE: `:line` is a vector in cljs

(defmethod form/metadata [::var-metadata :column]
  [ctx _]
  (let [data (get-in ctx [::metadata :column])
        form (if (sequential? data) (first data) data)]
    (hash-map :column form))) ; NOTE: `:column` is a vector in cljs

(defmethod form/metadata [::var-metadata :default]
  [ctx _]
  (let [tag  (get ctx ::form/tag)
        form (get-in ctx [::metadata tag])]
    (hash-map tag form)))


   ;;;
   ;;; NOTE: `form/binding` implementations
   ;;;


(defmethod form/binding ::args
  [ctx _]
  (let [context (get-in ctx [::signature-contexts (signature-index ctx) :fm/args])]
    (form/binding ctx [::args context])))

(defmethod form/binding [::args :fm.context/positional]
  [ctx _]
  (let [sym  (get-in ctx [::arglist-symbols (signature-index ctx)])
        form (form/form ctx [::form/binding ::form/fn ::args])]
    (hash-map
     ::core.specs/binding-form sym
     ::core.specs/local-name sym
     ::form/form form)))

(defmethod form/binding [::args :fm.context/nominal]
  [ctx _]
  (let [sym (get-in ctx [::arglist-symbols (signature-index ctx)])]
    (hash-map
     ::core.specs/binding-form sym
     ::core.specs/local-name sym)))

(defmethod form/binding ::destructured-argx
  [ctx _]
  (let [context (get-in ctx [::signature-contexts (signature-index ctx) :fm/args])]
    (form/binding ctx [::destructured-argx context])))

(defmethod form/binding [::destructured-argx :fm.context/positional]
  [ctx _]
  (let [arglist      (get-in ctx [::normalized-arglists (signature-index ctx)])
        sym          (get-in ctx [::arglist-symbols (signature-index ctx)])
        binding-form (lib/positional-combine [arglist :as sym])
        form         (form/form ctx [::form/binding ::form/fn ::combined-argxs])]
    (hash-map
     ::core.specs/binding-form binding-form
     ::core.specs/local-name sym
     ::form/form form)))

(defmethod form/binding [::destructured-argx :fm.context/nominal]
  [ctx _]
  (let [binding-form (first (get-in ctx [::normalized-arglists (signature-index ctx)]))
        sym          (form/local-name-for binding-form)
        form         (form/form ctx [::form/binding ::form/fn ::combined-argxs])]
    (hash-map
     ::core.specs/binding-form binding-form
     ::core.specs/local-name sym
     ::form/form form)))

(defmethod form/binding :fm.metadata/signature-indexed
  [ctx tag]
  (get-in
   (reduce
    (fn deduplicate [acc i]
      (let [acc     (assoc acc ::signature-index i)
            f       (fnil conj (vector))
            binding (form/default-binding acc tag)]
        (update-in acc [::form/bindings tag] f binding)))
    ctx
    (range
     (count-signatures ctx)))
   (vector ::form/bindings tag)))

(defmethod form/binding :fm.binding/implicitly-bound
  [_ tag]
  (let [sym (gensym (name tag))]
    (hash-map
     ::core.specs/binding-form sym
     ::core.specs/local-name sym)))


   ;;;
   ;;; NOTE: `form/form`, `form/forms` helpers
   ;;;


(defn dispatch-kv
  ([f [k v]] (f v k))
  ([f t [k v]]
   (let [k (lib/positional-combine [t k])]
     (f v k))))

(def dispatch-form-kv
  (partial dispatch-kv form/form))

(def arglist-metadata
  (partial dispatch-form-kv [::metadata :fm/arglist]))

(def args-metadata
  (partial dispatch-form-kv [::metadata :fm/args]))

(defn normalized-binding-tuple [[k [tag conformed]]]
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
  [shape args]
  (lib/zipvf vector? (fn [_ a] a) shape args))

(def mapv-any?
  (let [f (fn [x] (if (= x '&) x `any?))]
    (partial mapv f)))

(def plural-combine-warning
  (str
   "Multiple `:fm.sequent/combine` forms specified; "
   "all `:fm.sequent/combine` single-arities must be compatible."
   "\n\nChoosing one at random..."))


   ;;;
   ;;; NOTE: `form/form` implementations
   ;;;


(defmethod form/form ::simple-symbol
  [ctx _]
  (symbol (name (form/form ctx [::metadata :fm/ident]))))

(defmethod form/form ::metadata
  [ctx _]
  (let [metadata (get ctx ::metadata)]
    `(quote ~metadata)))

(defmethod form/form ::var-metadata
  [ctx tag]
  (let [ctx (assoc ctx ::form/ident tag)]
    (into
     (hash-map)
     (map (partial form/metadata ctx))
     (keys (get ctx ::metadata)))))

(defmethod form/form ::body
  [ctx _]
  (let [arglist (get-in ctx [::metadata :fm/arglists (signature-index ctx)])]
    (cond
      (not (throw? ctx)) (form/form ctx ::try)
      (seq arglist)      (form/form ctx [::bind ::args])
      :else              (form/form ctx [::bind ::ret]))))

(defmethod form/form [::body :fm/sequent]
  [ctx _]
  (let [throws (form/form ctx ::indexed-throw!-booleans)]
    (if (every? true? throws)
      (form/form ctx [::bind ::combined-argxs])
      (form/form ctx [::try :fm/sequent]))))

(defmethod form/form ::try
  [ctx _]
  (let [args    (form/form ctx [::form/binding ::form/fn ::args])
        args?   (or (symbol? args) (and (sequential? args) (seq args)))
        body    (if args?
                  (form/form ctx [::bind ::args])
                  (form/form ctx [::bind ::ret]))
        handler (form/form ctx [::form/binding ::form/fn :fm/handler])
        ident   (form/form ctx [::metadata :fm/ident])]
    `(try
       ~body
       (catch Throwable thrown#
         (~handler
          {:fm/ident           ~ident
           ::anomaly/ident     ::anomaly/thrown
           ::anomaly/args      ~args
           ::anomaly/throwable thrown#})))))

(defmethod form/form [::try :fm/sequent]
  [ctx _]
  (let [body       (form/form ctx [::bind ::combined-argxs])
        ctx        (form/bind ctx [::thrown])
        thrown     (form/form ctx [::form/binding ::form/fn ::thrown])
        catch-body (form/form ctx [::catch-body :fm/sequent])]
    `(try
       ~body
       (catch Throwable ~thrown
         ~catch-body))))

(defmethod form/form [::bind :fm.form.fn/argx]
  [ctx [_ form-tag]]
  (let [ctx       (form/bind ctx [form-tag])
        bindings  (form/bindings ctx [form-tag])
        trace     (when (trace? ctx :fm/args)
                    (form/forms ctx [::trace form-tag]))
        validate? (or (metadata? ctx :fm/args)
                      (and
                       (sequent? ctx)
                       (= (signature-tag ctx) :fm.signature/plural)))
        dispatch? (sequent? ctx)
        tag       (cond
                    validate? [::validate form-tag]
                    dispatch? [::dispatch form-tag]
                    :else     [::bind ::ret])
        body      (form/form ctx tag)
        form      (cond
                    (seq bindings) `(let [~@bindings] ~@trace ~body)
                    (some? trace)  `(do ~@trace ~body)
                    :else          body)]
    form))

(defmethod form/form [::validate :fm.form.fn/argx]
  [ctx [_ form-tag]]
  (let [ctx       (form/bind ctx [[form-tag ::s/conformed]])
        bindings  (form/bindings ctx [[form-tag ::s/conformed]])
        trace     (when (and (trace? ctx :fm/args) (conform? ctx :fm/args))
                    (form/forms ctx [::trace form-tag ::s/conformed]))
        ident     (form/form ctx [::metadata :fm/ident])
        handler   (form/form ctx [::form/binding ::form/fn :fm/handler])
        args-spec (form/form ctx [::form/binding ::form/fn :fm/args])
        args      (form/form ctx [::form/binding ::form/fn form-tag])
        conformed (form/form ctx [::form/binding ::form/fn form-tag ::s/conformed])
        tag       (cond
                    (sequent? ctx)          [::dispatch form-tag]
                    (conform? ctx :fm/args) [::bind form-tag ::s/conformed]
                    :else                   [::bind ::ret])
        body      (form/form ctx tag)]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/args
           ::s/explain-data (s/explain-data ~args-spec ~args)})
         ~body))))

(defmethod form/form [::dispatch :fm.form.fn/argx]
  [ctx tag]
  (let [sig-tag (if (= (count-signatures ctx) 1)
                  :fm.signature/singular
                  :fm.signature/plural)] ; NOTE: ignore syntactical distinction
    (form/form ctx (conj tag sig-tag))))

(defmethod form/form [::dispatch :fm.form.fn/argx :fm.signature/singular]
  [ctx [_ form-tag _]]
  (let [ctx       (form/bind ctx [::destructured-argx])
        bindings  (form/bindings ctx [::destructured-argx])
        args      (form/form ctx [::form/binding ::form/fn ::destructured-argx])
        conformed (form/form ctx [::form/binding ::form/fn form-tag ::s/conformed])
        conformed (when (conform? ctx :fm/args) (list args conformed))
        trace     (when (and (trace? ctx :fm/args) (conform? ctx :fm/args))
                    (form/forms ctx [::trace form-tag ::s/conformed]))
        ctx       (assoc-in ctx [::form/bindings ::args]
                            (get-in ctx [::form/bindings ::destructured-argx]))
        body      (form/form ctx [::bind ::ret])]
    `(let [~@bindings
           ~@conformed]
       ~@trace
       ~body))) ; ALT: `(derive ::destructured-argx ::args)`, `form/geta`

(defmethod form/form [::dispatch :fm.form.fn/argx :fm.signature/plural]
  [ctx [_ form-tag _]]
  (let [conformed (form/form ctx [::form/binding ::form/fn form-tag ::s/conformed])
        throws    (form/form ctx ::indexed-throw!-booleans)
        tags      (map (comp keyword str) (range (count-signatures ctx)))
        tag       (if (every? true? throws)
                    [::dispatch form-tag :fm.signature/singular]
                    [::try :fm/sequent ::signature])
        forms     (map (fn [index]
                         (let [ctx (assoc ctx ::signature-index index)]
                           (form/form ctx tag)))
                       (range (count-signatures ctx)))]
    `(case (first ~conformed)
       ~@(interleave tags forms))))

(defmethod form/form [::try :fm/sequent ::signature]
  [ctx _]
  (let [body       (form/form ctx [::dispatch ::combined-argxs :fm.signature/singular])
        ctx        (form/bind ctx [::thrown])
        thrown     (form/form ctx [::form/binding ::form/fn ::thrown])
        catch-body (form/form ctx [::catch-body :fm/sequent ::signature])]
    `(try
       ~body
       (catch Throwable ~thrown
         ~catch-body))))

(defmethod form/form [::bind :fm.form.fn/argx ::s/conformed]
  [ctx [_ form-tag _]]
  (let [args      (form/form ctx [::form/binding ::form/fn form-tag])
        conformed (form/form ctx [::form/binding ::form/fn form-tag ::s/conformed])
        body      (form/form ctx [::bind ::ret])]
    `(let [~args ~conformed]
       ~body)))

  ;; TODO: `:fm.anomaly/deep-detect`, `:fm/ignore`
  ;; TODO: unify `::ret`, `::retx`
(defmethod form/form [::bind ::ret]
  [ctx _]
  (let [ctx      (form/bind ctx [::ret])
        bindings (form/bindings ctx [::ret])
        trace    (when (trace? ctx :fm/ret) (form/forms ctx [::trace ::ret]))
        handler  (form/form ctx [::form/binding ::form/fn :fm/handler])
        ret      (form/form ctx [::form/binding ::form/fn ::ret])
        body     (cond
                   (metadata? ctx :fm/ret) (form/form ctx [::validate ::ret])
                   (metadata? ctx :fm/rel) (form/form ctx [::validate ::rel])
                   (sequent? ctx)          (form/form ctx ::retx)
                   :else                   ret)]
    `(let [~@bindings]
       ~@trace
       (if (anomaly/anomaly? ~ret)
         (~handler ~ret)
         ~body))))

(defmethod form/form [::validate ::ret]
  [ctx _]
  (let [ctx       (form/bind ctx [[::ret ::s/conformed]])
        bindings  (form/bindings ctx [[::ret ::s/conformed]])
        trace     (when (and (trace? ctx :fm/ret) (conform? ctx :fm/ret))
                    (form/forms ctx [::trace ::ret ::s/conformed]))
        ident     (form/form ctx [::metadata :fm/ident])
        handler   (form/form ctx [::form/binding ::form/fn :fm/handler])
        ret-spec  (form/form ctx [::form/binding ::form/fn :fm/ret])
        args      (form/form ctx [::form/binding ::form/fn ::args])
        ret       (form/form ctx [::form/binding ::form/fn ::ret])
        conformed (form/form ctx [::form/binding ::form/fn ::ret ::s/conformed])
        body      (cond
                    (conform? ctx :fm/ret)  (form/form ctx [::bind ::ret ::s/conformed])
                    (metadata? ctx :fm/rel) (form/form ctx [::validate ::rel])
                    :else                   (form/form ctx ::retx))]
    `(let [~@bindings]
       ~@trace
       (if (s/invalid? ~conformed)
         (~handler
          {:fm/ident        ~ident
           ::anomaly/ident  ::anomaly/ret
           ::anomaly/args   ~args
           ::s/explain-data (s/explain-data ~ret-spec ~ret)})
         ~body))))

(defmethod form/form [::bind ::ret ::s/conformed]
  [ctx _]
  (let [ret       (form/form ctx [::form/binding ::form/fn ::ret])
        conformed (form/form ctx [::form/binding ::form/fn ::ret ::s/conformed])
        body      (if (metadata? ctx :fm/rel)
                    (form/form ctx [::validate ::rel])
                    ret)]
    `(let [~ret ~conformed]
       ~body)))

(defmethod form/form [::validate ::rel]
  [ctx _]
  (let [ident (form/form ctx [::metadata :fm/ident])
        rel   (form/form ctx [::form/binding ::form/fn :fm/rel])
        args  (form/form ctx [::form/binding ::form/fn ::args])
        ret   (form/form ctx [::form/binding ::form/fn ::ret])
        retx  (form/form ctx ::retx)]
    `(if (s/valid? ~rel {:args ~args :ret ~ret})
       ~retx
       {:fm/ident        ~ident
        ::anomaly/ident  ::anomaly/rel
        ::s/explain-data (s/explain-data ~rel {:args ~args :ret ~ret})})))


  ;;
  ;; NOTE: `form/binding` form implementations
  ;;


(defmethod form/form [::form/binding ::form/fn :fm/spec]
  [ctx [_ _ spec-tag]]
  (or (get-in ctx [::form/bindings spec-tag (signature-index ctx) ::core.specs/local-name])
      (if (sequent? ctx)
        (form/form ctx [::s/form spec-tag :fm/sequent])
        (form/form ctx [::s/form spec-tag]))))

(defmethod form/form [::form/binding ::form/fn :fm/trace]
  [ctx _]
  (let [index (signature-index ctx)]
    (or (get-in ctx [::form/bindings :fm/trace index ::core.specs/local-name])
        (when-let [form (or (get-in ctx [::metadata :fm/trace index])
                            (get-in ctx [::defaults :fm/trace]))]
          (let [pred? (some-fn true? set?)
                fn?   (some-fn list? symbol?)]
            (cond
              (pred? form)  (get-in ctx [::defaults :fm/trace-fn])
              (fn? form)    form
              (false? form) nil ; ALT: `form`, `false`
              :else         `(partial ~(get-in ctx [::defaults :fm/trace-fn]) ~form)))))))

  ;; ALT: anomaly map literal when `nil`
(defmethod form/form [::form/binding ::form/fn :fm/handler]
  [ctx _]
  (let [index (signature-index ctx)]
    (or (get-in ctx [::form/bindings :fm/handler index ::core.specs/local-name])
        (let [form (get-in ctx [::metadata :fm/handler index])
              fn?  (some-fn list? symbol?)]
          (cond
            (fn? form) form
            :else      (get-in ctx [::defaults :fm/handler])))))) ; TODO: nil handler case

(defmethod form/form [::form/binding ::form/fn :fm.sequent/combine]
  [ctx _]
  (let [index (signature-index ctx)]
    (or (get-in ctx [::form/bindings :fm.sequent/combine index ::core.specs/local-name])
        (get-in ctx [::metadata :fm/sequent index :fm.sequent/combine]))))

(defmethod form/form [::form/binding ::form/fn :fm.binding/default]
  [ctx [_ _ form-tag]]
  (or (get-in ctx [::form/bindings form-tag ::core.specs/local-name])
      (form/form ctx form-tag)))

(defmethod form/form [::form/binding ::form/fn :fm.binding/default ::s/conformed]
  [ctx tag]
  (let [tag (vec (take-last 2 tag))]
    (or (get-in ctx [::form/bindings tag ::core.specs/local-name])
        (form/form ctx tag))))


  ;;
  ;; NOTE: nested form implementations, ctx-free form transformations
  ;;


(defmethod form/form [::metadata :fm/ident]
  [ctx _]
  (or
   (get-in ctx [::metadata :fm/ident])
   (keyword
    (str (get ctx ::form/ns))
    (name
     (or
      (get-in ctx [::conformed-definition :fm.definition/simple-symbol])
      (gensym (name (get ctx ::form/ident)))))))) ; TODO: sequent

(defmethod form/form [::metadata :fm/arglists :fm.signature/singular]
  [ctx _]
  (let [argv    (get-in ctx [::conformed-definition :fm.definition/rest 1 ::argv])
        arglist (form/form argv [::metadata :fm/arglist ::form/conformed-specv])]
    (vector arglist)))

(defmethod form/form [::metadata :fm/arglists :fm.signature/plural]
  [ctx _]
  (let [argvs    (map ::argv (get-in ctx [::conformed-definition :fm.definition/rest 1]))
        arglists (map (fn [argv] (form/form argv [::metadata :fm/arglist ::form/conformed-specv])) argvs)]
    (vec arglists)))

(defmethod form/form [::metadata :fm/arglist ::form/conformed-specv]
  [[context data] _]
  (let [tag       [::metadata :fm/arglist ::form/conformed-param-list context]
        conformed (case context
                    :fm.context/positional data
                    :fm.context/nominal    (first data))]
    (form/form conformed tag)))

(defmethod form/form [::metadata :fm/arglist ::form/conformed-param-list
                      :fm.context/positional]
  [conformed tag]
  (let [params     (when-let [params (get conformed :params)]
                     (form/forms params (conj tag :params)))
        var-params (when-let [var-params (get conformed :var-params)]
                     (form/forms var-params (conj tag :var-params)))]
    `[~@params ~@var-params]))

(defmethod form/form [::metadata :fm/arglist ::form/conformed-param-list
                      :fm.context/nominal]
  [conformed tag]
  (let [map-destructure (if-let [params (get conformed :params)]
                          (form/form params (conj tag :params))
                          (hash-map))
        map-destructure (if-let [as-sym (get-in conformed [:as-form :as-sym])]
                          (assoc map-destructure :as as-sym)
                          map-destructure)]
    `[~map-destructure]))

(defmethod form/form [::metadata :fm/arglist ::form/conformed-param-list
                      :fm.context/nominal :params]
  [params _]
  (into (hash-map) (map arglist-metadata) params))

(defmethod form/form [::metadata :fm/arglist ::s/registry-keyword]
  [k _]
  (symbol (name k)))

(defmethod form/form [::metadata :fm/arglist ::form/positional-binding-map]
  [m _]
  (normalized-arglist-metadata (first m)))

(defmethod form/form [::metadata :fm/arglist ::core.specs/binding-form]
  [conformed _]
  (arglist-metadata conformed))

(defmethod form/form [::metadata :fm/arglist :keyword]
  [k _]
  (let [sym (symbol (name k))]
    (hash-map sym k)))

(defmethod form/form [::metadata :fm/arglist ::form/nominal-binding-map]
  [m _]
  (into (hash-map) (map (juxt normalized-arglist-metadata first)) m))

(defmethod form/form [::metadata :fm/arglist :local-symbol] [sym _] sym)
(defmethod form/form [::metadata :fm/arglist :map-destructure] [m _] m)
(defmethod form/form [::metadata :fm/arglist :seq-destructure]
  [conformed _]
  (let [forms      (map arglist-metadata (get conformed :forms))
        rest-forms (when-let [rest-form (get-in conformed [:rest-forms :form])]
                     (let [form (arglist-metadata rest-form)]
                       `(& ~form)))
        as-forms   (when-let [as-sym (get-in conformed [:as-form :as-sym])]
                     `(:as ~as-sym))] ; ALT: `form/forms`
    `[~@forms ~@rest-forms ~@as-forms]))

(defmethod form/form [::metadata :fm/args :fm.signature/plural]
  [ctx _]
  (let [outer      (get-in ctx [::outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::inner-metadatas))
        signatures (get-in ctx [::conformed-definition :fm.definition/rest 1])
        arglist    (fn [argv] (form/form argv [::metadata :fm/arglist ::form/conformed-specv]))
        arglists   (map (comp arglist ::argv) signatures)
        args       (map-indexed
                    (fn [i inner]
                      (let [arglist (nth arglists i)]
                        (if-let [args (or inner outer)]
                          (zipv-args arglist args)
                          (mapv-any? arglist)))) ; ALT: `form/metadata` `::signature`
                    inners)]
    (vec args))) ; TODO: qualify, `s/explicate`?

(defmethod form/form [::metadata :fm/spec :keyword] [k _] k)
(defmethod form/form [::metadata :fm/spec ::s/registry-keyword] [k _] k)
(defmethod form/form [::metadata :fm/spec ::core.specs/binding-form] [_ _] `any?) ; TODO: additional inference
(defmethod form/form [::metadata :fm/spec ::form/positional-binding-map] [m _] (first (keys m)))
(defmethod form/form [::metadata :fm/spec ::form/nominal-binding-map] [m _] (keys m))
(defmethod form/form [::metadata :fm/spec ::form/conformed-specv]
  [[context data] [form-tag spec-tag _]]
  (let [tag       [form-tag spec-tag ::form/conformed-param-list context]
        conformed (case context
                    :fm.context/positional data
                    :fm.context/nominal    (first data))]
    (form/form conformed tag)))

(defmethod form/form [::metadata :fm/spec ::form/conformed-param-list
                      :fm.context/positional]
  [conformed [form-tag spec-tag data-tag _]]
  (form/form conformed [form-tag spec-tag data-tag]))

(defmethod form/form [::metadata :fm/spec ::form/conformed-param-list
                      :fm.context/nominal]
  [conformed [form-tag spec-tag data-tag _]]
  (vector ; NOTE: retain outer `[]` ; ALT: `hash-set` "keyset"
   (vec
    (sort
     (form/form conformed [form-tag spec-tag data-tag])))))

(defmethod form/form [::metadata :fm/spec ::form/conformed-param-list]
  [conformed tag]
  (let [params   (when-let [params (get conformed :params)]
                   (form/forms params (conj tag :params)))
        var-form (when-let [var-params (get conformed :var-params)]
                   (form/forms var-params (conj tag :var-params)))]
    `[~@params ~@var-form]))

(defmethod form/form [::metadata :fm/doc]
  [ctx _]
  (let [ctx (assoc ctx ::form/tag :fm/doc)]
    (form/form ctx [::metadata :default])))

(defmethod form/form [::metadata :fm/sequent]
  [ctx tag]
  (form/form ctx (conj tag (signature-tag ctx))))

(defmethod form/form [::metadata :fm/sequent :fm.signature/singular]
  [ctx [_ sequent-tag _ :as tag]]
  (let [data (get-in ctx [::outer-metadata sequent-tag])
        form (if (true? data)
               (let [tag [::metadata sequent-tag ::signature]]
                 (form/form ctx tag))
               data)]
    (vector form)))

(defmethod form/form [::metadata :fm/sequent :fm.signature/plural]
  [ctx _]
  (let [outer  (form/finda (get ctx ::outer-metadata) :fm/sequent)
        inners (map
                (fn [metadata] (form/finda metadata :fm/sequent))
                (get ctx ::inner-metadatas))
        forms  (map-indexed
                (fn fallback [i inner]
                  (let [[sequent-tag data] (or inner outer (rand-nth (filter some? inners)))]
                    (if (true? data)
                      (let [ctx (assoc ctx ::signature-index i)
                            tag [::metadata sequent-tag ::signature]]
                        (form/form ctx tag))
                      data)))
                inners)]
    (vec forms))) ; TODO: `warn!` unspecified sequent signature

  ;; TODO: warn unspecified mismatch ret context in `:fm.sequent/merge`
  ;; TODO: `normalized-sequent-ident`
(defmethod form/form [::metadata :fm/sequent ::signature]
  [ctx [_ sequent-tag _]]
  (let [context (get-in ctx [::signature-contexts (signature-index ctx) :fm/args])
        combine (case context
                  :fm.context/positional `lib/positional-combine
                  :fm.context/nominal    `lib/nominal-combine)]
    (hash-map
     :fm.sequent/ident sequent-tag
     :fm.sequent/combine combine)))

(defmethod form/form [::metadata :fm.metadata/signature-indexed :fm.signature/plural]
  [ctx [_ meta-tag _]]
  (let [outer  (get-in ctx [::outer-metadata meta-tag])
        inners (map meta-tag (get ctx ::inner-metadatas))
        metas  (map (fn fallback [inner] (or inner outer)) inners)]
    (vec metas)))

(defmethod form/form [::metadata :fm.metadata/default :fm.signature/singular]
  [ctx [_ meta-tag _]]
  (let [metadata (get-in ctx [::outer-metadata meta-tag])]
    (vector metadata))) ; NOTE: vector for signature indexing

(defmethod form/form [::metadata :fm.metadata/default]
  [ctx tag]
  (form/form ctx (conj tag (signature-tag ctx))))

(defmethod form/form [::metadata :default]
  [ctx tag]
  (form/form ctx (conj tag (signature-tag ctx)))) ; NOTE: default for unrecognized tags and `:fm/doc`

(defmethod form/form [::metadata :default :fm.signature/singular]
  [ctx _]
  (let [tag  (get ctx ::form/tag)
        form (get-in ctx [::outer-metadata tag])]
    form))

(defmethod form/form [::metadata :default :fm.signature/plural]
  [ctx _]
  (let [tag    (get ctx ::form/tag)
        outer  (get-in ctx [::outer-metadata tag])
        inners (map tag (get ctx ::inner-metadatas))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (vec inners) ; NOTE: at least one
                 :else                (vec (cons outer inners)))]
    form))

(defmethod form/form ::args
  [ctx _]
  (let [context (get-in ctx [::signature-contexts (signature-index ctx) :fm/args])]
    (form/form ctx [::args context])))

(defmethod form/form [::args :fm.context/positional]
  [ctx _]
  (let [arglist (get-in ctx [::normalized-arglists (signature-index ctx)])
        xf      (map form/local-name-for)
        form    (if (some #{'&} arglist)
                  (let [xf   (comp (take-while (complement #{'&})) xf)
                        args (into (vector) xf arglist)
                        var  (form/local-name-for (last arglist))]
                    `(into ~args ~var))
                  (into (vector) xf arglist))]
    form))

(defmethod form/form [::args :fm.context/nominal]
  [ctx _]
  (get-in ctx [::arglist-symbols (signature-index ctx)]))

(defmethod form/form ::combined-argxs
  [ctx _]
  (let [bindings (get-in ctx [::form/bindings :fm.sequent/combine])
        bindings (map ::core.specs/local-name bindings)
        _        (when-not (lib/singular? (set bindings))
                   (form/warn! plural-combine-warning))
        combine  (rand-nth bindings)
        argxs    (form/form ctx [::form/binding ::form/fn ::argxs])]
    `(~combine ~argxs)))

(defmethod form/form [:fm.form.fn/argx ::s/conformed]
  [ctx [form-tag _]]
  (let [spec (form/form ctx [::form/binding ::form/fn :fm/args])
        args (form/form ctx [::form/binding ::form/fn form-tag])]
    `(s/conform ~spec ~args)))

(defmethod form/form ::ret
  [ctx _]
  (let [body (get-in ctx [::conformed-signatures (signature-index ctx) ::body])
        form (if (lib/singular? body) (first body) `(do ~@body))]
    form))

(defmethod form/form [::ret ::s/conformed]
  [ctx _]
  (let [spec (form/form ctx [::form/binding ::form/fn :fm/ret])
        ret  (form/form ctx [::form/binding ::form/fn ::ret])]
    `(s/conform ~spec ~ret)))

(defmethod form/form ::retx
  [ctx _]
  (let [t (or (sequent-tag ctx) :fm.sequent/conse)]
    (form/form ctx [::retx t])))

(defmethod form/form [::retx :fm.sequent/conse]
  [ctx _]
  (form/form ctx [::form/binding ::form/fn ::ret]))

(defmethod form/form [::retx :fm.sequent/nonse]
  [ctx _]
  (form/form ctx [::form/binding ::form/fn ::args]))

(defmethod form/form [::retx :fm.sequent/merge]
  [ctx _]
  (let [combine (get-in ctx [::metadata :fm/sequent (signature-index ctx)
                             :fm.sequent/combine])
        args    (form/form ctx [::form/binding ::form/fn ::args])
        ret     (form/form ctx [::form/binding ::form/fn ::ret])]
    `(~combine ~args ~ret)))

(defmethod form/form [::catch-body :fm/sequent]
  [ctx tag]
  (let [handlers          (form/form ctx ::indexed-handler-symbols)
        throws            (form/form ctx ::indexed-throw!-booleans)
        none-throw?       (every? (complement boolean) throws)
        handler-singular? (lib/singular? (set handlers))
        t                 (vector
                           (if none-throw? :fm.throw/none :fm.throw/plural)
                           (if handler-singular? :fm.handler/singular :fm.handler/plural))
        tag               (lib/positional-combine [tag t])]
    (form/form ctx tag)))

(defmethod form/form [::catch-body :fm/sequent :fm.throw/none :fm.handler/singular]
  [ctx tag]
  (let [handler  (form/form ctx [::form/binding ::form/fn :fm/handler])
        ctx      (form/bind ctx [::ex-data])
        bindings (form/bindings ctx [::ex-data])
        tag      (lib/positional-combine [tag ::anomaly ::anomaly/thrown])
        anomaly  (form/form ctx tag)]
    `(let [~@bindings]
       (~handler ~anomaly))))

(defmethod form/form [::catch-body :fm/sequent :fm.throw/none :fm.handler/plural]
  [ctx tag]
  (let [tags     (vector ::ex-data ::ex-signature-index ::ex-handler)
        ctx      (form/bind ctx tags)
        bindings (form/bindings ctx tags)
        handler  (form/form ctx [::form/binding ::form/fn ::ex-handler])
        tag      (lib/positional-combine [tag ::anomaly ::anomaly/thrown])
        anomaly  (form/form ctx tag)]
    `(let [~@bindings]
       (~handler ~anomaly))))

(defmethod form/form [::catch-body :fm/sequent :fm.throw/plural :fm.handler/singular]
  [ctx tag]
  (let [tags     (vector ::ex-data ::ex-signature-index)
        ctx      (form/bind ctx tags)
        bindings (form/bindings ctx tags)
        handler  (form/form ctx [::form/binding ::form/fn :fm/handler])
        thrown   (form/form ctx [::form/binding ::form/fn ::thrown])
        data     (form/form ctx [::form/binding ::form/fn ::ex-data])
        index    (form/form ctx [::form/binding ::form/fn ::ex-signature-index])
        throws   (form/form ctx ::indexed-throw!-booleans)
        tag      (lib/positional-combine [tag ::anomaly ::anomaly/thrown])
        anomaly  (form/form ctx tag)]
    `(let [~@bindings]
       (if (get ~throws ~index false)
         (throw (get ~data ::anomaly/thrown ~thrown))
         (~handler ~anomaly)))))

(defmethod form/form [::catch-body :fm/sequent :fm.throw/plural :fm.handler/plural]
  [ctx tag]
  (let [ctx       (form/bind ctx [::ex-data ::ex-signature-index ::ex-handler])
        bindings1 (form/bindings ctx [::ex-data ::ex-signature-index])
        thrown    (form/form ctx [::form/binding ::form/fn ::thrown])
        data      (form/form ctx [::form/binding ::form/fn ::ex-data])
        index     (form/form ctx [::form/binding ::form/fn ::ex-signature-index])
        throws    (form/form ctx ::indexed-throw!-booleans)
        bindings2 (form/bindings ctx [::ex-handler])
        handler   (form/form ctx [::form/binding ::form/fn ::ex-handler])
        tag       (lib/positional-combine [tag ::anomaly ::anomaly/thrown])
        anomaly   (form/form ctx tag)]
    `(let [~@bindings1]
       (if (get ~throws ~index false)
         (throw (get ~data ::anomaly/throwable ~thrown))
         (let [~@bindings2]
           (~handler ~anomaly))))))

(defmethod form/form [::catch-body :fm/sequent :fm.throw/default :fm.handler/default
                      ::anomaly ::anomaly/thrown]
  [ctx _]
  (let [ident  (form/form ctx [::metadata :fm/ident])
        argxs  (form/form ctx [::form/binding ::form/fn ::argxs])
        thrown (form/form ctx [::form/binding ::form/fn ::thrown])
        data   (form/form ctx [::form/binding ::form/fn ::ex-data])]
    `(if (anomaly/anomaly? ~data)
       ~data
       {:fm/ident           ~ident
        ::anomaly/ident     ::anomaly/thrown
        ::anomaly/args      (vec ~argxs)
        ::anomaly/throwable ~thrown})))

(defmethod form/form [::catch-body :fm/sequent ::signature]
  [ctx tag]
  (let [handler (form/form ctx [::form/binding ::form/fn :fm/handler])
        throws  (form/form ctx ::indexed-throw!-booleans)
        tag     (lib/positional-combine [tag ::anomaly ::anomaly/thrown])
        anomaly (form/form ctx tag)
        form    (if (get throws (signature-index ctx))
                  `(throw (ex-info (str ::anomaly/thrown) ~anomaly))
                  `(~handler ~anomaly))]
    form))

(defmethod form/form [::catch-body :fm/sequent ::signature
                      ::anomaly ::anomaly/thrown]
  [ctx _]
  (let [ident  (form/form ctx [::metadata :fm/ident])
        args   (form/form ctx [::form/binding ::form/fn ::combined-argxs])
        thrown (form/form ctx [::form/binding ::form/fn ::thrown])
        index  (signature-index ctx)]
    {:fm/ident           ident
     ::anomaly/ident     ::anomaly/thrown
     ::anomaly/args      args
     ::anomaly/throwable thrown
     ::signature-index   index}))

(defmethod form/form ::indexed-handler-symbols
  [ctx _]
  (into
   (vector)
   (map ::core.specs/local-name)
   (get-in ctx [::form/bindings :fm/handler])))

(defmethod form/form ::indexed-throw!-booleans
  [ctx _]
  (if-let [forms (seq (get-in ctx [::metadata :fm/throw!]))]
    (into (vector) (map boolean) forms)
    (let [b (boolean (get-in ctx [::defaults :fm/throw!]))]
      (into
       (vector)
       (map (constantly b))
       (range (count-signatures ctx))))))

(defmethod form/form ::ex-data
  [ctx _]
  (let [thrown (form/form ctx [::form/binding ::form/fn ::thrown])]
    `(ex-data ~thrown)))

(defmethod form/form ::ex-signature-index
  [ctx _]
  (let [data (form/form ctx [::form/binding ::form/fn ::ex-data])]
    `(get ~data ::signature-index)))

(defmethod form/form ::ex-handler
  [ctx _]
  (let [handlers (form/form ctx ::indexed-handler-symbols)
        index    (form/form ctx [::form/binding ::form/fn ::ex-signature-index])
        fallback (first (remove nil? handlers))]
    `(get ~handlers ~index ~fallback)))

(defmethod form/form ::var-symbol
  [ctx _]
  (with-meta
    (form/form ctx ::simple-symbol)
    (form/form ctx ::var-metadata)))


  ;;
  ;; NOTE: spec forms
  ;;


(defmethod form/form [::s/form :fm/spec]
  [ctx [_ spec-tag :as tag]]
  (when-let [data (get-in ctx [::metadata spec-tag (signature-index ctx)])]
    (let [[t _] (lib/conform-throw! ::metadata-spec-form data)
          ctx   (if (= t ::metadata-specv-form) (assoc ctx spec-tag data) data)]
      (form/form ctx (conj tag t)))))

(defmethod form/form [::s/form :fm/spec ::s/registry-keyword] [k _] k)
(defmethod form/form [::s/form :fm/spec ::metadata-fn-form] [form _] form) ; NOTE: may require `s/spec` in spec2
(defmethod form/form [::s/form :fm/spec ::metadata-specv-form]
  [ctx [_ spec-tag _ :as tag]]
  (let [specv       (get ctx spec-tag)
        [context _] (lib/conform-throw! ::metadata-specv-form specv)
        ctx         (case context
                      :fm.context/positional specv
                      :fm.context/nominal    ctx)]
    (form/form ctx (conj tag context))))

(defmethod form/form [::s/form :fm/spec ::metadata-specv-form
                      :fm.context/positional]
  [specv tag]
  (let [parts      (partition-by (hash-set '&) specv)
        var?       (> (count parts) 1)
        var-only?  (= (count parts) 2)
        params     (when (not var-only?) (form/forms (first parts) (conj tag :params)))
        var-params (when var? (form/forms (first (last parts)) (conj tag :var-params)))]
    `(s/cat ~@params ~@var-params)))

(defmethod form/form [::s/form :fm/spec ::metadata-specv-form
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

(defmethod form/form [::s/form :fm/args :fm/sequent]
  [ctx tag]
  (form/form ctx (conj tag (signature-tag ctx))))

(defmethod form/form [::s/form :fm/args :fm/sequent :fm.signature/singular]
  [ctx [_ spec-tag _ _]]
  (form/form ctx [::s/form spec-tag]))

(defmethod form/form [::s/form :fm/args :fm/sequent :fm.signature/plural]
  [ctx [_ spec-tag _ _]]
  (let [tag-fn  (comp keyword str)
        form-fn (fn [index]
                  (let [ctx (assoc ctx ::signature-index index)]
                    (or
                     (form/form ctx [::s/form spec-tag])
                     (let [arglist (get-in ctx [::normalized-arglists index])
                           anys    (mapv-any? arglist)
                           ctx     (assoc ctx spec-tag anys)
                           tag     [::s/form spec-tag ::metadata-specv-form]]
                       (form/form ctx tag)))))
        tagged  (mapcat
                 (juxt tag-fn form-fn)
                 (range (count-signatures ctx)))]
    `(s/or ~@tagged))) ; ALT: `fm../sum` spec-op; `s/or-of` spec2 wiki example

(defmethod form/form [::s/form :fm/spec :fm/sequent]
  [ctx [_ spec-tag _ _]]
  (form/form ctx [::s/form spec-tag]))


   ;;;
   ;;; NOTE: `form/forms` implementations
   ;;;


(defmethod form/forms ::definition
  [ctx tag]
  (if (sequent? ctx)
    (form/forms ctx [tag :fm/sequent])
    (form/forms ctx [tag (signature-tag ctx)])))

(defmethod form/forms [::definition :fm.signature/singular]
  [ctx _]
  (let [norm  (get-in ctx [::normalized-arglists 0])
        body  (form/form ctx ::body)
        forms (list norm body)]
    forms))

(defmethod form/forms [::definition :fm.signature/plural]
  [ctx _]
  (map
   (fn [index]
     (let [norm (get-in ctx [::normalized-arglists index])
           ctx  (assoc ctx ::signature-index index)
           body (form/form ctx ::body)]
       (list norm body)))
   (range
    (count-signatures ctx))))

(defmethod form/forms [::definition :fm/sequent]
  [ctx _]
  (let [ctx   (form/bind ctx [::argxs])
        argxs (form/form ctx [::form/binding ::form/fn ::argxs])
        body  (form/form ctx [::body :fm/sequent])]
    `([& ~argxs] ~body)))

(defmethod form/forms [::metadata :fm/arglist ::form/conformed-param-list
                       :fm.context/positional :params]
  [params _]
  (map arglist-metadata params))

(defmethod form/forms [::metadata :fm/arglist ::form/conformed-param-list
                       :fm.context/positional :var-params]
  [var-params _]
  (let [var-form (arglist-metadata (get var-params :var-form))]
    `(& ~var-form)))

(defmethod form/forms [::metadata :fm/spec ::form/conformed-param-list
                       :params]
  [params _]
  (mapcat (comp lib/ensure-sequential args-metadata) params))

(defmethod form/forms [::metadata :fm/spec ::form/conformed-param-list
                       :var-params]
  [var-params _]
  (let [var-form (args-metadata (get var-params :var-form))]
    `(& ~var-form)))

(defmethod form/forms [::trace :fm.binding/default]
  [ctx [_ form-tag]]
  (let [ident (form/form ctx [::metadata :fm/ident])
        trace (form/form ctx [::form/binding ::form/fn :fm/trace])
        form  (form/form ctx [::form/binding ::form/fn form-tag])
        data  (hash-map :fm/ident ident form-tag form)
        form  (list trace data)
        forms (list form)]
    forms))

(defmethod form/forms [::trace :fm.binding/default ::s/conformed]
  [ctx [_ form-tag _]]
  (let [ident (form/form ctx [::metadata :fm/ident])
        trace (form/form ctx [::form/binding ::form/fn :fm/trace])
        form  (form/form ctx [::form/binding ::form/fn form-tag ::s/conformed])
        data  (hash-map :fm/ident ident form-tag form)
        form  (list trace data)
        forms (list form)]
    forms))

(defmethod form/forms [::s/form :fm/spec ::metadata-specv-form
                       :fm.context/positional :params]
  [params _]
  (let [ixf (fn [i param] (vector (keyword (str i)) param))
        xf  (comp (map-indexed ixf) cat)]
    (into (vector) xf params))) ; ALT: (map-indexed (evolve [f identity]))

(defmethod form/forms [::s/form :fm/spec ::metadata-specv-form
                       :fm.context/positional :var-params]
  [var-params _]
  (if (= var-params `any?)
    `(:& (s/* ~var-params))
    `(:& ~var-params)))


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
       ::conformed-signatures
       ::signature-contexts
       ::signature-index
       ::metadata
       ::outer-metadata
       ::inner-metadatas])))

  ;;;
  )
