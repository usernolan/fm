(ns fm.form
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [fm.anomaly :as anomaly]
   [fm.form.fm :as fm]
   [fm.form.lib :as lib]))

  ;; TODO: refactor into `context`
(def ^:dynamic *positional-argument-prefix-symbol*
  `positional-argument)

(def ^:dynamic *variadic-argument-prefix-symbol*
  `variadic-argument)

(def ^:dynamic *positional-signature-prefix-symbol*
  `positional-signature)

(defmulti  <<definition ::ident)
(defmethod <<definition ::fm
  [{::keys [definition] :as context}]
  (assoc
   context
   ::fm/conformed-definition
   (lib/conthrow ::fm/definition definition)))

(defmulti  ->ident ::ident)
(defmethod ->ident ::fm
  [{::fm/keys [definition]
    ::keys    [ns]}]
  (keyword
   (str ns)
   (name
    (or
     (::fm/simple-symbol?? definition)
     (gensym 'fm)))))

(defmulti ->metadata-form (fn [_context [tag _form]] tag))
(defmulti ->form-data     (fn [_context [tag _form]] tag))

(defmulti  -signature-dispatch ::ident)
(defmethod -signature-dispatch ::fm
  [{::fm/keys [conformed-definition]}]
  (first (::fm/rest conformed-definition)))

(defn -metadata-form-kv-dispatch
  [context k]
  [(-signature-dispatch context) k])

(defmulti  ->metadata-form-kv -metadata-form-kv-dispatch)
(defmethod ->metadata-form-kv :default ; NOTE: keep all metadata
  [{::fm/keys [raw-metadata raw-outer-metadata raw-inner-metadatas] :as context} k]
  [k (case (-signature-dispatch context)
       ::fm/signature  (get raw-metadata k)
       ::fm/signatures (let [vs (map k (cons raw-outer-metadata raw-inner-metadatas))]
                         (if (= (count (remove nil? vs)) 1) (first (filter some? vs)) vs)))])

  ;; TODO: warn on `:fm/args`/`::fm/argv` incompat
(defmethod ->metadata-form-kv [::fm/signature :fm/args]
  [{::fm/keys [conformed-definition raw-metadata] :as context} k]
  (let [argv    (get-in conformed-definition [::fm/rest 1 ::fm/argv])
        context (assoc context ::fm/argv argv)
        args    (lib/conthrow ::fm/args (:fm/args raw-metadata))
        form    (->metadata-form context [::fm/args args])]
    [k form]))

(defmethod ->metadata-form-kv [::fm/signatures :fm/args]
  [{::fm/keys [conformed-definition raw-outer-metadata raw-inner-metadatas] :as context} k]
  (let [argv+        (map ::fm/argv (get-in conformed-definition [::fm/rest 1]))
        context      (assoc context ::fm/argv+ argv+)
        outer-args?  (:fm/args raw-outer-metadata)
        inner-args?+ (map :fm/args raw-inner-metadatas)
        args         (lib/conthrow ::fm/signatures-args [outer-args? inner-args?+])
        form         (->metadata-form context [::fm/signatures-args args])]
    [k form]))

(defmulti  ->metadata -signature-dispatch)
(defmethod ->metadata ::fm/signature
  [{::fm/keys [conformed-definition] :as context}]
  (let [ident    (->ident context)
        raw-meta (merge
                  {:fm/ident ident}
                  (meta (::fm/simple-symbol?? conformed-definition))
                  (meta (::fm/argv (second (::fm/rest conformed-definition)))))
        context  (assoc context ::fm/raw-metadata raw-meta)]
    (into
     (hash-map)
     (map (partial ->metadata-form-kv context))
     (keys raw-meta))))

(comment

  (defn rzip
    ([recur? & xs]
     (apply
      map
      (fn [& ys]
        (if (every? recur? ys)
          (apply rzip recur? ys)
          ys))
      xs)))

  (rzip
   vector?
   '[a [b [c]]]
   '[int? ::m1])

  (rzip
   vector?
   '[a [b [c]]]
   '[int? [:m1 [int?]]])

  (rzip
   vector?
   '[a [b [c] :as bs] & ds]
   '[int? [::m1 [int?]] & int?])

  (rzip
   vector?
   '[a [b]]
   '[int? [::m1 [int?]] & int?])

  ;;
  )

(defmethod ->metadata ::fm/signatures
  [{::fm/keys [conformed-definition]
    ::keys    [definition]
    :as       context}]
  (let [sym?            (symbol? (first definition))
        signatures      (if sym?
                          (next definition)
                          definition)
        ident           (->ident context)
        raw-outer-meta  (merge
                         {:fm/ident ident}
                         (meta (first signatures))
                         (meta (::fm/simple-symbol?? conformed-definition)))
        raw-inner-metas (map
                         (comp meta ::fm/argv)
                         (second (::fm/rest conformed-definition)))
        context         (assoc
                         context
                         ::fm/raw-outer-metadata  raw-outer-meta
                         ::fm/raw-inner-metadatas raw-inner-metas)
        meta-keys       (into
                         (hash-set)
                         (mapcat keys)
                         (cons raw-outer-meta raw-inner-metas))]
    (into
     (hash-map)
     (map (partial ->metadata-form-kv context))
     meta-keys)))

(defmulti  <<metadata ::ident)
(defmethod <<metadata ::fm
  [context]
  (assoc
   context
   ::metadata
   (->metadata context)))

#_(defn <<bindings
    [{::keys [metadata] :as context}]
    (assoc
     context
     ::bindings
     (into
      (hash-map)
      (map ->binding-kv)
      metadata)))

(s/def ::parameters
  (s/keys
   :req
   [::ns
    ::definition]))

  ;; ALT: `interpret`, `analyze`
(defmulti  ->context ::ident)
(defmethod ->context ::fm
  [parameters]
  (->>
   parameters
   <<definition
   <<metadata
   #_<<bindings))

(defn ->context-binding-form
  [context]
  `context-binding-form)

(defn fm
  [parameters]
  (let [context (->context (into parameters {::ident ::fm}))
        form    (->context-binding-form context)]
    form))

(defmulti  ->arglists ::ident)
(defmethod ->arglists ::fm
  [{::fm/keys [definition]}]
  (case (first (::fm/rest definition))
    ::fm/signature  (list (::fm/argv (second (::fm/rest definition))))
    ::fm/signatures (map ::fm/argv (second (::fm/rest definition)))))

(defmulti ->var-metadata-kv key)
(defmulti ->var-metadata-kv :fm/arglists)
(defmulti ->var-metadata-kv :fm/doc)
(defmulti ->var-metadata-kv :default [_] nil)

(defn ->var-metadata
  [{::keys [metadata] :as context}]
  (into
   (hash-map)
   (map ->var-metadata-kv)
   (into
    metadata
    {:fm/arglists (->arglists context)})))

(defn ->var-sym
  [{::keys [metadata] :as context}]
  (with-meta
    (symbol (name (:fm/ident metadata)))
    (->var-meta context)))

(defn defm
  [parameters]
  (let [context (->context (into parameters {::ident ::fm}))
        sym     (->var-sym context)]
    `(def ~sym ~(fm parameters))))

(s/def :fm/ident
  qualified-keyword?)

#_(s/def :fm/doc
    (s/or
     ::fm/signature  string?
     ::fm/signatures (s/coll-of string?)))

(s/def :fm/arglists
  (s/coll-of vector?))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident]
   :opt
   [:fm/doc              ; [docouter doc1 doc2 doc3] xs | x
    :fm/arglists         ; n/a
    :fm/args             ; (s/or ::fm/signature-01 (s/tuple ,,,) ::fm/signature-02 ,,,)
    :fm/ret              ; (s/or ::fm/signature-01 ,,, ::fm/signature-02 ,,,)
    :fm/rel              ; [(s/,,,) ,,,] => (s/,,,), xs | x
    :fm/trace            ; xs | x
    :fm/conform          ; xs | x
    :fm.anomaly/handler  ; [handler handler handler] => handler, xs | x
    :fm.anomaly/handler? ; => binding *ignore* (conj (:fm/ignore ,,,) :fm.anomaly/received?)
    :fm/sequent          ; ,,, ; ALT: :fm.sequent/ident
    :fm.sequent/left     ; ,,,
    :fm.sequent/right    ; ,,,
    :fm.sequent/nonse    ; ,,,
    :fm/ignore           ; ,,,
    ]))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident]
   :opt
   [:fm/doc              ; '(",,," nil ",,,") | ""
    :fm/arglists         ; '([a] [a b] [a b & cs]) | '([a])
    :fm/args             ; '([int? int? & int?]) | '([int? int? & int?] [pos?] nil nil) | '([int?])
                         ; '([int?] [int? int?] [int? int? & int?])
    :fm/ret              ; '(int? neg? even?)
    :fm/rel              ; '() ??? '(s/,,,)
    :fm/trace            ; '(true) | '(false true false)
    :fm/conform          ; '(true) | '(#{}) | '(#{} true false) | ()
    :fm.anomaly/handler  ; '(h) | '(h f1 f2) | '()
    :fm.anomaly/handler? ; => binding *ignore* (conj (:fm/ignore ,,,) :fm.anomaly/received?)
                         ;
    :fm/sequent          ; ,,, ; ALT: :fm.sequent/ident
    :fm.sequent/left     ; ,,,
    :fm.sequent/right    ; ,,,
    :fm.sequent/nonse    ; ,,,
    :fm/ignore           ; ,,,
    ]))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident]           ; :some.ns1/fm1
   :opt
   [:fm/doc              ; '(",,," nil ",,,") | ""
    :fm/arglists         ; '([a])
    :fm/args             ; '([int?])
    :fm/ret              ; '(int?)
    :fm/rel              ; '(#function[,,,]) '???
    :fm/trace            ; '(true) | '(#{,,,})
    :fm/conform          ; '(true) | '(#{,,,})
    :fm.anomaly/handler  ; '(#function[,,,]) '???
    :fm.anomaly/handler? ; '(true)
    ]))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident]           ; :some.ns1/fm1
   :opt
   [:fm/doc              ; '(",,," nil ",,,") | ""
    :fm/arglists         ; '([a])
    :fm/args             ; '([int?])
    :fm/ret              ; '([int?])
    :fm/rel              ; '(#function[,,,]) '???
    :fm/trace            ; '(true) | '(#{,,,})
    :fm/conform          ; '(true) | '(#{,,,})
    :fm.anomaly/handler  ; '(#function[,,,]) '???
    :fm.anomaly/handler? ; '(true)
    ;;
    :fm.sequent/ident    ; :fm.sequent/merge
    :fm.sequent/left     ; '([int?])
    :fm.sequent/right    ; '([int?])
    | :fm.sequent/nonse  ; ,,,
    ]))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident]           ; :some.ns1/fm1
   :opt
   [:fm/doc              ; '(",,," nil ",,,") | ""
    :fm/arglists         ; '([a] [a b] [a b & cs])
    :fm/args             ; '([int?] [int? int?] [int? int? & int?])
    :fm/ret              ; '([int?] [int?] [int?])
    :fm/rel              ; '(#function[,,,] #fun ,,,,) '???
    :fm/trace            ; '(true true true) | '(#{,,,})
    :fm/conform          ; '(true false true) | '(#{,,,})
    :fm.anomaly/handler  ; '(#function[,,,]) '???
    :fm.anomaly/handler? ; '(true)
    ]))

:fm.anomaly/handler? ; => binding *fm/ignore* (conj (:fm/ignore ,,,) :fm.anomaly/received?)

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident]
   :opt
   [:fm/doc              ; '(",,," nil ",,,") | ""
    :fm/arglists         ; '([a] [a b] [a b & cs]) | '([a])
    :fm/args             ; '([int? int? & int?]) | '([int? int? & int?] [pos?] nil nil) | '([int?])
                                        ; '([int?] [int? int?] [int? int? & int?])
    :fm/ret              ; '(int? neg? even?)
    :fm/rel              ; '() ??? '(s/,,,)
    :fm/trace            ; '(true) | '(false true false)
    :fm/conform          ; '(true) | '(#{}) | '(#{} true false) | ()
    :fm.anomaly/handler  ; '(h) | '(h f1 f2) | '()
    :fm.anomaly/handler? ; => binding *ignore* (conj (:fm/ignore ,,,) :fm.anomaly/received?)
    :fm/sequent          ; ,,, ; alt: :fm.sequent/ident
    :fm.sequent/left     ; ,,,
    :fm.sequent/right    ; ,,,
    :fm.sequent/nonse    ; ,,,
    :fm/ignore           ; ,,,
    ]))

#_(def -metadata-acc-init
    {:fm/doc              []
     :fm/args             []
     :fm/ret              []
     :fm/rel              []
     :fm/trace            []
     :fm/conform          []
     :fm.anomaly/handler  []
     :fm.anomaly/handler? []
     :fm/ignore           []})

(comment

  ;; TODO: dynamic `:fm/args`
  (def args1 [int?])
  (fm ^{:fm/args args1} [x] (+ x 1))

  ;; TODO: warnings (log level?), `conformsplain`
  (fm ^{:fm/args [int?]} [x1 x2])

  ;;;
  )

  ;; NOTE: mid-level helpers

(defmethod ->metadata-form ::fm/args
  [context [tag form]]
  (let [data (->form-data context [tag form])
        form (->metadata-form context [::cat-tree data])]
    form))

(defmethod ->metadata-form ::fm/signatures-args
  [context [tag form]]
  (let [data (->form-data context [tag form])
        form (->metadata-form context [::or-pairs data])]
    form))

  ;; NOTE: low-level helpers

(defmethod ->metadata-form ::lib/arg-symbol   [_context [_tag form]] form)
(defmethod ->metadata-form ::lib/fn-form      [_context [_tag form]] form)
(defmethod ->metadata-form ::lib/spec-form    [_context [_tag form]] form)
(defmethod ->metadata-form ::lib/spec-keyword [_context [_tag form]] form)

(defmethod ->metadata-form ::fm/arg
  [context [_tag tagged-form]]
  (->metadata-form context tagged-form))

(defmethod ->metadata-form ::fm/variadic-arg
  [context [_tag tagged-form]]
  (let [form (->metadata-form context tagged-form)]
    (case (first tagged-form)
      ::lib/sequence-spec-form form
      `(s/* ~form))))

  ;; TODO: `s/op` for arbitrary tags, map syntax
(defmethod ->metadata-form ::fm/keyword-args-map
  [context [_tag -keyword=>tagged-form]]
  `(s/alt
    ~@(interleave
       (map keyword (keys -keyword=>tagged-form)) ; NOTE: `::fm/-keyword`
       (map
        (fn [[k tagged-form]]
          `(s/cat
            ::k ~(hash-set k)
            ::v ~(->metadata-form context tagged-form)))
        -keyword=>tagged-form))))

(defmethod ->metadata-form ::cat-tree
  [context [_tag cat-tree]]
  `(s/cat
    ~@(mapcat
       (fn [[tag form]]
         (if (vector? form)
           [tag `(s/spec ~(->metadata-form context [::cat-tree form]))]
           [tag form]))
       cat-tree)))

(defmethod ->metadata-form ::or-pairs
  [context [_tag or-pairs]]
  (let [tags  (map first or-pairs)
        forms (map second or-pairs)]
    `(s/or
      ~@(interleave tags forms))))

  ;; NOTE: lowest-level helpers

(defmethod ->form-data ::fm/args
  [context [_tag {::fm/keys [arg* variadic?]}]]
  (let [positional-data (->form-data context [::fm/arg* arg*])
        variadic-data   (->form-data context [::fm/variadic? variadic?])]
    (into positional-data variadic-data)))

  ;; TODO: warn on `:else`, dedup cases?
  ;; NOTE: we only reach this function if at least one `:fm/args` key is
  ;; supplied in the definition, i.e. either in `::fm/raw-outer-metadata` or in
  ;; at least one of the `::fm/raw-inner-metadatas`. that implies that the
  ;; resulting `s/or` spec will contain at least one relevant specification, and
  ;; that it may be necessary to fill in unspecified signatures using `any?`—
  ;; i.e. trigger the `:else` clause.
  ;;    in other words, triggering the `:else` clause implies that at least one
  ;; signature is specified w.r.t. its arguments, _and_ that at least one
  ;; signature is _un_specified w.r.t. its arguments. the design intent is that
  ;; either all signatures are specified at once in `::fm/raw-outer-metadata`,
  ;; or that each signature is individually specified in its respective
  ;; `::fm/raw-inner-metadata`. mixing and matching these approaches and leaving
  ;; certain signatures unspecified is supported and likely matches expectations
  ;; w.r.t. specificity (inner takes precedence over outer), but a style warning
  ;; might be appropriate when `:else` is triggered.
  ;; NOTE: tags sort in definition order. sort order is unspecified for fns with
  ;; > 99 signatures; arbitrary `s/or` tags (ints) would be perfect here.
(defmethod ->form-data ::fm/signatures-args
  [context [_tag {::fm/keys [outer-args? inner-args?+]}]]
  (map-indexed
   (fn [i inner-args?]
     (let [tag     (keyword (str *positional-signature-prefix-symbol* (format "_%02d" i)))
           argv    (nth (get context ::fm/argv+) i)
           context (assoc context ::fm/argv argv)
           form    (cond
                     (second inner-args?) (->metadata-form context [::fm/args (second inner-args?)])
                     (second outer-args?) (let [args (get-in context [::fm/raw-outer-metadata :fm/args])
                                                args (take (count argv) args)
                                                args (lib/conthrow ::fm/args args)]
                                            (->metadata-form context [::fm/args args]))
                     :else                `any?)]
       [tag form]))
   inner-args?+))

  ;; NOTE: 9th-circle helpers

  ;; ALT: ->ast, ->tagged-spec-form-tree, ->cat-tree, ->catt
  ;; ALT: extract path accumulation; `rreduce-with-path`
(defmethod ->form-data ::fm/arg*
  [{::fm/keys [argv] :as context} [_tag form]]
  (let [acc->tag
        (fn acc->tag [{::keys [path]}]
          (keyword
           (or
            (fm/arg->sym (get-in argv path))
            (symbol
             (str
              *positional-argument-prefix-symbol* "_"
              (string/join "_" path))))))
        recur?
        (fn recur? [acc x]
          (and
           (vector? x)
           (case (first x)
             ::fm/arg+ (second x)
             false)))
        initf
        (fn initf [acc x]
          (->
           acc
           (update-in (into [::data] (butlast (::path acc))) conj [])
           (update-in [::path] conj 0)))
        rf
        (fn rf [acc x]
          (let [tag  (acc->tag acc)
                form (->metadata-form context x)
                data [tag form]]
            (->
             acc
             (update-in (into [::data] (butlast (::path acc))) conj data)
             (update-in [::path (dec (count (::path acc)))] inc))))
        cf
        (fn cf [acc r]
          (let [tag (acc->tag acc)
                r   (update-in
                     r
                     (into [::data] (::path acc))
                     (partial vector tag))]
            (->
             acc
             (assoc ::data (::data r))
             (update-in [::path (dec (count (::path acc)))] inc))))
        init {::path [0] ::data []}
        acc  (lib/rreduce recur? initf rf cf init form)
        data (::data acc)]
    data))

(defmethod ->form-data ::fm/variadic?
  [{::fm/keys [argv] :as context} [_tag variadic?]] ; ALT: `form` rename `variadic?`
  (when variadic?
    (let [tag  (keyword (or (fm/arg->sym (last argv)) *variadic-argument-prefix-symbol*))
          form (if (= ::fm/arg+ (get-in variadic? [::fm/variadic-arg 1 0])) ; ALT: `::fm/arg+` case in `::fm/variadic-arg`
                 (let [context (update context ::fm/argv last)
                       form    (get-in variadic? [::fm/variadic-arg 1 1])]
                   (->form-data context [::fm/arg* form]))
                 (->metadata-form context (find form ::fm/variadic-arg)))
          data [[tag form]]]
      data)))

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

  (defmacro )

  ;;;
  )
