(ns fm.form
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [fm.anomaly :as anomaly]
   [fm.form.fn :as fn]
   [fm.form.lib :as lib]))

  ;; TODO: refactor into `context`
(def ^:dynamic *positional-argument-prefix-symbol*
  `positional-argument)

(def ^:dynamic *variadic-argument-prefix-symbol*
  `variadic-argument)

(def ^:dynamic *signature-prefix-symbol*
  `signature)

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

(s/def ::bindings
  #_{:fm/k1 [{::symbol ,,, ::form}]
     :fm/k2 [{::symbol ,,, ::form}]})

(s/def ::context
  (s/keys
   :opt
   [::ns
    ::definition
    ::metadata
    ::bindings
    ::fn/conformed-definition
    ::fn/outer-metadata
    ::fn/inner-metadatas]))

'^{:fm/args} (^{:fm/args} [a] a)
'([::a
   {::b b
    c   ::c
    ::d [d1 d2 d3 :as ds]
    ::e {:keys [ek1 ek2 ek3] :as ekv}}
   :as args]
  [::f ::g ::h])
'([[::a
    {::b b
     c   ::c
     ::d [d1 d2 d3 :as ds]
     ::e {:keys [ek1 ek2 ek3] :as ekv}}
    :as args]]
  [[::f ::g ::h]])

(fm.form/->form ::fn/body {})

(defmulti  <<conformed-definition ::ident)
(defmethod <<conformed-definition ::fn
  [{::keys [definition] :as ctx}]
  (assoc
   ctx
   ::fn/conformed-definition
   (lib/conthrow ::fn/definition definition)))

#_(defmethod <<conformed-definition :default
    [{::keys [definition] :as ctx}]
    (assoc
     ctx
     ::conformed-definition
     (lib/conthrow ::definition definition)))

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
     (get-in ctx [::fn/conformed-definition ::fn/simple-symbol??])
     (gensym 'fm)))))

#_(defmethod ->ident :default
    [ctx]
    (keyword
     (str (get ctx ::ns))
     (name
      (or
       (get-in ctx [::conformed-definition ::lib/simple-symbol??])
       (get-in ctx)
       (gensym 'fm)))))

(def ->symbol
  (comp symbol ->ident))

(def ->simple-symbol
  (comp symbol name ->ident))

(defmulti  ->default-metadata-form (fn [ctx k] :default #_[(->definition-tag ctx) k]))
(defmethod ->default-metadata-form :default
  [ctx k]
  (let [outer  (get-in ctx [::fn/outer-metadata k])
        inners (map k (get ctx ::fn/inner-metadatas))
        form   (cond
                 (every? nil? inners) outer
                 (nil? outer)         (apply list inners) ; NOTE: at least one
                 :else                (apply list (cons outer inners)))]
    form))

(defmulti  ->metadata-form (fn [_ctx tag] tag))
(defmethod ->metadata-form :default ; NOTE: keep all metadata
  [ctx tag]
  (->default-metadata-form ctx tag))

(s/def ::metadata
  (s/keys
   :req
   [:fm/ident            ; :some.ns1/fm1
    :fm/arglists]        ; '([a])
   :opt
   [:fm/doc              ; '(",,," nil ",,,") | ""
    :fm/args             ; '([int?])
    :fm/ret              ; '(int?)
    :fm/rel              ; '(#function[,,,]) '???
    :fm/trace            ; '(true) | '(#{,,,})
    :fm/conform          ; '(true) | '(#{,,,})
    :fm.anomaly/handler  ; '(#function[,,,]) '???
    :fm.anomaly/handler? ; '(true)
    ]))

(defmethod ->metadata-form :fm/ident
  [ctx _]
  (->ident ctx))

(defmethod ->metadata-form :fm/arglists
  [ctx tag]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form ctx tag)))

(defmethod ->metadata-form [:fm/arglists ::fn/signature]
  [ctx _]
  (let [signature (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argv      (get signature ::fn/argv)]
    (list argv)))

(defmethod ->metadata-form [:fm/arglists ::fn/signatures]
  [ctx _]
  (let [signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argvs      (map ::fn/argv signatures)]
    (apply list argvs))) ; FIXME: `(apply list ~@argvs) ?

(comment

  (def params1
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a] {:fm/args '[int?]})
                   'a)})

  (def params2
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                   '[a b c ds])})

  (def params3
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fm1"})
                   (list
                    (with-meta '[a] {:fm/args '[int?]})
                    'a)
                   (list
                    (with-meta '[a b] {:fm/args '[int? int?]})
                    '[a b]))})

  (def params4
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (list
                    (with-meta '[a [b [c]]] {:fm/args '[int? [int? [int?]]]})
                    '[a b c])
                   (list
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds]))})

  (def params5
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? int?]})
                   (list '[a] 'a)
                   (list '[a b] '[a b]))})

  (def params6
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list '[a] 'a)
                   (list '[a & bs] '[a bs]))})

  (->metadata-form
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a] {:fm/args '[int?]})
                  'a)}
   :fm/ident)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a] {:fm/args '[int?]})
                   'a)}
    <<conformed-definition)
   :fm/ident)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a] {:fm/args '[int?]})
                   'a)}
    <<conformed-definition)
   :fm/arglists)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                   '[a b c ds])}
    <<conformed-definition)
   :fm/arglists)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list '[a] 'a)
                   (list '[a & bs] '[a bs]))}
    <<conformed-definition)
   :fm/arglists)

  (type *1)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                   '[a b c ds])}
    <<conformed-definition
    <<metadata)
   :fm/args)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list '[a] 'a)
                   (list '[a & bs] '[a bs])
                   (list
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds]))}
    <<conformed-definition
    <<metadata)
   :fm/args)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list
                    (with-meta '[a] {:fm/doc "sig1" :fm/args '[even?]})
                    'a)
                   (list '[a & bs] '[a bs])
                   (list
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds]))}
    <<conformed-definition
    <<metadata)
   :fm/args)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds])
                   (list
                    (with-meta '[a] {:fm/doc "sig1" :fm/args '[even?]})
                    'a)
                   (list '[a & bs] '[a bs]))}
    <<conformed-definition
    <<metadata)
   :fm/args)

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds])
                   (list
                    (with-meta '[a] {:fm/doc "sig1" :fm/args '[even?]})
                    'a)
                   (list '[a & bs] '[a bs]))}
    <<conformed-definition
    <<metadata)
   :fm/doc)

  ;;;
  )

(defmethod ->metadata-form :fm/args
  [ctx tag]
  (let [tag [tag (->definition-tag ctx)]]
    (->metadata-form ctx tag)))

(defmethod ->metadata-form [:fm/args ::fn/signature]
  [ctx _]
  (let [args (get-in ctx [::fn/outer-metadata :fm/args]) ; ALT: `::fn/metadata`
        _    (lib/conthrow ::fn/args args)]
    (list args)))

(defmethod ->metadata-form [:fm/args ::fn/signatures]
  [ctx _]
  (let [outer      (get-in ctx [::fn/outer-metadata :fm/args])
        inners     (map :fm/args (get ctx ::fn/inner-metadatas))
        _          (map
                    (partial lib/conthrow ::fn/args)
                    (remove nil? (cons outer inners)))
        signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        argvs      (map ::fn/argv signatures)
        args       (map-indexed
                    (fn [i inner]
                      (let [argv (nth argvs i)]
                        (if-let [args (or inner outer)]
                          (fn/zipv-args argv args)
                          (fn/zipv-args argv)))) ; TODO: warn
                    inners)]
    (apply list args)))

(defmethod ->metadata-form :fm/ret)
(defmethod ->metadata-form :fm/rel)
(defmethod ->metadata-form :fm/trace)
(defmethod ->metadata-form :fm/conform)
(defmethod ->metadata-form :fm.anomaly/handler)
(defmethod ->metadata-form :fm.anomaly/handler?)

(defmethod ->metadata-form [:fm/arglists ::sequent/signature]
  [ctx tag]
  (let [tag (conj tag (->sequent-context-tag ctx))]
    (->metadata-form ctx tag)))
(defmethod ->metadata-form [:fm/arglists ::sequent/signature ::sequent/positional] [ctx _]) #_[::a ::b] #_-> #_([a b])
(defmethod ->metadata-form [:fm/arglists ::sequent/signature ::sequent/associative] [ctx _]) #_[[::a ::b]] #_-> #_([{a ::a b ::b}])
(defmethod ->metadata-form [:fm/arglists ::sequent/signatures] [ctx _]) ; TODO: delegate
(defmethod ->metadata-form [:fm/arglists ::sequent/signatures ::sequent/positional] [ctx _])
(defmethod ->metadata-form [:fm/arglists ::sequent/signatures ::sequent/associative] [ctx _])

(defmethod ->arglists ::fn
  [{::fn/keys [definition]}]
  (case (first (::fn/rest definition))
    ::fn/signature  (list (::fn/argv (second (::fn/rest definition))))
    ::fn/signatures (apply list (map ::fn/argv (second (::fn/rest definition))))))

  ;; TODO: warn on `:fm/args`/`::fn/argv` incompat
(defmethod ->metadata-form [::fn/signature :fm/args]
  [ctx k]
  (let [form (list (get-in ctx [::fn/outer-metadata k]))
        _    (lib/conthrow ::fn/args form)]
    form))

(defmethod ->metadata-form [::fn/signatures :fm/args]
  [ctx k]
  (let [outer  (get-in ctx [::fn/outer-metadata k])
        inners (map k (get ctx [::fn/inner-metadatas]))
        _      (map (partial lib/conthrow ::fn/args) (cons outer inners))
        argvs  (map ::fn/argv (get-in ctx [::fn/conformed-definition ::fn/rest 1]))
        forms  (map-indexed
                (fn [i inner]
                  (let [argv (nth argvs i)]
                    (if-let [args (or inner outer)]
                      (fn/zipv-args argv args)
                      (fn/zipv-args argv)))) ; TODO: warn
                inners)
        form   (apply list forms)]
    form))

(defmulti  ->metadata ->definition-tag)
(defmethod ->metadata ::fn/signature
  [{::fn/keys [conformed-definition] :as ctx}]
  (let [outer-meta (merge
                    (meta (::fn/simple-symbol? conformed-definition))
                    (meta (::fn/argv (second (::fn/rest conformed-definition)))))
        ctx        (assoc ctx ::fn/outer-metadata outer-meta)
        #_#_
        meta-keys  (into [:fm/ident :fm/arglists] (keys outer-meta))]
    #_
    (into
     (hash-map)
     (map (juxt identity (partial ->metadata-form ctx)))
     meta-keys)
    ctx))

(defmethod ->metadata ::fn/signatures
  [{::fn/keys [conformed-definition]
    ::keys    [definition]
    :as       ctx}]
  (let [signatures  (if (symbol? (first definition)) (next definition) definition)
        outer-meta  (merge
                     (meta (first signatures))
                     (meta (::fn/simple-symbol?? conformed-definition)))
        inner-metas (map
                     (comp meta ::fn/argv)
                     (second (::fn/rest conformed-definition)))
        ctx         (assoc ctx ::fn/outer-metadata outer-meta ::fn/inner-metadatas inner-metas)
        #_#_
        meta-keys   (into
                     [:fm/ident :fm/arglists]
                     (mapcat keys)
                     (cons outer-meta inner-metas))]
    #_
    (into
     (hash-map)
     (map (juxt identity (partial ->metadata-form ctx)))
     meta-keys)
    ctx))

(defmulti  <<metadata ::ident)
(defmethod <<metadata ::fn
  [ctx]
  (->metadata ctx)
  #_
  (assoc
   ctx
   ::metadata
   (->metadata ctx)))

#_(defn <<bindings
    [{::keys [metadata] :as ctx}]
    (assoc
     ctx
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
(defmethod ->context ::fn
  [parameters]
  (->>
   parameters
   <<conformed-definition
   <<metadata
   <<bindings))

(defmulti  ->form (fn [tag _ctx] tag))
(defmethod ->form ::fn
  [_ ctx]
  (let [bindings   (->forms ::fn/context-bindings ctx)
        sym        (->form ::fn/symbol ctx)
        definition (->form ::fn/definition ctx)
        metadata   (->form ::fn/metadata ctx)]
    `(let [~@bindings]
       (with-meta
         (fn ~sym ~definition)
         ~metadata))))

(defmulti  ->forms (fn [tag _ctx] tag))
(defmethod ->forms ::fn/context-bindings
  [_ ctx]
  (->>
   (get ctx ::bindings) ; TODO: warn on conflict
   (vals)
   (flatten)
   (distinct)
   (mapcat (juxt ::symbol ::form))))

(defmethod ->form ::fn/symbol
  [_ ctx]
  (symbol (name (get-in ctx [::metadata :fm/ident]))))

(defmethod ->form ::fn/definition
  [tag ctx]
  (let [tag [tag (->definition-tag ctx)]]
    (->form tag ctx)))

(defmethod ->form [::fn/definition ::fn/signature]
  [_ ctx]
  (let [index     (or (get ctx ::signature-index) 0)
        signature (or
                   (get ctx ::fn/signature)
                   (get-in ctx [::fn/conformed-definition ::fn/rest 1]))
        argv      (get signature ::fn/argv)
        ctx       (->
                   ctx
                   (update
                    ::bindings
                    (fn [bindings]
                      (into
                       (hash-map)
                       (map
                        (juxt
                         (comp i ::forms)
                         (comp i ::symbols))) bindings)))
                   (update [::metadata] (fn [metadata])))
        body      (->form ::fn/body ctx)]
    #_(update ctx ::bindings (comp (map i) vals))
    `(~argv ~body)))

(defmethod ->form [::fn/definition ::fn/signatures]
  [_ ctx]
  (let [signatures (get-in ctx [::fn/conformed-definition ::fn/rest 1])
        forms      (map-indexed
                    (fn [i signature]
                      (let [tag [::fn/definition ::fn/signature]
                            ctx (into ctx {::fn/signature signature ::signature-index i})]
                        (->form tag ctx)))
                    signatures)]
    (apply list forms)))

(defmethod ->form ::fn/metadata
  [_ ctx]
  (into
   (hash-map)
   (map
    (fn [[k forms]]
      (map-indexed
       (fn [i form]
         (if-let [sym (get-in ctx [::bindings k i ::symbol])]
           [k sym]
           [k form]))
       forms)))
   (get ctx ::metadata)))

(defmethod ->form ::fn/body
  [_ ctx]
  (let [handler (get-in ctx [::bindings ::anomaly/handler ::symbol])
        try     (->form ::fn/try ctx)]
    `(let [res# ~try]
       (if (anomaly/anomalous? res#)
         (~handler res#)
         res#))))

(defmethod ->form ::fn/try
  [_ ctx]
  (let [trace `(~trace-sym #:fm.trace{:sym '~sym :args ~args})]
    `(try
       ~@(when trace? [~trace])
       ~nested-form
       (catch Throwable throw#
         #::anomaly{:spec ::anomaly/throw
                    :sym  '~sym
                    :args ~args
                    :data throw#}))))

(defmethod ->form ::sequent/body [_ ctx])

(defn ->context-binding-form
  [context]
  `context-binding-form)

(defn fm
  [parameters]
  (let [context (->context (into parameters {::ident ::fn}))
        form    (->context-binding-form context)]
    form))

(defmulti  ->arglists ::ident)
(defmethod ->arglists ::fn
  [{::fn/keys [definition]}]
  (case (first (::fn/rest definition))
    ::fn/signature  (list (::fn/argv (second (::fn/rest definition))))
    ::fn/signatures (apply list (map ::fn/argv (second (::fn/rest definition))))))

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
  (let [context (->context (into parameters {::ident ::fn}))
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

  ;; NOTE: cut elimination, runtime `*ignore*`

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
  ;; supplied in the definition, i.e. either in `::fm/outer-metadata` or in
  ;; at least one of the `::fm/inner-metadatas`. that implies that the
  ;; resulting `s/or` spec will contain at least one relevant specification, and
  ;; that it may be necessary to fill in unspecified signatures using `any?`—
  ;; i.e. trigger the `:else` clause.
  ;;    in other words, triggering the `:else` clause implies that at least one
  ;; signature is specified w.r.t. its arguments, _and_ that at least one
  ;; signature is _un_specified w.r.t. its arguments. the design intent is that
  ;; either all signatures are specified at once in `::fm/outer-metadata`,
  ;; or that each signature is individually specified in its respective
  ;; `::fm/inner-metadata`. mixing and matching these approaches and leaving
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
                     (second outer-args?) (let [args (get-in context [::fm/outer-metadata :fm/args])
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

  (defmacro)

  ;;;
  )
