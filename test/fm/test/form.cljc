(comment

  (lib/zipv
   vector?
   '[a b & [c d & [e f & gs]]]
   '[int? int? & [int? int? & [int? int? & int?]]])

  (lib/zipv
   vector?
   '[a [b1 b2 b3 :as bs] & [c d & [e f & gs]]]
   '[int? (s/spec (s/* int?)) & [int? int? & [int? int? & int?]]])

  (lib/zipv
   vector?
   '[a bs & [c d & [e f & gs]]]
   '[int? [int? int? int?] & [int? int? & [int? int? & int?]]])

  (lib/zipv
   vector?
   '[a [b1 b2 b3] & [c d & [e f & gs]]]
   '[int? [int? int? int?] & [int? int? & [int? int? & int?]]])

  (lib/zipv
   vector?
   '[a [b1 b2 b3 & bs] & [c d & [e f & gs]]]
   '[int? [int? int? int? & int?] & [int? int? & [int? int? & int?]]])

  (lib/zipv
   vector?
   '[a [b1 b2 b3] & cs]
   '[int? [int? int? int?] & int?])

  (lib/zipv
   vector?
   '[a [b1 b2 b3] & cs]
   '[int? [int? int? int?] & int?])

  (lib/conform-explain
   (s/cat
    :0 int?
    :1 int?
    :&
    (s/?
     (s/cat
      :0 int?
      :& (s/* int?))))
   '[1 2 a b])

  (lib/conform-explain
   (s/cat
    :0 int?
    :1 int?
    :&
    (s/spec
     (s/?
      (s/cat
       :0 int?
       :rest
       (s/?
        (s/cat
         :1 int?
         :rest
         (s/?
          (s/cat
           :2 int?
           :rest (s/? any?)))))))))
   '[1 2 (3 4 5 b)])

  (lib/zipv
   vector?
   '[a b & [c d e & [f g & hs]]]
   '[int? [int? int? int?] & [::s1 (fn [a] (pos? a)) [int? int? int? & int?] & [(s/and pos? even?) int? & int?]]])

  (let [[a [b & cs :as cs2] & ds :as ds2] '[a [b c d] e f g]]
    [a b cs ds cs2 ds2])

  ((fn [a [b & cs :as cs2] & ds]
     [a b cs ds cs2])
   'a '[b c d] 'e 'f 'g)

  ((fn [a b & [c d & [e f & gs]]]
     [a b c d e f gs])
   'a '[b1 b2 b3] 'c 'd 'e)

  ((fn [a b & [c d & [e f & gs :as es] :as cs]]
     [a b c d e f gs es cs])
   'a 'b '[c1 c2 c3] 'd 'e 'f)

  ((fn [a b & [& [c d & es :as cs] :as rest]]
     [a b c d es cs rest])
   'a 'b '[c1 c2 c3] 'd 'e 'f)

  ((fn [a [b & bs] & [& [c d & es :as cs] :as rest]]
     [a b bs c d es cs rest])
   'a '[b1 b2 b3] '[c1 c2 c3] 'd 'e 'f)

  ((fn [a [& bs :as bs2] & [& [c d & es :as cs] :as rest]]
     [a bs bs2 c d es cs rest])
   'a '[b1 b2 b3] '[c1 c2 c3] 'd 'e 'f)

  (fn [a [& bs :as bs2] & [& [c d & es :as cs] :as rest] & f g]
    [a bs bs2 c d es cs rest])

  (lib/conform-explain
   ::fn/args
   '[a [& bs :as bs2] & [& [c d & es :as cs] :as rest]])

  '[int? [& int?] & [& [int? int? & int?]]]
  '[int? [& int?] & [int? [& int?] & int?]]
  '[a [b1 b2 b3] c [d1 d2 d3 d4] e f g]

   (lib/zipv
    sequential?
    '[a [& bs] & [c [& ds] & es]]
    '[a? [& b?] & [c? [& d?] & e?]])

   '[a (b1 b2 b3) c (d1 d2 d3 d4) e1 e2 e3]

   (lib/zipv
    sequential?
    '[a [& bs :as bs2] & [c [& ds :as ds2] & es :as cs2]]
    '[a? [& b?] & [c? [& d?] & e?]])

   (lib/conform-explain
    ::args
    '[a? [& b?] & [c? [& d?] & e?]])

   (lib/zipv
    sequential?
    '[a [& bs :as bs2] & [c [& ds :as ds2] & es :as cs2]]
    '[a? [& b?] & [c? [& d?] & e?]])

  '[a bs2 cs2]

  (lib/conform-explain
   ::arg
   :ns/kw)

  (lib/conform-explain
   ::arg
   [:ns/kw])

  (lib/conform-explain
   ::arg
   '[int? [& int?] & [& [int? int? & int?]]])

  (lib/conform-explain
   ::args
   '[int? [& int?] & [& [int? int? & int?]]])

  (lib/conform-explain
   ::args
   '[& int?])

  (lib/conform-explain
   ::args
   '[int? & int?])

  ((fn [& {:syms [a b c]}]
     [a b c])
   'a 'a 'b 'b 'c 'c)

  ((fn [& {:strs [a b c]}]
     [a b c])
   "a" 'a "b" 'b "c" 'c)

  '[a? [& b?] & [c? [& d?] & e?]]
  (s/spec
   (s/cat
    :p1 a?
    :p2
    (s/spec
     (s/cat
      :variadic
      (s/* b?)
      #_
      (s/?
       (s/* b?))))
    :variadic
    (s/?
       ;; NOTE: no `s/spec`
     (s/cat
      )
     )
    )
   )

  (comment

    ;; TODO: dynamic `:fm/,,,`
    (def args1 [int?])
    (fm ^{:fm/args args1} [x] (+ x 1))

    ;; TODO: warnings (log level?), `conform-explain`
    (fm ^{:fm/args [int?]} [x1 x2])

    ;;;
    )

  (reset! trace-atom [])

  (->form
   {::ns         *ns*
    ::definition '(^{:fm/doc "fn1"}
                   fn1
                   ^{:fm/args    [int?]
                     :fm/ret     int?
                     :fm/rel     (fn [{args :args ret :ret}]
                                   (>= ret (apply + (vals args))))
                     :fm/trace   true
                     :fm/conform true
                     :fm/handler identity}
                   [a :as argv]
                   (prn argv)
                   (inc a))
    ::defaults   {:fm/trace    nil
                  :fm/trace-fn `prn
                  :fm/handler  `identity}}
   ::fn)

  (->form
   {::ns         *ns*
    ::definition '(#_^{:fm/doc "fn1"}
                     fn1
                     #_^{:fm/rel     (fn [{args :args ret :ret}]
                                       (>= ret (apply + (vals args))))
                         :fm/trace   true
                         :fm/conform true
                         :fm/handler identity}
                     [[::a :as ctx]]
                     (prn argv)
                     (inc a))
    ::defaults   {:fm/trace    nil
                  :fm/trace-fn `prn
                  :fm/handler  `identity}}
   ::conse)

  (lib/conform-explain
   ::specv
   [[:a]])

  (lib/conform-explain
   ::core.specs/param-list
   '[a [b :as bs] & xs])

  (lib/conform-explain
   ::specv
   '[a])

  (lib/conform-explain
   ::definition
   '([a b] [a b]))

  (lib/conform-explain
   ::definition
   '([a b & cs] [a b cs]))

  (lib/conform-explain
   ::definition
   '([a ::b & cs] [a b cs]))

  (s/def ::b any?)

  (lib/conform-explain
   ::definition
   '([a ::b & cs] [a b cs]))

  (s/def ::cs (s/keys* :opt-un [::a ::b]))

  (lib/conform-explain
   ::definition
   '(^:fm/conform [a ::b & ::cs] [a b cs]))

  (lib/conform-explain
   ::definition
   '(([a] a)
     ([a ::b] [a b])
     ([a ::b & ::cs] [a b cs])))

  (def conformed-definition1
    #:fm.definition
    {:rest
     [:fm.form/signatures
      [#:fm.signature
       {:argv
        [:fm.context/positional
         {:params
          [[:clojure.core.specs.alpha/binding-form
            '[:local-symbol a]]]}],
        :body '[a]}
       #:fm.signature
       {:argv
        [:fm.context/positional
         {:params
          [[:clojure.core.specs.alpha/binding-form
            '[:local-symbol a]]
           [:clojure.spec.alpha/registry-keyword
            :fm.form/b]]}],
        :body '[[a b]]}
       #:fm.signature
       {:argv
        [:fm.context/positional
         {:params
          [[:clojure.core.specs.alpha/binding-form
            '[:local-symbol a]]
           [:clojure.spec.alpha/registry-keyword
            :fm.form/b]],
          :var-params
          {:ampersand '&,
           :var-form
           [:clojure.spec.alpha/registry-keyword
            :fm.form/cs]}}],
        :body '[[a b cs]]}]]})

  (def conformed-param-list1
    [:fm.context/positional
     {:params
      [[:clojure.core.specs.alpha/binding-form
        '[:local-symbol a]]
       [:clojure.spec.alpha/registry-keyword
        :fm.form/b]],
      :var-params
      {:ampersand '&,
       :var-form
       [:clojure.spec.alpha/registry-keyword
        :fm.form/cs]}
      :as-form {:as :as :as-sym 'specv1}}])

  (->metadata conformed-param-list1 ::conformed-param-list)

  (s/def ::a any?)
  (s/def ::as any?)
  (s/def ::b any?)
  (s/def ::bs any?)
  (s/def ::c any?)
  (s/def ::cs any?)

  (->metadata
   (lib/conform-explain
    ::specv
    '[])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[a])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[::a])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[::a b])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[::a b & cs])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[{::as [a1 a2 a3]} & ::cs])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[::a b & {::cs [{[[c111 c112 v113] c12 c13] :xs} c2 [x1 x2 x3]]}])
   [:fm/arglist ::conformed-specv])

  (lib/conform-explain
   ::specv
   '[[]])

  (->metadata
   (lib/conform-explain
    ::specv
    '[[::b {:a [a1 a2 a3] ::c c1 ::d {:keys [d1 d2 d3]}}]])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[[]])
   [:fm/arglist ::conformed-specv])

  (->metadata
   (lib/conform-explain
    ::specv
    '[[::a ::b {::c {:keys [k1 k2 k3]}}]])
   [:fm/arglist ::conformed-specv])

  (let [[{a :fm.form/a, b :fm.form/b, {:keys [k1 k2 k3], :as c} :fm.form/c}]]
    {::a 'a ::b 'b ::c {:k1 'k1 :k2 'k2 :k3 'k3}}
    [a b c k1 k2 k3])

  (->metadata
   (lib/conform-explain
    ::specv
    '[::a ::b & {::cs {:keys [k1 k2 k3]}}])
   [:fm/args ::conformed-specv])

  (s/unform
   ::specv
   '[:fm.context/nominal
     [{:params
       [[:keyword :fm.form/b]
        [:fm.form/nominal-binding-map
         {:a
          [:seq-destructure
           {:forms [[:local-symbol a1] [:local-symbol a2] [:local-symbol a3]]}],
          :fm.form/c [:local-symbol c1],
          :fm.form/d [:map-destructure {:keys [d1 d2 d3]}]}]]}]])

  (s/unform
   ::core.specs/binding-form
   '[:seq-destructure
     {:forms
      [[:local-symbol c1] [:local-symbol c2] [:local-symbol c3]],
      :as-form {:as :as, :as-sym cs}}])

  (s/unform
   ::core.specs/binding-form
   '[:map-destructure {:keys [d1 d2 d3] :as ds}])

  (s/unform
   ::core.specs/binding-form
   '[:local-symbol c1]
   #_
   '[:map-destructure {:keys [d1 d2 d3]}]
   #_
   '[:seq-destructure
     {:forms [[:local-symbol a1] [:local-symbol a2] [:local-symbol a3]]}])

  (let [conformed (lib/conform-explain
                   ::definition
                   '(([a] a)
                     ([a b] [a b])
                     ([a b & cs] [a b cs])))
        ctx       (assoc ctx ::ident ::fn ::conformed-definition conformed)
        ctx       (assoc ctx ::metadata (->metadata ctx ::metadata))
        ])

  (defn f1
    [& argms]
    (,,, (into (hash-map) argms)))

  ([] ,,,)
  ([] [] ,,,)
  ([] [[]] ,,,)
  ([[]] ,,,)
  ([[]] [] ,,,)
  ([[]] [[]] ,,,)

  ;; merges: meta-merge, deep merge, specter?
  (into (hash-map) argms)
  (into (vector) (mapcat (fn [x] (sequential? x) x (vector x))) args)
  (into (vector) args)
  (into (vector) (mapcat identity) argvs)
  (partial apply meta-merge)
  (partial apply deep-merge)
  (partial apply zip)
  (->> (select-keys ret right) (partial into args)) ; "closed"

  (derive :fm/-- :fm.sequent/nonse)
  (derive :fm/-> :fm.sequent/conse)
  (derive :fm/<- :fm.sequent/conse)
  (derive :fm/<< :fm.sequent/merge)
  (derive :fm/>> :fm.sequent/merge)
  (derive :fm/<> :fm.sequent/iso)

  (derive :fm.sequent/combine :fm.sequent/merge)

  (fm/defn f1
    [a]
    a)

  (fm/defn m1 [[::a ::b]]
    ::c)

  (defn f1 [a] (inc a)) ; b
  (fm/defn f1 ^:fm/throw [a] (inc a))
  #_(fm/defn f1 ^:fm/ignore [a] (inc a))

  (defn f2 [a]
    (try
     (if (s/valid? int? a)
       (inc a)
       'args-anomaly)
     (catch Throwable t
       'thrown-anomaly)))
  (fm/defn f2 ^{:fm/args [int?]} [a] (inc a))

  (s/def ::a int?)

  (fm/defn f2 [::a] (inc a))
  (fm/defn f1 ^{:fm/args [::a]} [a] (inc a)) ; b
  (fm/defn f1 ^{:fm/args [::a]} [a :as argv] (prn argv) (inc a)) ; b
  (fm/defn f1 [::a] (inc a)) ; b
  (fm/defn f1 ^:fm/-> [::a] (inc a)) ; [b]
  (fm/defn f1 ^:fm/-- [::a] (inc a)) ; [a]
  (fm/defn f1 ^:fm/<< [::a] (inc a)) ; [a b]
  (fm/defn f1 ^:fm/<> [::a] [::b] (inc a), (dec b)) ; [a] | [b]

  (fm/defn f1 [a] [::b] (inc a))

  (fm/defn m1
    ^{:fm/sequent
      {:fm.sequent/ident   :fm.sequent/merge
       :fm.sequent/unit    (partial into [] ,,,)
       :fm.sequent/combine (partial into [] ,,,)}}
    [:a :b :c]
    [:d :e :f]
    [d e f])

  (fm/defn ^:fm/<< m1
    ^{:fm/sequent {:fm.sequent/combine (partial apply meta-merge)}}
    [[:a :b :c]]
    [[:d :e :f]]
    [d e f])

  (m1 [:a] [:b] [:c])

  (fm/defn ^:fm/-- m2
    [])

  (fm/defconse conse1 [[::a ::b ::c]] [[::d]]
    ::d)

  (fm/defn f1 [x y z :as v3]
    v3)

  (lib/conform-explain
   (s/cat
    :fm.form/argv vector?
    :fm.form/retv (s/? vector?)
    :fm.form/body (s/+ any?))
   '([a b c] []))

    ;; NOTE: seqv, argv ambiguity
  (fm/fn [{::keys [a b c]}] ; recognized as sequent by default
    ,,,)

  (fm/fn [[::a ::b ::c]] ; unambiguous `seqv` expression
    ,,,)

  (s/def ::xs ,,,)
  (fm/fn [{::xs {::keys [a b c]}}] ; semantically equivalent `seqv`
    ,,,)

    ;; NOTE: warning + explicit disambiguation
  (fm/fn ^:fm.form/argv
    [{::keys [a b c]}]
    ,,,)

    ;; NOTE: warning + don't handle at all; "[{::keys [a b c]}] is a seqv"
  (fm/fn [{::xs {::keys [a b c] :as xs}}]
    ,,,)

  ;; NOTE: eliminate `seqv` destructuring
  ;; NOTE: change `seqv` specification
  ;; NOTE: try to detect/infer special `:keys` cases


  [[a b c]] :: ::keys

  (defmergesequent req<<developer
    [::db.developer/conn ::session/data]
    [::portal/developer]
    (let [ref    (credential.datalog/nuid->lookup-ref (:nu/id data))
          result (d/q q/credential->developer (d/db conn) ref)]
      (datalog.lib/->data (ffirst result))))

  #_(seqv->pull-pattern)
  #_(seqv->argv)
  (fm/defn ^:fm.sequent/merge merge-developer
    [::db.developer/conn {::session/data {:nu/keys [id]}}] ; TODO: seqv -> pull-pattern
    [::portal/developer]
    (let [ref    (credential.datalog/nuid->lookup-ref id)
          result (d/q q/credential->developer (d/db conn) ref)]
      (datalog.lib/<-ident (ffirst result))))

  (fm/defn ^:fm/<< merge-developer
    [[::db.developer/conn {::session/data {:nu/keys [id]}}]]
    [[::portal/developer]]
    (let [ref    (credential.datalog/nuid->lookup-ref id)
          result (d/q q/credential->developer (d/db conn) ref)]
      (datalog.lib/->data (ffirst result))))

  (fm/defn ^:fm/<< merge-developer
    ([[::db.developer/conn {::session/data {:nu/keys [id]}}]]
     [[::portal/developer]]
     (let [ref    (credential.datalog/nuid->lookup-ref id)
           result (d/q q/credential->developer (d/db conn) ref)]
       (datalog.lib/->data (ffirst result))))
    ([[::db.developer/conn :nu/id]]
     [[::portal/developer]]
     (let [ref    (credential.datalog/nuid->lookup-ref id)
           result (d/q q/credential->developer (d/db conn) ref)]
       (datalog.lib/->data (ffirst result)))))

  (fm/defn ^:fm.sequent/nonse transact!
    [:body-params ::db.developer/conn ::portal/developer]
    [::d/tx-report]
    (let [eid           (get developer :db/id)
          subscriptions (get body-params ::portal/subscriptions)
          tx-data       [{:db/id eid ::portal/subscriptions subscriptions}]]
      (d/transact conn {:tx-data tx-data})))

  (fm/defconse req->post-response ^{:fm/handler anomaly-handler}
    []
    [[:status]]
    {:status 204}) ; ALT: respond with developer; EQL

  (defmulti  handler :request-method)
  (defmethod handler :post
    [req]
    (->>
     req
     req<<developer
     req<transact!>
     req->post-response))

  (s/def ::req
    (s/keys
     :req [::session/data]
     :req-un [::request-method]))

  (defmulti handler
    (fm/fn ^{:fm/args ::req}
      [{:keys [request-method] ::session/keys [data]}]
      (let [session-tag (first (lib/conform-throw ::session/data data))]
        [request-method session-tag]))
    :hierarchy #'h1)

  (defmulti handler
    (fn [{:keys [request-method] ::session/keys [data]}]
      [request-method (first (fm/conform-throw ::session/data data))]))

  (defmethod handler [:post ::session/authenticated]
    [req]
    ())

  (defmethod handler :default
    [_req]
    {:status 405})

  (fm/defnonse nonse1
    ([[::db.developer/conn ::b]]
     [::c]
     ::c)
    ([::c]
     [::d]
     ::d))

  (fm/defnonse nonse1
    ([::authenticated]
     [::resp]
     {:status 200})
    ([::unauthenticated]
     [::resp]
     {:status 401}))

  (ns ns1.core)

  (derive :fm.sequent/conse :fm.sequent/ident)
  (derive :fm.sequent/nonse :fm.sequent/ident)
  (derive :fm.sequent/merge :fm.sequent/ident)

  (defmulti multi1
    (fn [metadata]
      (let [descs (descendants :fm.sequent/ident)
            tag   (reduce
                   (fn [acc k]
                     (when-let [v (get metadata k)]
                       (reduced (if (true? v) k v))))
                   nil
                   (conj descs :fm.sequent/ident))]
        tag)))

  (defmethod multi1 :fm.sequent/conse
    [metadata]
    :fm.sequent/conse)

  (defmethod multi1 :fm.sequent/nonse
    [metadata]
    :fm.sequent/nonse)

  (defmethod multi1 :fm.sequent/merge
    [metadata]
    :fm.sequent/merge)

  (defmacro macro1 [& definition]
    (multi1 (meta (first definition))))

  (ns ns2.core
    (:require [ns1.core :as ns1]))

  (derive ::-> :fm.sequent/conse)
  (derive ::-- :fm.sequent/nonse)
  (derive ::<< :fm.sequent/merge)

  (descendants :fm.sequent/ident)

  (ns1/macro ^::-> [])

  (->form
   {::ns *ns*
    ::definition
    '(^{:fm/doc "fn1"}
      [a [b1 & [b2]] & [c & ds]]
      (apply + a b1 b2 c ds))}
   ::fn)

  (s/def ::a any?)
  (s/def ::as any?)
  (s/def ::b any?)
  (s/def ::bs any?)
  (s/def ::c any?)
  (s/def ::cs any?)

  (->form
   {::ns *ns*
    ::definition
    '(^{:fm/doc "fn1"}
      [::a {::b [{:keys [k1 k2 k3]} & [b2]]} & [c & ds]]
      (apply + a b1 b2 c ds))}
   ::fn)

  (->form
   {::ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ([::a]
       (inc a))
      ([::a ::b]
       (inc (+ a b))))}
   ::fn)

  (->form
   {::ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ([::a]
       (inc a))
      (^:fm/throw! [x ::b]
       (inc (+ x b))))}
   ::fn)

  (s/def ::x any?)

  (->form
   {::ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      (^{:fm/ret ::x}
       [::a :as argv]
       (inc a))
      (^{:fm/rel   (fn [{[a b] :args [x] :ret}] (>= x (+ a b)))
         :fm/trace false}
       [::a b]
       [::x]
       [(inc (+ a b))])
      ([[::a :b ::c :as x]]
       [[::x]]
       {::x (inc (+ a b c))}))}
   ::fn)

  (s/conform ::specv [[::a]])

  '[int? int? & [int? int? int?]]
  (let [[a b & [c d e]] '[a b [c d e] [d e] [e]]]
    [a b c d e])

  (let [[a b & [c d e :as cs]] '[a b]]
    [a b cs])

  (lib/conform-explain
   (s/tuple int? int? (s/? (s/cat :c int? :d int? :e int?)))
   '[1 2 nil])

  (let [[a b & [c d e :as cs]] '[a b nil]]
    [a b cs])

  (lib/conform-explain
   (s/tuple int? int? (s/? (s/cat :c int? :d int? :e int?)))
   '[1 2 [nil]]) ; NOTE: should fail

  (let [[a b & [c d e :as cs]] '[a b c]]
    [a b cs])

  (lib/conform-explain
   (s/tuple int? int? (s/? (s/cat :c int? :d int? :e int?)))
   '[1 2 [3]]) ; NOTE: shouldn't fail?

  (lib/conform-explain
   (s/tuple int? int? (s/cat :c int? :d int? :e int?))
   '[1 2 [3 4 5]])

  (lib/conform-explain
   (s/tuple int? int? (s/? (s/cat :c int? :d int? :e int?)))
   '[1 2 []])

  (lib/conform-explain
   (s/tuple int? int? (s/? (s/cat :c int? :d int? :e int?)))
   '[1 2 [3 4 5]])

  (let [[a b & [c d e :as cs]] '[a b c d]]
    [a b c d e cs])

  (lib/conform-explain
   (s/tuple int? int? (s/? (s/cat :c int? :rest (s/? (s/cat :d int? :rest (s/? (s/cat :e int? :rest any?)))))))
   '[1 2 [3 4 5 6]]) ; NOTE: shouldn't fail?

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest any?)))))))
   '[1 2 nil])

  (lib/conform-explain
   (s/cat
    :0 int?
    :1 int?
    :&
    (s/?
     (s/cat
      :0 int?
      :rest
      (s/?
       (s/cat
        :1 int?
        :rest
        (s/?
         (s/cat
          :2 int?
          :rest any?)))))))
   '[1 2 3])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest any?)))))))
   '[1 2 []])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest any?)))))))
   '[1 2 ()])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest any?)))))))
   '[1 2 (3)])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest any?)))))))
   '[1 2 (3 4)])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest (s/? any?))))))))
   '[1 2 (3 4 5)])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :0 int?
      :rest
      (s/?
       (s/cat
        :1 int?
        :rest
        (s/?
         (s/cat
          :2 int?
          :rest
          (s/? any?))))))))
   '[1 2 (3 4 5 6)])

  (lib/conform-explain
   (s/tuple
    int?
    int?
    (s/?
     (s/cat
      :c int?
      :rest
      (s/?
       (s/cat
        :d int?
        :rest
        (s/?
         (s/cat
          :e int?
          :rest
          (s/? any?))))))))
   '[1 2 (3 4 5 a)])

  (sequential?)
  (::fn/arg+ ,,,) #_=> `(s/? (s/cat ,,,))
  (::fn/arg ,,,) #_=> `(s/* ,,,)

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? int? & [int? int? int?]]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a b & [c d e :as cs]]
       (apply + a b cs))}))

  (let [[a b & [c d e & [f g]]] '[a b c d e f g]]
    [a b c d e f g])

  ((fn [a b & [c d e & [f g :as fs] :as cs]]
     [a b c cs d e f g fs])
   'a 'b 'c 'd 'e 'f 'g)

  ((fn [a b & [c d {:keys [e]} & [f g :as fs] :as cs]]
     [a b c cs d e f g fs])
   'a 'b 'c 'd '{:e e} 'f 'g)

  ((fn [a b & [c d {:keys [e]} & [f g :as fs] :as cs]]
     [a b c cs d e f g fs])
   'a)

  ((fn [a b & [c d {:keys [e]} & [f g :as fs] :as cs]]
     [a b c cs d e f g fs])
   'a nil)

  '[int? int? & [int? int? ::m1 & int?]]

  (def trace-idents #{:fm/args :fm/ret})
  (def conform-idents #{:fm/args})

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? int?]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       ([a b] (+ a b)))}))

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? int?]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a b] (+ a b))}))

  ((fn [a b & [c d e]] [a b c d e]) 'a 'b 'c 'd)

  (def context2 *1)
  (lib/conform-explain (eval (get-in context2 [::metadata :fm/args])) [1])
  (lib/conform-explain (eval (get-in context2 [::metadata :fm/args])) [1 [2]])
  (lib/conform-explain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]]])
  (lib/conform-explain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]] {:d 'd}])
  (lib/conform-explain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]] {:d 'd} :f 'f])
  (lib/conform-explain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]] {:d 'd} :f 'f :g 'g])

  (s/def ::m1 (s/map-of keyword? any?))

  (def args1 '[int? [int? [int?]] ::m1 & int?])
  (def argv1 '[a [b [c :as cs] :as bs] {:keys [d] :as e} & fs])

  (def conformed-args1 (lib/conform-explain ::fn/args args1))

  (def bindings1
    {:fm/k1 [{::symbol 's1 ::form 'f1}
             {::symbol 's2 ::form 'f2}
             {::symbol 's3 ::form 'f3}]
     :fm/k2 [{::symbol 's3 ::form 'f3}
             {::symbol 's3 ::form 'f3}
             {::symbol 's4 ::form 'f4}]})

  (->form
   ::fn/context-bindings
   {::bindings bindings1})

  ;;;
  )

(comment

  (ns ns1.core)

  (defn f1 [d1]
    (prn d1)
    (when d1 (prn "tracing"))
    ::f1)

  (ns ns2.core
    (:require [ns1.core :as ns1]))

  (def ^:dynamic *trace* false)

  (defmacro f [& definition]
    `(def ~(gensym 'f) ~*trace*))

  (f sym [] [])

  (binding [*trace* true]
    (f sym [] [])) ; NOTE: not sure why this doesn't work

  (alter-var-root #'*trace* (constantly true))

  (f sym [] [])

  (alter-var-root #'*trace* (constantly false))

  (ns ns3.core
    (:require
     [ns2.core :as ns2]))

  (ns2/f sym [] [])

  (ns ns4.core
    (:require
     [ns2.core :as ns2]
     [ns3.core :as ns3]))

  (ns2/f sym [] [])

  (alter-var-root #'ns2/*trace* (constantly true))

  (ns2/f sym [] [])

  ;;;
  )

(comment

  (lib/conform-explain
   ::argv
   '[a b c & ds :as asv]
   #_'[:as argv]
   #_'[ctx tag]
   #_'[[::a ::b ::c :as xs]]
   #_'[{::keys [a b c] :as xs} :as as]
   )

  (lib/conform-explain ::argv '[{:keys [a b c]}])
  (fm/fn [& as :as argv] )
  (fm/fn [:as argv])

  (fm/fn '(::a ::b ::c)
    )


  (defm -wrap-session ^{:fm/args    [router.lib/fn? ::opts]
                        :fm/handler -anomaly-handler}
    [handler opts]
    (fn [{:keys [uri] :as req}]
      (let [route (router/match-route uri)
            req   (into req #::session{:handler handler :opts opts :route route})]
        (session/req->res req))))

  (fn/defn -wrap-session ^{:fm/handler -anomaly-handler}
    [::handler ::opts]
    (fn [{:keys [uri] :as req}]
      (let [route (router/match-route uri)
            req   (into req #::session{:handler handler :opts opts :route route})]
        (session/req->res req))))

  (fn/defn -wrap-session ^{:fm/handler -anomaly-handler}
    [{::router.lib/fn handler} ::opts]
    (fn [{:keys [uri] :as req}]
      (let [route (router/match-route uri)
            req   (into req #::session{:handler handler :opts opts :route route})]
        (session/req->res req))))

  [handler opts]
  [::handler opts]
  [{::handler h} _opts]
  [{::router.lib/fn handler} opts]
  [{::router.lib/keys [a b c]} opts] ; potentially confusing
  [{::router.lib/keys [a b c] :as handler} opts]
  [{::handler {::router.lib/keys [a b c]}} opts]
  [{::router.lib/fn handler} opts]
  [{::router.lib/fn [h1 h2 h3]} opts]
  [{::router.lib/fn [h1 h2 h3]} opts]
  [{::router.lib/fn [h1 h2 h3]} ::opts]
  [{::router.lib/fn [h1 h2 h3]} {::opts {:keys [k1 k2 k3]}}]
  [{::router.lib/fn [h1 h2 h3]} {::opts {:keys [k1 k2 k3]}} & xs]
  [{::router.lib/fn [h1 h2 h3]} {::opts {:keys [k1 k2 k3]}} & ::xs]
  [{::router.lib/fn [h1 h2 h3]} {::opts {:keys [k1 k2 k3]}} & {::xs [x y z]}]

  [[:a {::b {:nu/keys [id]} ::c [c1 c2 c3]}]]

  (fn/defn ^:fm/throw! -wrap-session
    [{:as k}] #_=> [k] #_=> 
    [{:keys [k]}] #_=> [[k :as keys]]
    [{:keys [k] :as ks}] #_=> [{:keys [k] :as ks}]
    (fn [{:keys [uri] :as req}]
      (let [route (router/match-route uri)
            req   (into req #::session{:handler handler :opts opts :route route})]
        (session/req->res req))))

  [::a _b ::c & ::ds :as xs]

  [_]
  [_a]
  [::a]
  [{::a ,,,}]

  (fm/defn ^:fm/-- f1
    ([[::a ::b]]
     [[::c]]
     (inc a b))
    ([[::d ::e]]
     [[::f]]
     )
    )

  [[:a]]
  [[::a]]
  [[{:a ,,,}]]
  [[{::a ,,,}]]
  [[{,,, :a}]]
  [[{,,, ::a}]]

  (s/def ::email->recently-delivered?_args
    (s/keys
     :req [::db.developer/conn ::email/hash-key ::email/debounce-inst]))

  (defm email->recently-delivered? ^{:fm/args ::email->recently-delivered?_args}
    [{::db.developer/keys [conn]
      ::email/keys        [debounce-inst hash-key]}]
    (-> q/email->recently-delivered-count
        (d/q (d/db conn) hash-key debounce-inst)
        (ffirst)
        (or 0)
        (> 0)))

  (fm/defn email->recently-delivered?
    [[::db.developer/conn ::email/hash-key ::email/debounce-inst]]
    (-> q/email->recently-delivered-count
        (d/q (d/db conn) hash-key debounce-inst)
        (ffirst)
        (or 0)
        (> 0)))

  )
