(ns fm.form.proto)

#_
(cartesian
 #{:formal :function :failure :free :frequency}
 #{:method :mode :map :merge :meta :metadata :markup :meaning :medium
   :multi :monad :monoid :modulation :modulus}
 #{:language :logic})

#_
(consequent
 mergesequent
 insequent
 isoquent
 sequent
 match
 ? multiquent
 ? gensequent
 )

(require '[clojure.spec.alpha :as s])
(require '[fm.sequent.macro :refer [defmergesequent defconsequent]])

(s/def ::a1 #{::a1})
(s/def ::a2 #{::a2})
(s/def ::a3 #{::a3})

(s/def ::A
  (s/or
   ::a1 ::a1
   ::a2 ::a2
   ::a3 ::a3))

(s/def ::b1 #{::b1})
(s/def ::b2 #{::b2})

(s/def ::B
  (s/or
   ::b1 ::b1
   ::b2 ::b2))

(s/def ::c1 #{::c1})
(s/def ::c2 #{::c2})
(s/def ::c3 #{::c3})
(s/def ::c4 #{::c4})
(s/def ::c5 #{::c5})
(s/def ::c6 #{::c6})

(s/def ::C
  (s/or
   ::c1 ::c1
   ::c2 ::c2
   ::c3 ::c3
   ::c4 ::c4
   ::c5 ::c5
   ::c6 ::c6))

(s/def ::d1 #{::d1})
(s/def ::d2 #{::d2})
(s/def ::d3 #{::d3})
(s/def ::d4 #{::d4})
(s/def ::d5 #{::d5})
(s/def ::d6 #{::d6})

(s/def ::D
  (s/or
   ::d1 ::d1
   ::d2 ::d2
   ::d3 ::d3
   ::d4 ::d4
   ::d5 ::d5
   ::d6 ::d6))

^{:fm.sequent/left  [::A ::B]
  :fm.sequent/right [::C]}
(defmultiquent AB->C
  [::A ::B]
  [::a1 ::b1] a1b1->c1
  [::a1 ::b2] a1b2->c2
  [::a2 ::b1] a2b1->c3
  [::a2 ::b2] a2b2->c4
  [::a3 ::b1] a3b1->c5
  [::a3 ::b2] a3b2->c6
  [::C])

{::A ::a1
 ::B ::b1}
#_=>
{::C ::c1}

^{:fm.sequent/left  #{[::a1 ::b1] ,,,}
  :fm.sequent/right [::C]}
(defmultiquent AB->C
  [::a1 ::b1] a1b1->c1
  [::a1 ::b2] a1b2->c2
  [::a2 ::b1] a2b1->c3
  [::a2 ::b2] a2b2->c4
  [::a3 ::b1] a3b1->c5
  [::a3 ::b2] a3b2->c6
  [::C])

{::a1 ::a1
 ::b1 ::b1}
#_=>
{::C ::c1}

^{:fm.sequent/left  [::C]
  :fm.sequent/right {[::c1] [::d1] ,,,}}
(defmultiquent C->D
  [::C]
  [::c1] c1->d1
  [::c2] c2->d2
  [::c3] c3->d3
  [::c4] c4->d4
  [::c5] c5->d5
  [::c6] c6->d6)

{::C ::c1}
#_=>
{::d1 ::d1}

^{:fm.sequent/left  [::A ::B]
  :fm.sequent/right {[::a1 ::b1] [::c1] ,,,}}
(defmultiquent AB->C
  [::A ::B]
  [::a1 ::b1] a1b1->c1
  [::a1 ::b2] a1b2->c2
  [::a2 ::b1] a2b1->c3
  [::a2 ::b2] a2b2->c4
  [::a3 ::b1] a3b1->c5
  [::a3 ::b2] a3b2->c6)

{::A ::a1
 ::B ::b1}
#_=>
{::c1 ::c1}

^{:fm.sequent/left  #{[::a1 ::b1] ,,,}
  :fm.sequent/right {[::a1 ::b1] [::c1] ,,,}}
(defmultiquent AB->C
  [::a1 ::b1] a1b1->c1
  [::a1 ::b2] a1b2->c2
  [::a2 ::b1] a2b1->c3
  [::a2 ::b2] a2b2->c4
  [::a3 ::b1] a3b1->c5
  [::a3 ::b2] a3b2->c6)

{::a1 ::a1
 ::b1 ::b1}
#_=>
{::c1 ::c1}

^{:fm.sequent/left  #{[::c1] ,,,}
  :fm.sequent/right {[::c1] ,,,}}
(defmultiquent C->D
  [::c1] c1->d1
  [::c2] c2->d2
  [::c3] c3->d3
  [::c4] c4->d4
  [::c5] c5->d5
  [::c6] c6->d6)

{::c1 ::c1}
#_=>
{::d1 ::d1}

#_
(defconsequent req->post-response ^{:fm/handler anomaly-handler}
  [::developer/hashed-email-address ::d/conn :as req]
  [:status]
  (if (registered? req)
    {:status 403}
    (registersequent req)))

#_
(defmultiquent anomaly-handler ^{:fm/accept-anomalies true
                                 :fm/handler ,,,}
  [::anomaly]
  [:status]
  [::http-400] {:status 400} ; => look for `:fm.sequent/right`
  [::http-422] {:status 422})

(defmultiquent C><D
  c1->d1
  c2->d2
  c3->d3 ,,,) ; fms

(defmultiquent C><D
  []    ; dispatch
  [] 'x ; 1
  [] 'y ; 2
  [] 'z ; 3
  )

(defmultiquent C><D
  []    ; dispatch
  a1->x
  a2->y
  a3->z)

(sequent
 a->b
 b->c
 c->d) ; fm-aware comp, how deep to go with compatibility checking?

fm?

^{:fm/ret int? :fm/rel 'rel? ,,,}
(let [x 'x] (throw (ex-info ,,,))) #_=> #_(try (let [#res (do ~body)] ,,,) ,,,)

^{:fm/args [,,,] :fm/ret int?}
(fn | comp | partial | ,,,) #_=> (,,,)
#_[&args] #_(apply ,,, args)

(fm/->>
 )

(fm->
 (update % :a :b)
 ,,,
 ) ; (fn [%] ,,,) if unbound

(sequent
 )

(multiquent
 )

(fm ^{:fm/args int? ;=> [int?] => (s/or ::nil #{[]} ::sig1 (s/tuple int?) ::sig2 )
      :fm/ret  [nil? int? vector?]} #_=> #_(s/or ::sign...)
    ([] nil)
    ([a] a)
    ([a b] [a b])
    ([a b & xs] (into [a b] xs)))

#_
{:fm/args (s/or ::_0 #{[]} ::_01 (s/tuple even?) ::_02 (s/tuple int? string?) ,,,)
 :fm/ret  (s/or ::_0 any?)
 }
(fm ^{:fm/args [int? string? [string? int?] & vector?]
      :fm/ret  [any? any? any? any?]} #_=> #_(s/or ::sign...)
    ([] nil)
    (^{:fm/args even?} [a] a)
    (^{:fm/args int?} [a b] [a b])
    (^{:fm/ret set?} [a b [c d]] (into #{} [c d]))
    ([a b [c d] & xs] (into [a b] xs)))

(fm ^{:fm/args [int? string?]
      :fm/ret  [nil? int? vector?]} #_=> #_(s/or ::sign...)
    ([] nil)
    ([a] a)
    ([a b] [a b]))

(fm ^{:fm/doc ",,,"}
    fm1
    ^{:fm/args [int? string? & map?]}
    (^{:fm/ret nil?} [])
    (^{:fm/ret int?} [a] a)
    (^{:fm/ret vector?} [a b] [a b])
    (^{:fm/ret vector?} [a b & xs] (into [a b] xs)))

(fm ^{:fm/doc ",,,"}
    fm1
    ^{:fm/args [int? string? & map?]} ; => s/or [int?] [int? string?] [int? string? & ,,,]
    (^{:fm/args [string?]
       :fm/ret} [a] a)
    (^{:fm/args [string? even?]
       :fm/ret} [a b] [a b])
    (^{:fm/ret} [a b & xs] (into [a b] xs)))

(defconsequent ->meta|metas
  [,,,]
  [(or
    [::-metadata]
    [::-outer-metadata
     ::-inner-metadatas])]
  ,,,
  (if p1?
    {::-metadata 'm}
    {::-outer-metadata  'om
     ::-inner-metadatas 'ims}))

(->
 context
 ->meta|metas
 (match
  -meta->form1
  -metas->form2)
 (branch
  x->a|b|c
  a->
  b->
  c->)
 (match
  form1->a
  form2->b)
 a|b<<c|d
 c|d->e
 ,,,)


#_(s/def ::defm-definition
    (s/cat
     ::sym symbol?
     ::definition
     (s/alt
      ::fn-signature  ::fn-signature
      ::fn-signatures (s/+ (s/spec ::fn-signature)))))

#_(s/def ::sequent-definition
    (s/cat
     ::sym (s/? symbol?)
     ::left vector?
     ::right vector?
     ::body (s/* any?)))

#_(s/def ::defsequent-definition
    (s/cat
     ::sym symbol?
     ::left vector?
     ::right vector?
     ::body (s/* any?)))

#_(s/def ::quent-definition
    (s/cat
     ::sym (s/? symbol?)
     ::left vector?
     ::right vector?
     ::forward any?
     ::reverse any?))

#_(s/def ::defquent-definition
    (s/cat
     ::sym symbol?
     ::left vector?
     ::right vector?
     ::forward any?
     ::reverse any?))

#_(s/def :fm.sequent/signature)
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

sequent | sequent-signature
(->>
 req
 ((fm/conse
   unauthsequent
   authsequent
   ([::a ::session/unauth]
    [::c]
    'ab->c)
   ([::a ::session/auth]
    [::d]
    'ac->d))))
