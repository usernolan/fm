(comment

  (s/def ::a (s/and int? odd?))
  (s/def ::b (s/and int? even?))
  (s/def ::c (s/or ::a ::a ::b ::b))
  (s/def ::cs (s/* ::c))
  (s/def ::cs? (s/? ::cs))
  (s/def ::ds (s/cat ::a ::a ::b ::b ::cs? ::cs?))

  (lib/conform-explain
   ::definition
   '([a ::b & cs] [a b cs]))

  (lib/conform-explain
   ::definition
   '([a ::b & ::cs] [a b cs]))

  (lib/conform-explain
   ::definition
   '(([a] a)
     ([a ::b] [a b])
     ([a ::b & ::cs] [a b cs])))

  (def defaults-atom
    (atom
     {:fm/trace    nil
      :fm/trace-fn prn
      :fm/handler  identity}))

    ;; TODO: eliminate redundant argv binding
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([a] (inc a))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([::a] (inc a))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(([a] (inc a))
      ([a b] (inc (+ a b))))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(([::a] (inc a))
      ([::a ::b] (inc (+ a b))))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(([::a] (inc a))
      ([a ::b] (inc (+ a b))))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/conform [::c :as argv] (prn argv) (inc c))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^{:fm/args [int?] :fm/ret int?} [a] 'a)}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([:fm/anomaly] (anomaly/geta anomaly ::anomaly/ident))}
   ::form/fn)

    ;; NOTE: explicit anomaly cases; typically baked into args, ret, rel
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([::c] (if (= (first (str c)) \1)
              (inc c)
              {::anomaly/ident :integer/does-not-start-with-1
               :integer/value  c}))}
   ::form/fn)

  (def h1 prn)

    ;; NOTE: no outer `let`
    ;; TODO: eliminate redundant args binding
    ;; TODO: warn unbound `h1`; explicate defaults
    ;; ALT: phased analysis, rewrite
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^{:fm/handler h1} [a] (inc a))}
   ::form/fn)

  (def h2 (constantly :bummer))

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '((^{:fm/handler h1} [a] (inc a))
      (^{:fm/handler h2} [a b] (inc (+ a b))))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([::a [b c]] (+ a b c))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([& ::cs] (apply + cs))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '((^:fm/trace [::a] (inc a))
      ([::a ::b] (inc (+ a b))))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '((^{:fm/ret ::c}
       [::a]
       (inc a))
      (^{:fm/rel (fn [{[a b] :args [c] :ret}] (< (+ a b) c))}
       [::a ::b]
       [::c]
       [(dec (- a b))]))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([[::a]] (inc a))}
   ::form/fn)

    ;; NOTE: irreconcilable nominal context ambiguity
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(([[::a]] (inc a))
      ([[::a ::b]] (inc (+ a b)))
      ([[::a ::b ::c]] (inc (+ a b c))))}
   ::form/fn)

    ;; NOTE: locally unhandleable arity exception
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([::a] (inc a))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent [::a] [::b] [(inc a)])}
   ::form/fn)

    ;; NOTE: locally-recoverable arity anomalies
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent
      ([::a] [::b] [(inc a)])
      ([::a ::b] [::c] [(inc (+ a b))]))}
   ::form/fn)

    ;; NOTE: plural args contexts in sequent considered invalid
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent
      ([::a] (inc a))
      ([[::a]] (inc a)))}
   ::form/fn)

    ;; TODO: capture default
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent
      ([::a] [::b] [(inc a)])
      (^{:fm/handler h1} [::b] [::a] [(inc b)]))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent ^{:fm/ret [::c]}
      ([::a] [(inc a)])
      ([::a ::b] [(inc (+ a b))]))}
   ::form/fn)

  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent [[::a]] [[::b]] {::b (inc a)})}
   ::form/fn)

    ;; NOTE: locally deduplicated spec forms
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent
      ([[::a]] [[::b]] {::b (inc a)})
      ([[::b]] [[::a]] {::a (dec b)}))}
   ::form/fn) ; TODO: fix (f1 1)

    ;; NOTE: nominal context disambiguation; most specific first
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^:fm/sequent
      ([[::a ::b ::c]] [[::ds]] {::ds [a b c (inc (+ a b))]})
      ([[::a ::b]] [[::c]] {::c (inc (+ a b))})
      ([[::a]] [[::b]] {::b (inc a)}))}
   ::form/fn)

    ;; NOTE: explicit `s/keys` validates all qualified keys
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '(^{:fm/args [(s/keys :req [::a ::b])]} [m] m)}
   ::form/fn)

    ;; NOTE: nominal specification only validates relevant keys
  (form/form
   {::form/ns  *ns*
    ::defaults @defaults-atom
    ::definition
    '([[::a ::b :as m]] m)}
   ::form/fn)

  ;;;
  )

(comment

  (require '[clojure.spec.alpha :as s])
  (require '[fm.core :as fm])

  (swap! fm/defaults-atom assoc :fm/handler identity)

  (macroexpand '(fm/defn f1 [::a] (inc a)))

  (fm/defn f2 [::a] (inc a))

  (s/def ::ks1
    (fm/keys :req [::a ::b]))

  (s/valid? ::ks1 {::a 1 ::b 2 ::c 'a})

  (s/def ::or1
    (fm/or ::a ::b))

  (s/conform ::or1 2)

  ;;;
  )
