(comment

  (s/def ::a int?)
  (s/def ::b int?)
  (s/def ::c int?)
  (s/def ::as any?)
  (s/def ::bs any?)
  (s/def ::cs (s/* int?))

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

    ;; TODO: eliminate redundant argv binding
  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm/throw! [a] (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '([a] (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '([::a] (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(([a] (inc a))
      ([a b] (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(([::a] (inc a))
      ([::a ::b] (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(([::a] (inc a))
      ([a ::b] (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(([::a] (inc a))
      ([a b] (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm/conform
      [::a :as argv]
      (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/ret int?}
      [::a]
      'a)
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '([:fm/anomaly]
      (anomaly/geta anomaly ::anomaly/ident))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (s/def ::anom1 :fm/anomaly)

  (form/form
   {::form/ns *ns*
    ::definition
    '([::anom1]
      (anomaly/geta anom1 ::anomaly/ident))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (def h1 prn)

    ;; NOTE: no outer `let`
    ;; TODO: eliminate redundant args binding
    ;; TODO: warn unbound `h1`; explicate defaults
    ;; ALT: phased analysis, rewrite
  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/handler h1}
      [a]
      (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/handler h1}
      ([::a]
       (inc a))
      ([a ::b]
       (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (def h2 (constantly :bummer))

  (form/form
   {::form/ns *ns*
    ::definition
    '((^{:fm/handler h1}
       [a]
       (inc a))
      (^{:fm/handler h2}
       [a b]
       (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; TODO: shrink, isolate test cases
    ;; TODO: eliminate redundancy in `args`
  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "fn1" :fm/throw! true}
      [a [b1 & [b2] :as bs] & [c & ds]]
      (apply + a b1 b2 c ds))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (comment ; NOTE:
    (apply + nil)
    (apply + 0 nil)
    (apply + nil nil)
    (apply + 0 nil nil))

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "fn1" :fm/handler (fn [a] a)}
      [::a {::bs [{:keys [k1 k2 k3]} & [b2]]} & cs :as xs]
      (apply + a k1 k2 k3 b2 cs))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '([& ::cs]
      (apply + 1 cs))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ([::a]
       (inc a))
      ([::a ::b]
       (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    true
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ([::a]
       (inc a))
      (^:fm/throw! [x ::b]
       (inc (+ x b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; TODO: analyze case where `::x` is in the registry in the following
    ;; `form/form` invocation
  (s/def ::x int?)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      (^{:fm/ret ::x}
       [::a ::b :as argv]
       (inc (+ a b)))
      (^:fm/throw!
       ^:fm/trace
       ^{:fm/rel (fn [{[a b c] :args [x] :ret}] (>= x (+ a b c)))}
       [::a b ::c]
       [::x]
       [(inc (- a b c))]) ; NOTE: rel
      (^{:fm/trace (fn [t] (prn :flavor t))
         :fm/rel   (fn [{a :args r :ret}] (prn a r) true)}
       [[::a :b ::c]]
       [[::x]]
       {::x (inc (+ a b c))}))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '([[::a]] (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: irreconcilable nominal context ambiguity
  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ([[::a]] (inc a))
      ([[::a ::b]] (inc (+ a b)))
      ([[::a ::b ::c]] (inc (+ a b c))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: locally unhandleable arity exception
  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ([::a] (inc a)))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; TODO: fallback ret spec
  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      [a]
      [b]
      [(inc a)])
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      [::a]
      [::b]
      [(inc a)])
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: arity immunity
  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      ([a]
       [b]
       [(inc a)])
      ([a b]
       [c]
       [(inc (+ a b))]))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      (^{:fm/handler h1}
       [::a]
       [b]
       [(inc a)])
      (^{:fm/handler h2}
       [a b]
       [c]
       [(inc (+ a b))]))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      (^:fm/throw!
       [a]
       [b]
       [(inc a)])
      (^{:fm/handler (fn [x] (prn x))}
       [a b]
       [c]
       [(inc (+ a b))]))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; TODO: capture default
  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      ([a]
       [b]
       [(inc a)])
      (^{:fm/handler (fn [x] (prn x))}
       [a b]
       [c]
       [(inc (+ a b))]))
    ::defaults
    {:fm/throw!   true
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: sequent metadata fallback/fill
  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      (^:fm.sequent/conse
       [::a]
       (inc a))
      ([::a ::b]
       (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      ^{:fm/doc "variadic increment"
        :fm/ret [::c]}
      ([::a]
       [(inc a)])
      ([::a ::b]
       [(inc (+ a b))]))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      ^:fm.sequent/conse
      ([::a] (inc a))
      ([::a ::b] (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      [[::a]]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/nonse
      [[::a]]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      [[::a]]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      [::a]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/nonse
      [::a]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      [::a]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      [[::a]]
      [::b]
      [(inc a)])
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      [::a]
      [::b]
      [(inc a)])
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      [[::a]]
      [[::b]]
      {::b (inc a)})
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: locally deduplicated spec forms
    ;; NOTE: essentially `:fm.sequent/iso`
  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      ([[::a]]
       [[::b]]
       {::b (inc a)})
      ([[::b]]
       [[::a]]
       {::a (dec b)}))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '((^:fm.sequent/conse
       [[::a]]
       [[::b]]
       {::b (inc a)})
      (^:fm.sequent/nonse
       [[::b]]
       [[::a]]
       {::a (dec b)}))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: nominal context disambiguation; most specific first
  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/merge
      ([[::a ::b ::c]]
       [[::d]]
       {::d (inc (+ a b c))})
      ([[::a ::b]]
       [[::c]]
       {::c (inc (+ a b))})
      ([[::a]]
       [[::b]]
       {::b (inc a)}))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm/throw!
      ^{:fm/doc "inc"}
      [a]
      (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm/throw!
      ([a] (inc a))
      (^{:fm/doc "2-arity"}
       [a b] (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/nonse
      [a]
      'b)
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: `s/keys` validates all qualified keys
  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/args [(s/keys :req [::a ::b])]}
      [m]
      m)
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '(^{:fm/ret (s/keys :req [::a ::b])}
      [x]
      {::a 1 ::b 2 ::c 'y}) ; NOTE: invalid `::c`
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: `select-keys` intuition patchwork for spec sugar
  (form/form
   {::form/ns *ns*
    ::definition
    '([[::a ::b :as m]]
      m)
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/form
   {::form/ns *ns*
    ::definition
    '([x]
      [[::a ::b]]
      {::a 1 ::b 2 ::c 'y}) ; NOTE: `::c` is ignored
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  ;;;
  )
