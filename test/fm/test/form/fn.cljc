(comment

  (def h1 prn)

    ;; NOTE: no outer `let`
  (form/->form
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

  (form/->form
   {::form/ns *ns*
    ::definition
    '(^{:fm/handler h1}
      ([a]
       (inc a))
      ([a b]
       (inc (+ a b))))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (def h2 (constantly :bummer))

  (form/->form
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

  (s/def ::a any?)
  (s/def ::as any?)
  (s/def ::b any?)
  (s/def ::bs any?)
  (s/def ::c any?)
  (s/def ::cs any?)

    ;; TODO: shrink, isolate test cases
    ;; TODO: eliminate redundancy in `fn-args`
  (form/->form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "fn1" :fm/handler? true :fm/throw! true}
      [a [b1 & [b2] :as bs] & [c & ds]]
      (apply + a b1 b2 c ds))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/->form
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

  (s/def ::a int?)
  (s/def ::b int?)
  (s/def ::c int?)
  (s/def ::cs (s/* int?))

  (form/->form
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

  (form/->form
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

  (form/->form
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

  (s/def ::x int?)

  (form/->form
   {::form/ns *ns*
    ::definition
    '(^{:fm/doc "variadic increment"}
      (^{:fm/ret ::x}
       [::a ::b :as argv]
       (inc (+ a b)))
      (^:fm/trace ^:fm/throw! ^{:fm/rel (fn [{[a b c] :args [x] :ret}] (>= x (+ a b c)))}
       [::a b ::c]
       [::x]
       [(inc (+ a b c))])
      (^{:fm/trace (fn [t] (prn :flavor t)) :fm/rel (fn [{a :args r :ret}] (prn a r) true)}
       [[::a :b ::c]]
       [[::x]]
       {::x (inc (+ a b c))}))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/->form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      [[::a]]
      [[::b]]
      (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/->form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      [::a]
      [[::b]]
      (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/->form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/nonse
      [::a]
      [[::b]]
      (inc a))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

    ;; NOTE: locally deduplicated spec forms
    ;; NOTE: essentially `:fm.sequent/iso`
  (form/->form
   {::form/ns *ns*
    ::definition
    '(^:fm.sequent/conse
      ([[::a]]
       [[::b]]
       (inc a))
      ([[::b]]
       [[::a]]
       (dec b)))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  (form/->form
   {::form/ns *ns*
    ::definition
    '((^:fm.sequent/conse
       [[::a]]
       [[::b]]
       (inc a))
      (^:fm.sequent/nonse
       [[::b]]
       [[::a]]
       (dec b)))
    ::defaults
    {:fm/throw!   nil
     :fm/trace    nil
     :fm/trace-fn `prn
     :fm/handler  `identity}}
   ::form/fn)

  ;;;
  )
