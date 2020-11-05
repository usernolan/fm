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

  (reset! trace-atom [])

  (->form
   {::ns *ns*
    ::definition
    '(^{:fm/doc "fn1"}
      fn1
      ^{:fm/args [int?]}
      [a]
      (inc a))}
   ::fn)

  (clojure.core/let
      [args8064 [:fm.form/bindings :fm/args]]
    (clojure.core/with-meta
      (clojure.core/fn fn1 [])
      '#:fm{:arglists [[a]], :doc "fn1", :args [[int?]], :ident :fm.form/fn1}))

  (->metadata-form
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (list
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds])
                   (list '[a & bs] '[a bs])
                   (list
                    (with-meta '[a] {:fm/doc "sig1" :fm/args '[even?]})
                    'a))}
    <<conformed-definition
    <<metadata)
   :fm/args)

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition '(^{:fm/doc "fn1"} fn1
                   ^{:fm/args [int?]}
                   [a]
                   (inc a))})

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition '(^{:fm/doc "fn1"} fn1
                   ^{:fm/args [int? int?]}
                   (^{:fm/doc "sig1"}
                    [a] (inc a))
                   ([a b] (+ a b)))})

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                  '[a b c ds])})

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a [b [c] :as b&c] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                  '[a b c ds])})

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a [b [c] :as b&c] & [d]] {:fm/args '[int? [int? [int?]] & [int?]]})
                  '[a b c ds])})

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a [b [c] :as b&c] & [d :as f]] {:fm/args '[int? [int? [int?]] & [int?]]})
                  '[a b c ds])})

  (->context
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

  (->context
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

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? int?]})
                  (list '[a] 'a)
                  (list '[a b] '[a b]))})

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                  (list '[a] 'a)
                  (list '[a & bs] '[a bs]))})

  (s/def ::m1 (s/map-of keyword? any?))

  (->context
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fm1 {:fm/doc "fm1" :fm/args '[int? [int? [int?]] ::m1 & {:f int? :g int?}]})
                  (list '[a] 'a)
                  (list '[a [b]] '[a b])
                  (list '[a [b [c]]] '[a b c])
                  (list '[a [b [c]] {:keys [d] :as e}] '[a b c d e])
                  (list '[a [b [c]] {:keys [d] :as e} & {:keys [f g] :as h}] '[a b c d e f g h]))})

  (->context
   {::ident ::fn
    ::ns    *ns*
    ::definition
    '(^{:fm/doc             "fn1"
        :fm/args            [int?]
        :fm/ret             int?
        :fm/rel             (fn [{args :args ret :ret}]
                              (= (apply + args) ret))
        :fm/trace           #{:fm/args :fm/ret}
        :fm/conform         #{:fm/args}
        :fm.anomaly/handler (fn [a] a)}
      fn1
      [a]
      (inc a))})

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
      fn1
      ([a] (inc a))
      ([a b] (+ a b)))})

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
      fn1
      (^{:fm/doc "sig1"}
       [a] (inc a))
      (^{:fm/trace #{:fm/args}
         :fm/ret   even?}
       [a b] (+ a b)))})

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
      (^{:fm/doc "sig1"}
       [a] (inc a))
      (^{:fm/trace #{:fm/args}
         :fm/ret   int?}
       [a b] (+ a b)))})

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
         :fm/trace           true
         :fm/conform         true
         :fm.anomaly/handler (fn [a] (prn "anomaly!" a) a)}
       ([a] (inc a))
       ([a b] (+ a b)))}))

  (def fn1
    (clojure.core/let
     [trace11666
      true
      arglists11668
      '[a]
      arglists11669
      '[a b]
      doc11670
      "fn1"
      rel11671
      (fn [{args :args, ret :ret}] (>= ret (apply + args)))
      column11673
      15
      conform11674
      true
      line11676
      2989
      args11677
      [int?]
      args11678
      [int? int?]
      handler11679
      (fn [a] (prn "anomaly!" a) a)
      ident11681
      :fm.form/fm11665
      ret11682
      int?]
      (clojure.core/with-meta
        (clojure.core/fn
          fm11665
          ([a]
           (try
             (clojure.core/let
              [args11684 [a]]
               (trace11666 {:fm/ident :fm.form/fm11665, :fm.trace/args args11684})
               (if
                (fm.anomaly/anomalous? args11684)
                 (handler11679
                  {:fm.anomaly/ident :fm.anomaly/received,
                   :fm.anomaly/args  args11684,
                   :fm/ident         :fm.form/fm11665})
                 (clojure.core/let
                  [conformed-args11686
                   (clojure.spec.alpha/conform args-spec11685 args11684)]
                   (trace11666
                    {:fm/ident                :fm.form/fm11665,
                     :fm.trace/conformed-args conformed-args11686})
                   (if
                    (clojure.spec.alpha/invalid? conformed-args11686)
                     (handler11679
                      {:fm.anomaly/ident :fm.anomaly/args,
                       :clojure.spec.alpha/explain-data
                       (clojure.spec.alpha/explain-data args-spec11685 args11684),
                       :fm/ident         :fm.form/fm11665})
                     (clojure.core/let
                      [[a] conformed-args11686]
                       (clojure.core/let
                        [ret11687 (do (inc a))]
                         (trace11666 {:fm/ident :fm.form/fm11665, :fm.trace/ret ret11687})
                         (if
                          (fm.anomaly/anomalous? ret11687)
                           (handler11679
                            {:fm.anomaly/ret   ret11687,
                             :fm.anomaly/ident :fm.anomaly/nested,
                             :fm.anomaly/args  args11684,
                             :fm/ident         :fm.form/fm11665})
                           (clojure.core/let
                            [conformed-ret11689
                             (clojure.spec.alpha/conform ret-spec11688 ret11687)]
                             (trace11666
                              {:fm/ident               :fm.form/fm11665,
                               :fm.trace/conformed-ret conformed-ret11689})
                             (if
                              (clojure.spec.alpha/invalid? conformed-ret11689)
                               (handler11679
                                {:fm.anomaly/ident :fm.anomaly/ret,
                                 :clojure.spec.alpha/explain-data
                                 (clojure.spec.alpha/explain-data ret-spec11688 ret11687),
                                 :fm.anomaly/args  args11684,
                                 :fm/ident         :fm.form/fm11665})
                               (clojure.core/let
                                [ret11687 conformed-ret11689]
                                 (if
                                     (rel11671 {:args args11684, :ret ret11687})
                                   ret11687
                                   {:fm.anomaly/ident :fm.anomaly/rel,
                                    :clojure.spec.alpha/explain-data
                                    (clojure.spec.alpha/explain-data
                                     rel11671
                                     {:args args11684, :ret ret11687}),
                                    :fm/ident         :fm.form/fm11665 })))))))))))
             (catch
              java.lang.Throwable
              thrown__6700__auto__
               (handler11679
                {:fm.anomaly/ident  :fm.anomaly/thrown,
                 :fm.anomaly/thrown thrown__6700__auto__,
                 :fm.anomaly/args   [a],
                 :fm/ident          :fm.form/fm11665}))))
          ([a b]
           (try
             (clojure.core/let
              [args11690 [a b]]
               (trace11666 {:fm/ident :fm.form/fm11665, :fm.trace/args args11690})
               (if
                (fm.anomaly/anomalous? args11690)
                 (handler11679
                  {:fm.anomaly/ident :fm.anomaly/received,
                   :fm.anomaly/args  args11690,
                   :fm/ident         :fm.form/fm11665})
                 (clojure.core/let
                  [args-spec11691
                   (clojure.spec.alpha/cat :0 int? :1 int?)
                   conformed-args11692
                   (clojure.spec.alpha/conform args-spec11691 args11690)]
                   (trace11666
                    {:fm/ident                :fm.form/fm11665,
                     :fm.trace/conformed-args conformed-args11692})
                   (if
                    (clojure.spec.alpha/invalid? conformed-args11692)
                     (handler11679
                      {:fm.anomaly/ident :fm.anomaly/args,
                       :clojure.spec.alpha/explain-data
                       (clojure.spec.alpha/explain-data args-spec11691 args11690),
                       :fm/ident         :fm.form/fm11665})
                     (clojure.core/let
                      [[a b] conformed-args11692]
                       (clojure.core/let
                        [ret11693 (do (+ a b))]
                         (trace11666 {:fm/ident :fm.form/fm11665, :fm.trace/ret ret11693})
                         (if
                          (fm.anomaly/anomalous? ret11693)
                           (handler11679
                            {:fm.anomaly/ret   ret11693,
                             :fm.anomaly/ident :fm.anomaly/nested,
                             :fm.anomaly/args  args11690,
                             :fm/ident         :fm.form/fm11665})
                           (clojure.core/let
                            [ret-spec11694
                             int?
                             conformed-ret11695
                             (clojure.spec.alpha/conform ret-spec11694 ret11693)]
                             (trace11666
                              {:fm/ident               :fm.form/fm11665,
                               :fm.trace/conformed-ret conformed-ret11695})
                             (if
                              (clojure.spec.alpha/invalid? conformed-ret11695)
                               (handler11679
                                {:fm.anomaly/ident :fm.anomaly/ret,
                                 :clojure.spec.alpha/explain-data
                                 (clojure.spec.alpha/explain-data ret-spec11694 ret11693),
                                 :fm.anomaly/args  args11690,
                                 :fm/ident         :fm.form/fm11665})
                               (clojure.core/let
                                [ret11693 conformed-ret11695]
                                 (if
                                     (rel11671 {:args args11690, :ret ret11693})
                                   ret11693
                                   {:fm.anomaly/ident :fm.anomaly/rel,
                                    :clojure.spec.alpha/explain-data
                                    (clojure.spec.alpha/explain-data
                                     rel11671
                                     {:args args11690, :ret ret11693}),
                                    :fm/ident         :fm.form/fm11665})))))))))))
             (catch
              java.lang.Throwable
              thrown__6700__auto__
               (handler11679
                {:fm.anomaly/ident  :fm.anomaly/thrown,
                 :fm.anomaly/thrown thrown__6700__auto__,
                 :fm.anomaly/args   [a b],
                 :fm/ident          :fm.form/fm11665})))))
        '{:fm/trace    [true true],
          :fm/arglists ['[a] '[a b]],
          :fm/doc      "fn1",
          :fm/rel
          [(fn [{args :args, ret :ret}] (>= ret (apply + args)))
           (fn [{args :args, ret :ret}] (>= ret (apply + args)))],
          :column      15,
          :fm/conform  [true true],
          :line        2989,
          :fm/args     [[int?] [int? int?]],
          :fm.anomaly/handler
          [(fn [a] (prn "anomaly!" a) a) (fn [a] (prn "anomaly!" a) a)],
          :fm/ident    :fm.form/fm11665,
          :fm/ret      [int? int?]})))

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? int? & int?]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a b & cs] (apply + a b cs))}))


  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    int?
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 2])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    int?
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 2 3])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    int?
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 2 3 a])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    int?
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[a 2 a])

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? [int? int? int?] & int?]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a [b1 b2 b3] & cs] (apply + a b1 b2 b3 cs))}))

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 nil])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 2])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 [2]])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 [a]])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 [a]])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 [2 3 4]])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[1 [2 3 4] 5 a])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat :0 int? :1 int? :2 int?))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :0
      int?
      :rest
      (clojure.spec.alpha/* clojure.core/any?))))
   '[a [2 3 4] 5 a])

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? [int? int? int? & int?] & int?]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a [b1 b2 b3 & bs :as bs] & cs] (apply + a (into bs cs)))}))

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat
      :0
      int?
      :1
      int?
      :2
      int?
      :&
      (clojure.spec.alpha/* int?)))
    :&
    (clojure.spec.alpha/* int?))
   '[1 [2 3 4]])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat
      :0
      int?
      :1
      int?
      :2
      int?
      :&
      (clojure.spec.alpha/* int?)))
    :&
    (clojure.spec.alpha/* int?))
   '[1 [2 3 4 5]])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat
      :0
      int?
      :1
      int?
      :2
      int?
      :&
      (clojure.spec.alpha/* int?)))
    :&
    (clojure.spec.alpha/* int?))
   '[1 [2 3 4 5] 6])

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :0
    int?
    :1
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat
      :0
      int?
      :1
      int?
      :2
      int?
      :&
      (clojure.spec.alpha/* int?)))
    :&
    (clojure.spec.alpha/* int?))
   '[1 [2 3 4 5] 6 7])

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? [int? & [int?]] & [int? & even?]]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a [b1 & [b2]] & [c & ds]] (apply + a (into bs cs)))}))

  (lib/conform-explain
   (clojure.spec.alpha/cat
    :a
    int?
    :bs
    (clojure.spec.alpha/spec
     (clojure.spec.alpha/cat
      :b1
      int?
      :&
      (clojure.spec.alpha/?
       (clojure.spec.alpha/cat
        :b2
        int?
        :rest
        (clojure.spec.alpha/* clojure.core/any?)))))
    :&
    (clojure.spec.alpha/?
     (clojure.spec.alpha/cat
      :c int?
      :& (clojure.spec.alpha/* even?))))
   '[1 [2 3 a b c] d 6])

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? [int? & [int?]] & [int? & [even?]]]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a [b1 & [b2] :as bs] & [c & [ds] :as cs]] (apply + a (into bs cs)))}))

  ((fn [a [b & [c :as cs] :as bs] & [e & f :as es]]
     [a b bs c cs e f es])
   'a '[b c h] 'e 'f 'g)

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? [int? & [int?]] & [int? & even?]]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       [a [b1 b2 b3 & bs :as bs] & cs] (apply + a (into bs cs)))}))

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
       ([a] (inc a))
       (^{:fm/trace #{}
          :fm/ret   even?}
        [a b] (+ a b)))}))

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
         :fm/conform         #{:fm/args :fm/ret}
         :fm.anomaly/handler (fn [a] a)}
       ([] 1)
       (^{:fm/trace            #{}
          :fm/ret              even?
          :fm.anomaly/handler? true}
        [a b] (+ a b)))}))

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
         :fm/conform         #{:fm/args :fm/ret}
         :fm.anomaly/handler (fn [a] a)}
       ([] 1)
       (^{:fm/trace            #{}
          :fm/ret              [even? & [even? int?]]
          :fm.anomaly/handler? true}
        [a b] (+ a b)))}))

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       ([] 1)
       (^{:fm/args [int? int?]}
        [a b] (+ a b)))}))

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       ([a] (inc a))
       (^{:fm/args [int? int?]}
        [a b] (+ a b)))}))

  (->form
   ::fn
   (->context
    {::ident ::fn
     ::ns    *ns*
     ::definition
     '(^{:fm/doc             "fn1"
         :fm/args            [int? int? & int?]
         :fm/ret             int?
         :fm/rel             (fn [{args :args ret :ret}]
                               (>= ret (apply + args)))
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       (^{:fm/trace #{}
          :fm/ret   even?}
        [a b & cs] (apply + a b cs)))}))

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
