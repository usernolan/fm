(comment

  ((fn [a [b :as c]] [a b c]) 1 [2])
  ((fn [a []] a) 1 2) ; NOTE: currently unspecified
  ((fn [a [:as b]] [a b]) 1 2)
  ((fn [a [:as]] [a]) 1 2)

  (lib/conform-explain
   ::fn/definition
   (list
    (with-meta 'fm1 {:fm/doc "fm1"})
    (with-meta '[a] {:fm/args '[int?]})
    'a))

  (lib/conform-explain
   ::fn/definition
   (list
    (with-meta 'fm1 {:fm/doc "fm1"})
    (list
     (with-meta '[a [b [c]]] {:fm/args '[int? [int? [int?]]]})
     '[a b c])
    (list
     (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
     '[a b c ds])))

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
   :fm/ident
   {::ident      ::fn
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a] {:fm/args '[int?]})
                  'a)})

  (->metadata-form
   :fm/ident
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a] {:fm/args '[int?]})
                   'a)}
    <<conformed-definition))

  (->metadata-form
   :fm/arglists
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a] {:fm/args '[int?]})
                   'a)}
    <<conformed-definition))

  (->metadata-form
   :fm/arglists
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1"})
                   (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                   '[a b c ds])}
    <<conformed-definition))

  (->metadata-form
   :fm/arglists
   (->>
    {::ident      ::fn
     ::ns         *ns*
     ::definition (list
                   (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                   (list '[a] 'a)
                   (list '[a & bs] '[a bs]))}
    <<conformed-definition))

  (type *1)

  (->metadata-form
   :fm/args
   (->>
    {::ident       ::fn
     ::ns          *ns*
     ::definition  (list
                    (with-meta 'fn1 {:fm/doc "fn1"})
                    (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                    '[a b c ds])
     ::fn/metadata {:fm/doc "fn1" :fm/args '[int? [int? [int?]] & int?]}}
    <<conformed-definition))

  (->metadata-form
   :fm/args
   (->>
    {::ident              ::fn
     ::ns                 *ns*
     ::definition         (list
                           (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                           (list '[a] 'a)
                           (list '[a & bs] '[a bs])
                           (list
                            (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                            '[a b c ds]))
     ::fn/outer-metadata  {:fm/doc "fn1" :fm/args '[int? & int?]}
     ::fn/inner-metadatas (list nil nil {:fm/args '[int? [int? [int?]] & int?]})}
    <<conformed-definition))

  (->metadata-form
   :fm/doc
   (->>
    {::ident              ::fn
     ::ns                 *ns*
     #_#_
     ::definition         (list
                           (with-meta 'fn1 {:fm/doc "fn1" :fm/args '[int? & int?]})
                           (list
                            (with-meta '[a] {:fm/doc "sig1" :fm/args '[even?]})
                            'a)
                           (list '[a & bs] '[a bs])
                           (list
                            (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
                            '[a b c ds]))
     ::definition         '(^{:fm/doc "fn1" :fm/args [int? & int?]} fn1
                            (^{:fm/doc "sig1" :fm/args [even?]}
                             [a] a)
                            ([a & bs] [a bs])
                            (^{:fm/args [int? [int? [int?]] & int?]}
                             [a [b [c]] & ds]
                             [a b c ds]))
     ::fn/outer-metadata  {:fm/doc "fn1" :fm/args '[int? & int?]}
     ::fn/inner-metadatas (list
                           {:fm/doc "sig1" :fm/args '[even?]}
                           nil
                           {:fm/args '[int? [int? [int?]] & int?]})}
    <<conformed-definition))

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
                   (with-meta 'fn1 {:fm/doc "fn1"})
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

  (lib/zipv
   vector?
   '[a b & [c d e & [f g & hs]]]
   '[int? [int? int? int?] & [::s1 (fn [a] (pos? a)) [int? int? int? & int?] & [(s/and pos? even?) int? & int?]]])

  (partition-by (hash-set '(& &)) *1)

  (partition-by
   #{'(& &)}
   '[(a a?)])

  (partition-by
   #{'(& &)}
   '[(& &) (a a?)])

  '[a b (c d e f gs)]

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
   (s/cat
    :a a?
    ::pos-arg_1
    (s/spec
     (s/cat
      ::var-args
      (s/? (s/* b?))))
    ::var-arg
    )

   (lib/zipv
    sequential?
    '[a [& bs :as bs2] & [c [& ds :as ds2] & es :as cs2]]
    '[a? [& b?] & [c? [& d?] & e?]]
    )

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
    ::definition (list
                  (with-meta 'fn1 {:fm/doc "fn1"})
                  (with-meta '[a] {:fm/args '[int?]})
                  'a)})

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
    '(^{:fm/doc     "fn1"
        :fm/args    [int?]
        :fm/ret     int?
        :fm/rel     (fn [{args :args ret :ret}]
                      (= (apply + args) ret))
        :fm/trace   #{:fm/args :fm/ret}
        :fm/conform #{:fm/args}
        :fm/handler (fn [a] a)}
      fn1
      [a]
      (inc a))})

  (->context
   {::ident ::fn
    ::ns    *ns*
    ::definition
    '(^{:fm/doc     "fn1"
        :fm/args    [int? int?]
        :fm/ret     int?
        :fm/rel     (fn [{args :args ret :ret}]
                      (>= ret (apply + args)))
        :fm/trace   #{:fm/args :fm/ret}
        :fm/conform #{:fm/args}
        :fm/handler (fn [a] a)}
      fn1
      ([a] (inc a))
      ([a b] (+ a b)))})

  (->context
   {::ident ::fn
    ::ns    *ns*
    ::definition
    '(^{:fm/doc     "fn1"
        :fm/args    [int? int?]
        :fm/ret     int?
        :fm/rel     (fn [{args :args ret :ret}]
                      (>= ret (apply + args)))
        :fm/trace   #{:fm/args :fm/ret}
        :fm/conform #{:fm/args}
        :fm/handler (fn [a] a)}
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
    '(^{:fm/doc     "fn1"
        :fm/args    [int? int?]
        :fm/ret     int?
        :fm/rel     (fn [{args :args ret :ret}]
                      (>= ret (apply + args)))
        :fm/trace   #{:fm/args :fm/ret}
        :fm/conform #{:fm/args}
        :fm/handler (fn [a] a)}
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
         :fm/trace           #{:fm/args :fm/ret}
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       (^{:fm/doc "sig1"}
        [a] (inc a))
       (^{:fm/trace #{:fm/args}
          :fm/ret   int?}
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
         :fm/conform         #{:fm/args}
         :fm.anomaly/handler (fn [a] a)}
       ([] 1)
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
