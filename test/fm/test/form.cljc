(comment

  ((fn [a [b :as c]] [a b c]) 1 [2])
  ((fn [a []] a) 1 2) ; NOTE: currently unspecified
  ((fn [a [:as b]] [a b]) 1 2)
  ((fn [a [:as]] [a]) 1 2)

  (lib/consplain
   ::fn/definition
   (list
    (with-meta 'fm1 {:fm/doc "fm1"})
    (with-meta '[a] {:fm/args '[int?]})
    'a))

  (lib/consplain
   ::fn/definition
   (list
    (with-meta 'fm1 {:fm/doc "fm1"})
    (list
     (with-meta '[a [b [c]]] {:fm/args '[int? [int? [int?]]]})
     '[a b c])
    (list
     (with-meta '[a [b [c]] & ds] {:fm/args '[int? [int? [int?]] & int?]})
     '[a b c ds])))

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

  (def context1 *1)
     ;; NOTE: `eval` because at this phase `:fm/args` is a form to be bound in `<<bindings`
  (lib/consplain (eval (get-in context1 [::metadata :fm/args])) [1])
  (lib/consplain (eval (get-in context1 [::metadata :fm/args])) [1 2])
  (lib/consplain (eval (get-in context1 [::metadata :fm/args])) [1 2 3 4])

  (s/def ::s2
    (eval (get-in context1 [::metadata :fm/args])))

  (s/def ::m1 (s/map-of keyword? any?))

  (->context
   {::ident      ::fm
    ::ns         *ns*
    ::definition (list
                  (with-meta 'fm1 {:fm/doc "fm1" :fm/args '[int? [int? [int?]] ::m1 & {:f int? :g int?}]})
                  (list '[a] 'a)
                  (list '[a [b]] '[a b])
                  (list '[a [b [c]]] '[a b c])
                  (list '[a [b [c]] {:keys [d] :as e}] '[a b c d e])
                  (list '[a [b [c]] {:keys [d] :as e} & {:keys [f g] :as h}] '[a b c d e f g h]))})

  (def context2 *1)
  (lib/consplain (eval (get-in context2 [::metadata :fm/args])) [1])
  (lib/consplain (eval (get-in context2 [::metadata :fm/args])) [1 [2]])
  (lib/consplain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]]])
  (lib/consplain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]] {:d 'd}])
  (lib/consplain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]] {:d 'd} :f 'f])
  (lib/consplain (eval (get-in context2 [::metadata :fm/args])) [1 [2 [3]] {:d 'd} :f 'f :g 'g])

  (->metadata-form
   nil
   [::fn/args
    (lib/conthrow
     ::fn/args
     '[int? [int? [int?]] & int?])])

  (->metadata-form
   nil
   [::fn/args
    (lib/conthrow
     ::fn/args
     '[int? [int? [int?]] & {:kw int?}])])

  (->metadata-form
   nil
   [::fn/args
    (lib/conthrow
     ::fn/args
     '[int? [int? [int?]] & [int?]])])

  (->metadata-form
   nil
   [::fn/args
    (lib/conthrow
     ::fn/args
     '[int? [int? [int?]] & (s/* int?)])])

  (->metadata-form
   nil
   [::fn/args
    (lib/conthrow
     ::fn/args
     '[& (s/cat :int? (s/* int?))])])

  (->metadata-form
   nil
   [::fn/args
    (lib/conthrow ::fn/args '[& int?])])

  (lib/consplain
   ::fn/signatures-args
   '[nil
     [nil nil]])

  (lib/consplain
   ::fn/signatures-args
   '[[int? [int? [int?]] & int?]
     [nil nil]])

  (s/def ::m1 (s/map-of keyword? any?))

  (def args1 '[int? [int? [int?]] ::m1 & int?])
  (def argv1 '[a [b [c :as cs] :as bs] {:keys [d] :as e} & fs])

  (def conformed-args1 (lib/consplain ::fn/args args1))

  (->form-data
   nil
   [::fn/arg* nil])

  (->form-data
   nil
   [::fn/arg* (::fn/arg* conformed-args1)])

  (->form-data
   {::fn/argv argv1}
   [::fn/arg* (::fn/arg* conformed-args1)])

  (->form-data
   nil
   [::fn/variadic? nil])

  (->form-data
   nil
   [::fn/variadic? (::fn/variadic? conformed-args1)])

  (->form-data
   {::fn/argv argv1}
   [::fn/variadic? (::fn/variadic? conformed-args1)])

  (def form-data1
    (into
     (->form-data
      {::fn/argv argv1}
      [::fn/arg* (::fn/arg* conformed-args1)])
     (->form-data
      {::fn/argv argv1}
      [::fn/variadic? (::fn/variadic? conformed-args1)])))

  (->metadata-form
   {::fn/argv argv1}
   [::fn/form-data form-data1])

  (->metadata-form
   {::fn/argv '[a [b [c]] {:keys [d]} & fs]}
   [::fn/args conformed-args1])

  '[int? [int? [int?]] ::m1 & [int?]]
  '[a [b [c :as cs] :as bs] {:keys [d] :as e} & [f]]
  '[1 [2 [3]] {:d 'd}]
  '[1 [2 [3]] {:d 'd} [1]]

  (map
   (comp
    (partial keyword "fm.form.fm.signature")
    (partial format "_%03d"))
   (range 5 11))

  (def bindings1
    {:fm/k1 [{::symbol 's1 ::form 'f1}
             {::symbol 's2 ::form 'f2}
             {::symbol 's3 ::form 'f3}]
     :fm/k2 [{::symbol 's3 ::form 'f3}
             {::symbol 's3 ::form 'f3}
             {::symbol 's4 ::form 'f4}]})

  (->form ::fn/context-bindings {::bindings bindings1})

  ;;;
  )

(comment

  #_(def inner-metas
      [{:fm/doc "outer" :fm/args 'outer-args :fm/ret int?}
       {:fm/doc "inner1" :fm/args 'inner1-args :fm/ret int?}
       {:fm/doc "inner2" :fm/args 'inner2-args :fm/ret int?}])

  #_(reduce
     (fn [acc m]
       (reduce
        (fn [acc [k _]]
          (update acc k (fnil conj []) (m k)))
        acc
        acc))
     {:fm/doc              []
      :fm/args             []
      :fm/ret              []
      :fm/rel              []
      :fm/trace            []
      :fm/conthrow         []
      :fm.anomaly/handler  []
      :fm.anomaly/handler? []
      :fm/ignore           []}
     inner-metas)

  ;;;
  )
