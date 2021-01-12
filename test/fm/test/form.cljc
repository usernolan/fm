(comment

  (s/valid? ::fn-symbol 'memoize)
  (s/valid? ::fn-symbol 'trash)

  (binding [*fn-symbol-set* #{'trash}]
    (s/valid? ::fn-symbol 'trash))

  (fn-form? '[memoize])
  (fn-form? '(memoize))
  (fn-form? (list memoize))
  (fn-form? (list 'memoize))
  (fn-form? '(trash))

  (binding [*fn-symbol-set* #{'trash}]
    (fn-form? '(trash)))

  (defmulti multi1 identity)

  (fn? multi1)
  (fn? multi1)
  (fn? identity)
  (fn? (fn [x] x))
  (fn? '(fn [x] x))
  (fn? #{})
  (fn? '#{})
  (fn-form? multi1)
  (fn-form? identity)
  (fn-form? (fn [x] x))
  (fn-form? '(fn [x] x))
  (fn-form? #{})
  (fn-form? '#{})
  (fn-form? '#{})

  (spec-form? '[s/or])
  (spec-form? '(s/or))
  (spec-form? 'a)

  ;;;
  )
