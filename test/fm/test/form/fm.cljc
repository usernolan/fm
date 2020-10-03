(comment

  #_(fm ^{:fm/doc "fm1"}
        fm1
        ^{:fm/args ,,,} [a] a)
  (def definition1
    (list
     (with-meta 'fm1 {:fm/doc "fm1"})
     (with-meta '[a] {:fm/args '[(complement nil?)]})
     'a))

  #_(fm ^{:fm/doc "fm1"}
        fm1
        (^{:fm/args ,,,} [a] a)
        (^{:fm/args ,,,} [a b] [a b]))
  (def definition2
    (list
     (with-meta 'fm1 {:fm/doc "fm1"})
     (list
      (with-meta '[a] {:fm/args '[(complement nil?)]})
      'a)
     (list
      (with-meta '[a b] {:fm/args '[(complement nil?) (complement nil?)]})
      '[a b])))

  (lib/conform-explain ::fm/definition definition1)
  (lib/conform-explain ::fm/definition definition2)

  (def params1
    {::ident      ::fm
     ::ns         "ns1"
     ::definition definition1})

  (case (first definition1)
    ::signature  '(,,,)
    ::signatures (into (map (comp ,,,) (second definition1))))

  (fn [a [b c] {:keys [d e] :as f} & {:kw kw}])

  #_#:fm.form{:sym fm1,
              :definition
              [:fm.form/fm-signature #:fm.form{:argv [a], :body [a]}]}

  #_#:fm.form{:sym fm1,
              :definition
              [:fm.form/fm-signatures
               [#:fm.form{:argv [a], :body [a]}
                #:fm.form{:argv [a b], :body [[a b]]}]]}

  (fn ^{:fm/doc "fm1"} fm1 ^{,,,}
    (^{:fm/args [int?] :fm/ret int?} [a] a)
    (^{:fm/args [int? string?] :fm/ret vector?} [a b] [a b]))

  ;;;
  )

(comment

  (s/def ::s1 any?)

  (lib/conform-explain ::fm/arg 'int?)
  (lib/conform-explain ::fm/arg int?)
  (lib/conform-explain ::fm/arg '(fn [x] x))
  (lib/conform-explain ::fm/arg (fn [x] x))
  (lib/conform-explain ::fm/arg '(partial (fn [x y z] z) 'x 'y))
  (lib/conform-explain ::fm/arg ::s1)
  (lib/conform-explain ::fm/arg ::valid) ; NOTE: accepts "to-be-defined" because spec does
  (lib/conform-explain ::fm/arg '(s/or ::s1 ::s1))
  (lib/conform-explain ::fm/arg 1)
  (lib/conform-explain ::fm/arg 'a)
  (lib/conform-explain ::fm/arg nil)
  (lib/conform-explain ::fm/arg '())
  (lib/conform-explain ::fm/arg [])
  (lib/conform-explain ::fm/arg {})
  (lib/conform-explain ::fm/arg #{})
  (lib/conform-explain ::fm/arg '[int? string?])
  (lib/conform-explain ::fm/arg '[int? [string?]])
  (lib/conform-explain ::fm/arg [int? string?])

    ;; NOTE: litmus recursion
  (def args1 '[int? (s/or :a int? :b string?) ::s1 (fn [x] (nil? x))])
  (def args2 '[int? (s/or :a int? :b string?) [::s1 (fn [x] (nil? x))]])
  (def args3 '[int? (s/or :a int? :b string?) [::s1 [(fn [x] (nil? x))]]])

  (lib/conform-explain ::fm/args args1)
  (lib/conform-explain ::fm/args args2)
  (lib/conform-explain ::fm/args args3)
  #_[int? int? & int?]
  ((fn [x1 x2 & xs]
     [x1 x2 xs (type xs)])
   1 2 3 4)

  #_[int? int? & [int? int?]]
  ((fn [x1 x2 & [x3 x4 :as xs]]
     [x1 x2 x3 x4 xs (type xs)])
   1 2 3 4)

  ((fn [x1 x2 & [x3 x4 :as xs]]
     [x1 x2 x3 x4 xs (type xs)])
   1 2 3)

  #_[int? int? & [int? [int?]]]
  ((fn [x1 x2 & [x3 [x4 :as ixs] :as xs]]
     [x1 x2 x3 x4 xs (type xs) ixs (type ixs)])
   1 2 3 [4])

  #_[int? int? & {:x3 int? ,,,}]
  ((fn [x1 x2 & {:keys [x3] :as xs}]
     [x1 x2 x3 xs (type xs)])
   1 2 :x3 3)

  #_[int? int? & {:x3 int?}]
  ((fn [x1 x2 & {:keys [x3] :as xs}]
     [x1 x2 x3 xs (type xs)])
   1 2 :x3 3)

  #_[int? int? & {:ns/x3 int?}]
  ((fn [x1 x2 & {:keys [ns/x3] :as xs}]
     [x1 x2 x3 xs (type xs)])
   1 2 :ns/x3 3)

  #_[int? int? & {:ns/x3 int?}]
  ((fn [x1 x2 & {:keys [ns/x3] :as xs}]
     [x1 x2 x3 xs (type xs)])
   1 2 :x3 3)

  #_[int? int? & {:x3 [int? [int?]]}]
  ((fn [x1 x2 & {:keys [x3] :as xs}]
     [x1 x2 x3 xs (type xs)])
   1 2 :x3 [3 [4]])

  (type int?) ; ???

  (lib/conform-explain ::fm/var-arg '{:kw int?})
  (lib/conform-explain ::fm/var-arg '{:kw [int? int? [int?]]})
  (lib/conform-explain ::fm/var-arg {:kw int?})
  (lib/conform-explain ::fm/var-arg '{})
  (lib/conform-explain ::fm/var-arg '[])

  (lib/conform-explain ::fm/var-args '[& int?])
  (lib/conform-explain ::fm/var-args '[& {:kw int?}])
  (lib/conform-explain ::fm/var-args '[& {:kw nil}])
  (lib/conform-explain ::fm/var-args '[& {}])
  (lib/conform-explain ::fm/var-args '[&])
  (lib/conform-explain ::fm/var-args '[& nil])

  (lib/conform-explain ::fm/args '[int? [int? [int?]]])
  (lib/conform-explain ::fm/args '[int? [int? [int?]] & int?])
  (lib/conform-explain ::fm/args '[int? [int? [int?]] & {:kw int?}])
  (lib/conform-explain ::fm/args '[int? [int? [int?]] & [int?]])
  (lib/conform-explain ::fm/args '[int? [int? [int?]] & (s/* any?)])
  (lib/conform-explain ::fm/args '[])
  (lib/conform-explain ::fm/args '[nil])
  (lib/conform-explain ::fm/args '[int? [int? [int?]] &])
  (lib/conform-explain ::fm/args '[int? [int? [int?]] & nil])
  (lib/conform-explain ::fm/args '[&])
  (lib/conform-explain ::fm/args '[& int?])
  (lib/conform-explain ::fm/args '[& {:kw int?}])
  (lib/conform-explain ::fm/args '[& [:kw]])
  (lib/conform-explain ::fm/args '[& [:ns/kw]])
  (lib/conform-explain ::fm/args '[& [[int?]]])

  ((fn [x1 x2 &]
     [x1 x2 & (type &)])
   1 2 3)

  ((fn [x1 x2 :as xs]
     [x1 x2 xs])
   1 2)

  (fn fm.form/fn1 [x] x)

  ((fn [x1 x2 & {:keys [x3 x4]}]
     [x1 x2 x3 x4])
   1 2 :x3 3 :x4 4)

  ((fn [x1 x2 & {:keys [x3 x4]}]
     [x1 x2 x3 x4])
   1 2 :x4 4 :x3 3)

  ((fn [x1 x2 & {:keys [x3 x4] :as xs}]
     (prn (lib/conform-explain (s/map-of keyword? int?) xs))
     [x1 x2 x3 x4 xs])
   1 2 :x3 3 :x4 4 :x4 5)

  ((fn [x1 x2 & {:syms [x3 x4] :as xs}]
     [x1 x2 x3 x4 xs])
   1 2 'x3 3 'x4 4)

  ((fn [x1 x2 & {:strs [x3 x4] :as xs}]
     [x1 x2 x3 x4 xs])
   1 2 "x3" 3 "x4" 4)

  (lib/conform-explain
   (s/*
    (s/alt
     :x3 (s/cat ::k #{:x3} ::v int?)
     :x4 (s/cat ::k #{:x4} ::v int?)))
   '(:x3 3 :x4 4))

  (keyword :k)
  (keyword :ns/k)
  (keyword 'k)
  (keyword 'ns/k)
  (keyword "k")
  (keyword "ns/k")

  ((fn [a b & {:keys [c d] :as cds}] [a b c d cds])
   1 2 :c 3 :d 4)

  ;;;
  )
