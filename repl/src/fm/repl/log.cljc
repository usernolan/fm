(comment

  (defmulti  multi1 set)
  (defmethod multi1 #{:a} [s] s)
  (defmethod multi1 #{:b :a} [s] s)
  (defmethod multi1 #{:a :c} [s] s)

  (multi1 #{:a})
  (multi1 #{:b})
  (multi1 [:a :b])
  (multi1 [:a :c])

  ;;;
  )

(comment

  (ns ns.ns1)

  (def ^:dynamic *dyn1* nil)

  (defmacro macro1
    [& definition]
    (if *dyn1*
      ''ay
      ''nah))

  (macro1 'definition)

  (ns ns.ns2
    (:require
     [ns.ns1 :as ns1]))

  ns1/*dyn1*

  (with-redefs [ns1/*dyn1* 'dyn1]

    (ns1/macro1 'definition))

  (binding [ns1/*dyn1* 'dyn1]
    (ns1/macro1 'definition))

  (alter-var-root #'ns1/*dyn1* (constantly 'dyn1))
  (ns1/macro1 'definition)

  (alter-var-root #'ns1/*dyn1* (constantly nil))
  (ns1/macro1 'definition)

  (ns ns.ns1)

  (def ^:dynamic *dyn1* nil)

  (defn p1?
    []
    (boolean *dyn1*))

  (defmacro macro1
    [& definition]
    (if (p1?)
      ''ay
      ''nah))

  (macro1 'definition)

  (ns ns.ns2
    (:require
     [ns.ns1 :as ns1]))

  ns1/*dyn1*

  (with-redefs [ns1/*dyn1* 'dyn1]
    (ns1/macro1 'definition))

  (binding [ns1/*dyn1* 'dyn1]
    (ns1/macro1 'definition))

  (alter-var-root #'ns1/*dyn1* (constantly 'dyn1))
  (ns1/macro1 'definition)

  (alter-var-root #'ns1/*dyn1* (constantly nil))
  (ns1/macro1 'definition)

  (ns ns.ns1
    (:require
     [ns.ns2 :as ns2]))

  (defmacro macro1
    [& definition]
    (ns2/->form *ns*))

  (macro1 'definition)

  (ns ns.ns2)

  (defn ->form
    [ns]
    (prn ns (type ns))
    ns)

  (ns ns.ns3
    (:require
     [ns.ns1 :as ns1]))

  (ns1/macro1 'definition) ; NOTE: `ns.ns3`

  ;;;
  )

(comment

  (defmacro -fm-
    [& definition]
    (prn definition)
    (let [-meta    (meta (first definition))
          _        (prn -meta)
          args     (:fm/args -meta)
          _        (prn args)
          resolved (resolve args)
          _        (prn resolved)
          eval1    @resolved
          _        (prn eval1)
          _        (prn (type eval1))
          _        (prn (type (first eval1)))]
      ''-fm-))

  ;;;
  )

(comment

  (require '[criterium.core :as crit])

  (crit/bench (vec (map (fn [a] a) (range 0 1000))))
  (crit/bench (vec (map identity (range 0 1000))))

  (defn id1 [a] a)
  (crit/bench (vec (map id1 (range 0 1000))))

  (let [id2 (fn [a] a)]
    (crit/bench (vec (map id2 (range 0 1000)))))

  ;;;
  )
