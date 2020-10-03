(comment

  (lib/conform-explain :fm/anomaly {::ident :a})
  (lib/conform-explain :fm/anomaly {:cognitect/anomaly 'a})
  (lib/conform-explain :fm/anomaly {::ide :a})
  (lib/conform-explain :fm/anomaly nil)
  (lib/conform-explain :fm/anomaly 'a)

  (anomaly? {::ident :a})

  (binding [*identifier-set* #{::ident :cognitect/anomaly}]
    (anomaly? {:cognitect/anomaly 'a}))

  (contains-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x 'x}}}]}])

  (contains-anomaly? [{::ident :a} #{[{:a #{{:k/v [[#{}]] :x 'x}}}]}])
  (contains-anomaly? [:a #{{::ident :a} [{:a #{{:k/v [[#{}]] :x 'x}}}]}])
  (contains-anomaly? [:a #{[{:a #{{{::ident :a} [[#{}]] :x 'x}}}]}])
  (contains-anomaly? [:a #{[{:a #{{:k/v [[#{{::ident :a}}]] :x 'x}}}]}])
  (contains-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x {::ident :a}}}}]}])
  (contains-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x 'x}}} {::ident :a}]}])
  (contains-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x 'x}}}]} {::ident :a}])

  (anomalous? {::ident 'a})
  (anomalous? [{::ident 'a}])

  (defn <map?>
    [x]
    (when (map? x) x))

  (defn select-identifiers
    [m]
    (select-keys m *identifier-set*))

  (defmulti  handler (comp first vals select-identifiers <map?>))
  (defmethod handler :fm.anomaly/args [a] a)

  ;;;
  )
