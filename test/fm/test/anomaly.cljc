(comment

  (lib/conform-explain :fm/anomaly {::ident :a})
  (lib/conform-explain :fm/anomaly {:cognitect/anomaly 'a})
  (lib/conform-explain :fm/anomaly {::ide :a})
  (lib/conform-explain :fm/anomaly nil)
  (lib/conform-explain :fm/anomaly 'a)

  (anomaly? {::ident :a})
  (anomaly? nil)
  (anomaly? 'a)

  (deep-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x 'x}}}]}])
  (deep-anomaly? [{::ident :a} #{[{:a #{{:k/v [[#{}]] :x 'x}}}]}])
  (deep-anomaly? [:a #{{::ident :a} [{:a #{{:k/v [[#{}]] :x 'x}}}]}])
  (deep-anomaly? [:a #{[{:a #{{{::ident :a} [[#{}]] :x 'x}}}]}])
  (deep-anomaly? [:a #{[{:a #{{:k/v [[#{{::ident :a}}]] :x 'x}}}]}])
  (deep-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x {::ident :a}}}}]}])
  (deep-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x 'x}}} {::ident :a}]}])
  (deep-anomaly? [:a #{[{:a #{{:k/v [[#{}]] :x 'x}}}]} {::ident :a}])

  (anomalous? {::ident 'a})
  (anomalous? [{::ident 'a}])

  ;;;
  )
