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

(comment ; NOTE: extending anomaly indication

  (ns example.ns1
    (:require
     [fm.anomaly :as anomaly]))

    ;; NOTE: https://github.com/cognitect-labs/anomalies/blob/master/src/cognitect/anomalies.cljc
  (def indicator-hierarchy
    (swap!
     anomaly/indicator-hierarchy-atom
     (fn [hierarchy]
       (->
        hierarchy
        (derive :cognitect.anomalies/category ::anomaly/ident)))))

  (defmulti handler
    (fn [anomaly]
      (anomaly/geta anomaly ::anomaly/ident))
    #_:hierarchy #_some-other-hierarchy-ref)

  (defmethod handler :fm.anomaly/args
    [anomaly]
    "args!")

  (defmethod handler :cognitect.anomalies/unavailable
    [anomaly]
    "unavailable!")

  (handler {::anomaly/ident ::anomaly/args})
  (handler {:cognitect.anomalies/category :cognitect.anomalies/unavailable})

  ;;;
  )
