(ns fm.api
  (:require
   [clojure.alpha.spec.gen :as gen]
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]))

(def fm?
  (comp boolean :fm/sym meta))

(def anomaly?
  (partial s/valid? :fm/anomaly))

(def args-anomaly?
  (partial s/valid? ::anomaly/args))

(def ret-anomaly?
  (partial s/valid? ::anomaly/ret))

(def rel-anomaly?
  (partial s/valid? ::anomaly/rel))

(def throw-anomaly?
  (partial s/valid? ::anomaly/throw))

(def received-anomaly?
  (partial s/valid? ::anomaly/received))

(def not-anomaly?
  (comp not anomaly?))

(defn genform
  [tagging-spec x]
  (let [c (s/conform tagging-spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))
