(ns fm.api
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]))

(def fm?
  (comp boolean :fm/sym meta))

(def sequent?
  (comp boolean :fm/sequent meta))

(def anomaly?
  (partial s/valid? :fm/anomaly))

(def args-anomaly?
  (partial s/valid? ::anomaly/args))

(def ret-anomaly?
  (partial s/valid? ::anomaly/ret))

(def nonse-anomaly?
  (partial s/valid? ::anomaly/nonse))

(def rel-anomaly?
  (partial s/valid? ::anomaly/rel))

(def throw-anomaly?
  (partial s/valid? ::anomaly/throw))

(def received-anomaly?
  (partial s/valid? ::anomaly/received))

(def not-anomaly?
  (comp not anomaly?))
