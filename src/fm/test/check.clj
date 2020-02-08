(ns fm.test.check
  (:require
   [clojure.alpha.spec :as s]
   [clojure.alpha.spec.test :as stest]
   [fm.utils :as fm]))

(s/def :clojure.spec.test.check/ret-pass
  (s/schema
   {:pass?           true?
    :num-tests       nat-int?
    :result          any?
    :seed            nat-int?
    :time-elapsed-ms nat-int?}))

(s/def :clojure.spec.test.check/ret-fail
  (s/schema
   {:pass?           false?
    :num-tests       nat-int?
    :result          any?
    :seed            nat-int?
    :failed-after-ms nat-int?
    :fail            any?
    :failing-size    any?
    :result-data     any?
    :shrunk          any?}))

(s/def :clojure.spec.test.check/ret
  (s/or
   :pass :clojure.spec.test.check/ret-pass
   :fail :clojure.spec.test.check/ret-fail))

(s/def :clojure.spec.test.check/result
  (s/schema
   [:clojure.spec.test.check/ret
    {:spec s/spec?
     :sym  symbol?}]))

(defn meta->fdef!
  [{:keys [fm/sym fm/args]}]
  (eval
   `(s/fdef ~sym
      :args ~(s/form args)
      :ret  fm/not-anomaly?)))

(defn fdef-ns-fms!
  "lazily fdef all fms in the given namespace."
  [ns-sym]
  (->>
   (ns-publics ns-sym)
   (vals)
   (map deref)
   (filter fm/fm?)
   (map (comp meta->fdef! meta))))

(defn check!
  [ns-sym-or-syms]
  (->>
   (if (vector? ns-sym-or-syms)
     ns-sym-or-syms
     (vector ns-sym-or-syms))
   (mapcat fdef-ns-fms!)
   (into [])
   (stest/check)))
