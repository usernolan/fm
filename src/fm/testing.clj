(ns fm.testing
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as stest]
            [fm.macros :refer [defm]]
            [fm.utils :refer [fm? not-anomaly?]]))

(alias 'stc 'clojure.spec.test.check)

(s/def ::namespaces
  (s/coll-of symbol?
             :distinct true))

(s/def ::check-result-pass
  (s/schema
   {:num-tests       nat-int?
    :pass?           boolean?
    :result          any?
    :seed            nat-int?
    :time-elapsed-ms nat-int?}))

(s/def ::check-result-fail
  (s/schema
   {:fail            any?
    :failed-after-ms nat-int?
    :failing-size    any?
    :num-tests       nat-int?
    :pass?           boolean?
    :result          any?
    :result-data     any?
    :seed            nat-int?
    :shrunk          any?}))

(s/def ::check-result
  (s/schema
   {:stc/ret (s/or
              ::check-result-pass
              ::check-result-fail)
    :spec    s/spec?
    :sym     symbol?}))

(defm fmdef!
  ^{:fm/doc "Takes a defm and passes it through to s/fdef."
    :fm/args map?
    :fm/ret  symbol?}
  [{:keys [fm/sym fm/args]}]
  (eval
   `(s/fdef ~sym
      :args ~(s/form args)
      :ret not-anomaly?)))

(defm ns->fnsym
  ^{:fm/doc "Extracts all defms from the given namespace."
    :fm/args symbol?
    :fm/ret (s/coll-of fn?)}
  [ns-sym]
  (->> ns-sym ns-publics vals (map deref)))

(defn fms-from-ns
  ^{:fm/args ::namespaces
    :fm/ret  (s/coll-of symbol?)}
  [namespaces]
  (->>
   namespaces
   (mapcat ns->fnsym)
   (filter fm?)
   (map (comp fmdef! meta))))

(defm check
  ^{:fm/doc
    "Iterate through namespaces, extracting any fns that have fm metadata,
registering those with s/fdef, and finally running them against s/check."
    :fm/args ::namespaces
    :fm/ret  (s/coll-of ::check-result)}
  [namespaces]
  (->>
   namespaces
   (fms-from-ns)
   (stest/check)))

(comment

  (fms-from-ns '[fm.test.ns1])


  )
