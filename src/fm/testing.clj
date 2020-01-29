(ns fm.testing
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as stest]
            [fm.macros :refer [defm]]
            [fm.utils :refer [fm? not-anomaly?]]
            [clojure.alpha.spec.gen :as gen]))

(s/def ::namespaces
  (s/coll-of
   symbol?
   :distinct true))

(s/def ::check-result any?)

(defn ns->fnsym
  [ns-sym]
  (->> ns-sym
       ns-publics
       vals
       (map deref)))

(defm handle-check-failure
  ^{:fm/args :fm/anomaly}
  [anomaly]
  (prn anomaly))

(defn fmdef!
  ^{:fm/doc "Takes a fn that has fm metadata on it and passes it through to s/fdef."
    :fm/args :fm/meta
    :fm/ret symbol?}
  [{:keys [:fm/sym :fm/args :fm/ret :fm/rel]}]
  (eval `(s/fdef ~sym
           :args (s/or args any?)
           :ret not-anomaly?)))

(defm fms-from-ns
  ^{:fm/args ::namespaces
    :fm/ret (s/coll-of symbol?)}
  [namespaces]
  (->> namespaces
       (mapcat ns->fnsym)
       (filter fm?)
       (map (comp fmdef! meta))))

(defm check
  ^{:fm/doc "Iterate through namespaces, extracting any fns that have fm metadata, registering those with s/fdef, and finally running them against s/check."
    :fm/args ::namespaces
    :fm/ret ::check-result
    :fm/anomaly handle-check-failure}
  [namespaces]
  (->> namespaces
       (fms-from-ns)
       (stest/check)))

(comment

  (require '[clojure.alpha.spec.gen :as gen])
  (gen/sample (gen/gen-for-pred int?))

  (defm test1
    ^{:fm/args (s/with-gen
                 (s/cat :n int?)
                 (fn [] (gen/vec (gen/gen-for-pred int?))))
      :fm/ret int?}
    [[n]]
      (inc n))

  (test1 [2])
  (fmdef! (meta @#'fm.testing/test1))

  (s/get-spec 'fm.testing/test1)

  (stest/check 'fm.testing/test1)

  (defm test2
    ^{:fm/args (s/with-gen
                 (s/cat :n int?)
                 (fn [] (gen/gen-for-pred int?)))
      :fm/ret (s/spec int? :gen
                      (fn [] (gen/gen-for-pred int?)))}
    [n]
    (inc n))

  (fmdef! (meta @#'fm.testing/test2))

  (s/get-spec 'fm.testing/test2)

  (stest/check 'fm.testing/test2)

  (defn test3
    [n]
    (inc n))

  (s/fdef test3
    :args
    (s/with-gen
      (s/cat :n int?)
      (fn [] (gen/gen-for-pred int?)))
    :ret int?)

  (s/get-spec 'fm.testing/test3)

  (stest/check 'fm.testing/test3)

  (defn test4
    [n]
    (inc n))

  (s/fdef test4
    :args int?
    :ret int?)

  (s/get-spec 'fm.testing/test4)

  (stest/check 'fm.testing/test4)

  (defn test5
    [n]
    (inc n))

  (s/fdef test5
    :args (s/cat :n int?)
    :ret int?)

  (s/get-spec 'fm.testing/test5)

  (stest/check 'fm.testing/test5)

  (s/def ::number int?)
  (s/conform (s/spec int?) 5)

  )

