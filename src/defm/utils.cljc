(ns defm.utils
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.spec-alpha2 :as s]))

(def dispatch-tag "__dispatch__")

(defn cartesian-product
  [m]
  (map (partial zipmap (keys m))
       (apply combo/cartesian-product (vals m))))

(s/def :clojure.spec.alpha/problemed (s/and map? #(contains? % :clojure.spec.alpha/problems)))
(s/def :defm/anomalied (s/and map? #(contains? % :defm/anomalies)))

(defn errored?
  [m]
  (and (or (map? m) (set? m))
       (or (contains? m :clojure.spec.alpha/problems)
           (contains? m :defm/anomalies))))

(defn wrap
  [f]
  (fn [m & rest]
    (if (errored? m)
      m
      (apply f m rest))))

(defn group-by-conform
  [spec x]
  (->> (s/conform spec x)
       (group-by first)
       (into {} (map (fn [[k v]] [k (mapv second v)])))))

(defn deep-merge-with
  [f & ms]
  (apply merge-with
         (fn [a b]
           (if (and (map? a) (map? b))
             (deep-merge-with f a b)
             (f a b)))
         ms))

(comment

  (require '[defm.macros :refer [defm]])
  (require '[defm.utils :as defm])
  (require '[clojure.spec-alpha2 :as s])

  (s/def ::n number?)

  (defm m-inc [::n] (inc n))
  (defm m-sq {x ::n} (* x x))

  (map (comp ffirst defm/rest m-sq defm/rest m-inc defm/init) (range 1 5))

  )
