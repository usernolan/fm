(ns defm.utils
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.spec-alpha2 :as s]))

(def dispatch-tag "__dispatch__")

(defn cartesian-product
  [m]
  (map (partial zipmap (keys m))
       (apply combo/cartesian-product (vals m))))

(defn errored?
  [x]
  (some #(and (or (map? %) (set? %))
              (or (contains? % :clojure.spec.alpha/problems)
                  (contains? % :defm/anomaly))) x))

(defn wrap
  [f]
  (fn [x]
    (if (errored? (set (apply concat x)))
      x
      (f x))))

(defn group-by-conform
  [spec x]
  (->> (s/conform spec x)
       (group-by first)
       (into {} (map (fn [[k v]] [k (mapv second v)])))))

(comment

  (require '[defm.macros :refer [defm]])
  (require '[defm.utils :as defm])
  (require '[clojure.spec-alpha2 :as s])

  (s/def ::n number?)

  (defm m-inc [::n] (inc n))
  (defm m-sq {x ::n} (* x x))

  (map (comp ffirst defm/rest m-sq defm/rest m-inc defm/init) (range 1 5))

  )
