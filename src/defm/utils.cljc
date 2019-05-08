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

(defn group-by-conform
  [spec x]
  (->> (s/conform spec x)
       (group-by first)
       (into {} (map (fn [[k v]] [k (mapv second v)])))))

(defn init
  [x]
  [#{x}])
