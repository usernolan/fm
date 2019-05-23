(ns defm.utils
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.spec-alpha2 :as s]))

(defn schema-keys [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond (vector? schema)        (distinct (mapcat f schema))
          (map? schema)           (keys schema)
          (or (keyword? schema)
              (s/schema? schema)) (schema-keys (second (s/form schema)))
          (list? schema)          (schema-keys (second schema)))))

(defn anomaly?
  [m]
  (and (or (map? m) (set? m))
       (contains? m :defm/anomaly)))
