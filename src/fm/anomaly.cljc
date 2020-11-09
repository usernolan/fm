(ns fm.anomaly
  (:require
   [clojure.spec.alpha :as s]
   [fm.lib :as lib]))

(def ^:dynamic *indicator-keyset*
  "Set of keys whose presence indicates anomality"
  #{::ident})

(defn contains-indicator?
  [x]
  (and
   (or (set? x) (map? x))
   (some
    (partial contains? x)
    *indicator-keyset*)))

(s/def :fm/anomaly
  (s/and
   map?
   contains-indicator?))

(def anomaly?
  (partial s/valid? :fm/anomaly))

(def contains-anomaly?
  (partial
   lib/rreduce
   (fn recur? [acc x]
     (if (anomaly? x)
       (reduced true)
       (coll? x)))
   (constantly false)))

(s/def :fm/contains-anomaly
  (s/and
   coll?
   contains-anomaly?))

(s/def :fm/anomalous
  (s/or
   :fm/anomaly :fm/anomaly
   :fm/contains-anomaly :fm/contains-anomaly))

(def anomalous?
  (partial s/valid? :fm/anomalous))

#_(def idents
    #{::received
      ::args
      ::nested
      ::ret
      ::rel
      ::nonse ; ALT: ::ret
      ::thrown})

#_(s/def ::ident
    idents)
