(ns fm.lib
  (:require
   [clojure.spec.alpha :as s]))

(defn zip
  [recur? & xs]
  (apply
   map
   (fn [& ys]
     (if (every? recur? ys)
       (apply zip recur? ys)
       ys))
   xs))

(defn zipf
  [recur? f & xs]
  (apply
   map
   (fn [& ys]
     (if (every? recur? ys)
       (apply zipf recur? f ys)
       (apply f ys)))
   xs))

(defn zipv
  [recur? & xs]
  (apply
   mapv
   (fn [& ys]
     (if (every? recur? ys)
       (apply zipv recur? ys)
       ys))
   xs))

(defn zipvf
  [recur? f & xs]
  (apply
   mapv
   (fn [& ys]
     (if (every? recur? ys)
       (apply zipvf recur? f ys)
       (apply f ys)))
   xs))

(defn -rreduce
  ([recur? rf xs]         (-rreduce recur? (fn initf [acc _] acc) rf (fn cf [_ r] r) (rf) xs))
  ([recur? rf init xs]    (-rreduce recur? (fn initf [acc _] acc) rf (fn cf [_ r] r) init xs))
  ([recur? rf cf init xs] (-rreduce recur? (fn initf [acc _] acc) rf cf init xs))
  ([recur? initf rf cf init xs]
   (reduce
    (fn [acc x]
      (if-let [rec (recur? acc x)]
        (if (reduced? rec)
          (reduced rec)
          (let [init (initf acc x)]
            (if (reduced? init)
              (reduced init)
              (let [xs (if (true? rec) x rec)
                    r  (-rreduce recur? initf rf cf init xs)]
                (if (reduced? r)
                  (reduced r)
                  (let [c (cf acc r)]
                    (if (reduced? c)
                      (reduced c)
                      c)))))))
        (let [r (rf acc x)]
          (if (reduced? r)
            (reduced r) ; NOTE: ensures termination of all nested reductions
            r))))
    init
    xs)))

(def rreduce
  (comp unreduced -rreduce))

(defn consplain
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      (s/explain spec x)
      c)))

(defn conthrow
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      (throw
       (ex-info
        (s/explain-str spec x)
        #:fm.anomaly{:ident :fm.anomaly/conform
                     :args  [x]
                     :data  (s/explain-data spec x)}))
      c)))
