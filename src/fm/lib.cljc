(ns fm.lib
  (:refer-clojure :exclude [fn?])
  (:require
   [clojure.spec.alpha :as s]))


   ;;;
   ;;; NOTE: predicates
   ;;;


(def multifn?
  (partial
   instance?
   #?(:clj clojure.lang.MultiFn
      :cljs cljs.core/MultiFn)))

(def fn?
  (some-fn
   clojure.core/fn?
   multifn?))

(def singular? ; ALT: `singleton?`
  (every-pred
   seqable?
   (comp some? first)
   (comp nil? next)))


   ;;;
   ;;; NOTE: compound spec operations
   ;;;


(defn conform-explain
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      (s/explain spec x)
      c)))

(defn conform-throw!
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      (throw
       (ex-info
        (s/explain-str spec x)
        (s/explain-data spec x)))
      c)))


   ;;;
   ;;; NOTE: data transformations
   ;;;


(defn zip [recur? & xs]
  (apply
   map
   (fn [& ys]
     (if (every? recur? ys)
       (apply zip recur? ys)
       ys))
   xs))

(defn zipv [recur? & xs]
  (apply
   mapv
   (fn [& ys]
     (if (every? recur? ys)
       (apply zipv recur? ys)
       ys))
   xs))

(defn zipf [recur? f & xs]
  (apply
   map
   (fn [& ys]
     (if (every? recur? ys)
       (apply zipf recur? f ys)
       (apply f ys)))
   xs))

(defn zipvf [recur? f & xs]
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

(defn deep-some [pred xs]
  (rreduce
   (fn recur? [_ x]
     (if (pred x)
       (reduced x)
       (coll? x)))
   (constantly false)
   xs))

(defn evolve [xf x]
  (cond
    (fn? xf)         (xf x)
    (vector? xf)     (zipvf sequential? evolve xf x)
    (sequential? xf) (zipf sequential? evolve xf x)
    (map? xf)        (reduce
                      (fn [acc [k xf]]
                        (if (and (map? acc) (contains? acc k))
                          (update acc k (partial evolve xf))
                          acc))
                      x
                      xf))) ; ALT: `extend-protocol`


   ;;;
   ;;; NOTE: hierarchical retrieval
   ;;;


(defn geta
  ([m k]
   (let [ks (conj (or (descendants k) (hash-set)) k)]
     (reduce
      (fn [_ k]
        (when-let [v (get m k)]
          (reduced v)))
      nil
      ks)))
  ([h m k]
   (let [ks (conj (or (descendants h k) (hash-set)) k)]
     (reduce
      (fn [_ k]
        (when-let [v (get m k)]
          (reduced v)))
      nil
      ks)))) ; ALT: O(n) vs. O(m); use min(n, m), `isa?`

(defn geta-in
  ([m ks]
   (reduce
    (fn [acc k]
      (if-let [v (geta acc k)]
        v
        (reduced nil)))
    m
    ks))
  ([h m ks]
   (reduce
    (fn [acc k]
      (if-let [v (geta h acc k)]
        v
        (reduced nil)))
    m
    ks)))

(defn finda
  ([m k]
   (let [ks (conj (or (descendants k) (hash-set)) k)]
     (reduce
      (fn [_ k]
        (when-let [e (find m k)]
          (reduced e)))
      nil
      ks)))
  ([h m k]
   (let [ks (conj (or (descendants h k) (hash-set)) k)]
     (reduce
      (fn [_ k]
        (when-let [e (find m k)]
          (reduced e)))
      nil
      ks))))


   ;;;
   ;;; NOTE: default sequent combinators
   ;;;


(defn ensure-sequential [x]
  (if (sequential? x) x (vector x)))

(defn positional-combine
  ([args ret] (into args ret))
  ([argxs]
   (if (and (singular? argxs) (sequential? (first argxs)))
     (if (vector? (first argxs))
       (first argxs)
       (vec (first argxs)))
     (into (vector) (mapcat ensure-sequential) argxs))))

(defn nominal-combine
  ([args ret] (into args ret))
  ([argxs]
   (if (and (singular? argxs) (map? (first argxs)))
     (first argxs)
     (into (hash-map) argxs))))
