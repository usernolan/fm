(ns fm.sequent
  (:require
   [clojure.spec.alpha :as s]
   [fm.anomaly :refer [anomaly?]]
   [fm.lib :as lib :refer [singular? which positional-combine nominal-combine]]))


   ;;;
   ;;; NOTE: context detection predicates
   ;;; ALT: `fm.analysis`, `fm.context` ns
   ;;;


(s/def ::specv
  (s/or :fm.context/nominal (s/tuple vector?)
        :fm.contect/positional vector?)) ; TODO: `fm.lib`

(defn nominal? [f]
  (let [metadata (meta f)
        args     (get metadata :fm/args)
        context  (which ::specv (first args))]
    (and (= context :fm.context/nominal)
         (or (get metadata :fm/sequent)
             (singular? args)))))

(def positional?
  (complement nominal?))


   ;;;
   ;;; NOTE: internal logic
   ;;;


(defn wrapped-fn [f]
  (fn
    ([g]
     (with-meta
       (partial f g)
       (meta g)))
    ([x y]
     (if (lib/fn? x)
       (f x y)
       (f y x))) ; TODO: maintain `combine` ordering
    ([x y & zs]
     (if (lib/fn? x)
       (apply f x y zs)
       (apply f (last zs) x y (butlast zs))))))

(defn apply-combine
  [f & xs]
  (let [y (apply f xs)]
    (cond
      (anomaly? y)   y
      (and
       (map? y)
       (nominal? f)) (reduce nominal-combine {} [xs y])
      :else          (positional-combine [xs y]))))

(defn apply-effect
  [f & xs]
  (let [y (apply f xs)]
    (cond
      (anomaly? y) y
      (nominal? f) (nominal-combine xs)
      :else        (positional-combine xs))))


   ;;;
   ;;; NOTE: api
   ;;;


(def combine
  (wrapped-fn apply-combine))

(def effect
  (wrapped-fn apply-effect))
