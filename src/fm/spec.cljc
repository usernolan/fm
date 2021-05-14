(ns fm.spec
  (:refer-clojure :exclude [or cat keys select-keys])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen])
  #?(:cljs
     (:require-macros fm.spec)))


   ;;;
   ;;; NOTE: internal logic, helpers
   ;;;


(def unq
  (comp keyword name)) ; ALT: extend `clojure.lang.Named`; maintain type

(def unq-xf
  (comp (filter keyword?)
        (map unq)))

(def q-xf
  (filter keyword?))


   ;;;
   ;;; NOTE: spec reifications, transformations
   ;;;


(defn select-keys [s ks]
  (let [s (if (s/spec? s) s (s/get-spec s))]
    (reify
      s/Specize
      (specize* [s] s)
      (specize* [s _] s)
      s/Spec
      (conform* [_ x]
        (if (map? x)
          (let [y (clojure.core/select-keys x ks)
                c (s/conform* s y)]
            (cond
              (s/invalid? c)   ::s/invalid
              (identical? y c) x
              :else            (merge x c)))
          ::s/invalid))
      (unform* [_ x]
        (if (map? x)
          (let [y (clojure.core/select-keys x ks)
                c (s/unform* s y)]
            (if (identical? y c)
              x
              (merge x c)))
          x))
      (explain* [_ path via in x]
        (let [y (if (map? x) (clojure.core/select-keys x ks) x)]
          (s/explain* s path via in y)))
      (gen* [_ overrides path rmap]
        (s/gen* s overrides path rmap))
      (with-gen* [_ gfn]
        (s/with-gen* s gfn))
      (describe* [_]
        (cons `keys
              (rest (s/describe* s)))))))


   ;;;
   ;;; NOTE: spec creation macros
   ;;; NOTE: replaceable by custom spec ops in spec2
   ;;;


#?(:clj (defmacro or [& ks]
          `(s/or ~@(interleave ks ks))))

#?(:clj (defmacro cat [& ks]
          `(s/cat ~@(interleave ks ks))))

#?(:clj (defmacro alt [& ks]
          `(s/alt ~@(interleave ks ks))))

#?(:clj (defmacro keys [& {:keys [req opt req-un opt-un] :as args}]
          (let [q   (into (hash-set) q-xf (flatten (concat req opt)))
                unq (into (hash-set) unq-xf (flatten (concat req-un opt-un)))
                ks  (into q unq)]
            `(let [s# (s/keys ~@(apply concat args))]
               (select-keys s# ~ks)))))

#?(:clj (defmacro keys* [& args]
          `(let [s# (keys ~@args)]
             (s/with-gen
               (s/& (s/* (s/cat ::s/k keyword? ::s/v any?)) ::s/kvs->map s#)
               (fn [] (gen/fmap (fn [m] (apply concat m)) (s/gen s#)))))))
