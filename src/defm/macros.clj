(ns defm.macros
  (:require
   [clojure.spec-alpha2 :as s]
   [clojure.data :as data]
   [defm.utils :as utils]))

(defmacro defm
  "The `defm` macro."
  [fname specs & body]
  (let [dispatch (keyword (str *ns*) (str fname utils/dispatch-tag))
        syms (if (map? specs) (map first specs) (map symbol specs))
        specs (if (map? specs) (vals specs) specs)]
    `(do (s/def ~dispatch (s/* (s/or ~@(interleave syms specs) nil identity)))
         (defn ~(symbol (name fname))
           [~@(when (empty? specs) '(&)) prev#]
           (let [in# (set (apply concat prev#))]
             (if (utils/errored? in#)
               prev#
               (try
                 (let [grouped# (utils/group-by-conform ~dispatch (seq in#))
                       missing# (filter #(not (contains? grouped# %)) '(~@syms))
                       out# (set (if (empty? missing#)
                                   (map (fn [{:syms ~(vec syms)}] ~@body)
                                        (utils/cartesian-product (dissoc grouped# nil)))
                                   (let [m# (select-keys '~(zipmap syms specs) missing#)
                                         sp# (s/spec* (cons `s/or (interleave (keys m#) (vals m#))))]
                                     (map (partial s/explain-data sp#)
                                          (get grouped# nil)))))]
                   (data/diff in# out#))
                 (catch Exception e#
                   (let [fname# ~(keyword (str *ns*) (str fname))
                         out# #{{:defm/fname fname# :defm/anomaly e#}}]
                     (data/diff in# out#))))))))))

(defmacro defmm
  "Monotonic `defm`."
  [fname specs & body]
  (let [dispatch (keyword (str *ns*) (str fname utils/dispatch-tag))
        syms (if (map? specs) (map first specs) (map symbol specs))
        specs (if (map? specs) (vals specs) specs)]
    `(do (s/def ~dispatch (s/* (s/or ~@(interleave syms specs) nil identity)))
         (defn ~(symbol (name fname))
           [~@(when (empty? specs) '(&)) prev#]
           (let [in# (set (apply concat prev#))]
             (if (utils/errored? in#)
               prev#
               (try
                 (let [grouped# (utils/group-by-conform ~dispatch (seq in#))
                       missing# (filter #(not (contains? grouped# %)) '(~@syms))
                       out# (into (set (if (empty? missing#)
                                         (map (fn [{:syms ~(vec syms)}] ~@body)
                                              (utils/cartesian-product (dissoc grouped# nil)))
                                         (let [m# (select-keys '~(zipmap syms specs) missing#)
                                               sp# (s/spec* (cons `s/or (interleave (keys m#) (vals m#))))]
                                           (map (partial s/explain-data sp#)
                                                (get grouped# nil)))))
                                  (apply concat (vals (dissoc grouped# nil))))]
                   (data/diff in# out#))
                 (catch Exception e#
                   (let [fname# ~(keyword (str *ns*) (str fname))
                         out# #{{:defm/fname fname# :defm/anomaly e#}}]
                     (data/diff in# out#))))))))))
