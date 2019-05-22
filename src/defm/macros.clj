(ns defm.macros
  (:require
   [clojure.spec-alpha2 :as s]
   [defm.utils :as utils]))

(defmacro defm
  "The `defm` macro."
  [fname in-specs out-specs & body]
  (let [in-dispatch (keyword (str *ns*) (str fname utils/dispatch-tag "in__"))
        in-syms (if (map? in-specs) (map first in-specs) (map symbol in-specs))
        in-specs (if (map? in-specs) (vals in-specs) in-specs)
        out-dispatch (keyword (str *ns*) (str fname utils/dispatch-tag "out__"))
        out-syms (if (map? out-specs) (map first out-specs) (map symbol out-specs))
        out-specs (if (map? out-specs) (vals out-specs) out-specs)]
    `(do (s/def ~in-dispatch (s/* (s/or ~@(interleave in-syms in-specs)
                                        :clojure.spec.alpha/problems :clojure.spec.alpha/problemed
                                        :defm/anomalies :defm/anomalied
                                        nil identity)))
         (s/def ~out-dispatch (s/or ~@(interleave out-syms out-specs)
                                    :clojure.spec.alpha/problems :clojure.spec.alpha/problemed
                                    :defm/anomalies :defm/anomalied
                                    nil identity))
         (defn ~(symbol (name fname))
           [~@(when (empty? in-specs) '(&)) in#]
           (let [m# (utils/group-by-conform
                     ~in-dispatch
                     (seq (cond (map? in#)  (apply concat (vals in#))
                                (coll? in#) in#
                                :else       [in#])))
                 mf# (or ~(:defm/merge (meta in-specs))
                         (partial utils/deep-merge-with conj))]
             (if (utils/errored? m#)
               m#
               (try
                 (let [missing# (filter #(not (contains? m# %)) '(~@in-syms))]
                   (if (empty? missing#)
                     (map (comp (partial s/conform ~out-dispatch)
                                (fn [{:syms ~(vec in-syms) :as args#}]
                                  (try
                                    ~@body
                                    (catch Exception e#
                                      {:defm/fname ~(keyword (str *ns*) (str fname))
                                       :defm/args args#
                                       :defm/anomaly e#}))))
                          (utils/cartesian-product (dissoc m# nil)))
                     (let [missing# (select-keys '~(zipmap in-syms in-specs) missing#)
                           sp# (s/spec* (cons `s/or (interleave (keys missing#) (vals missing#))))
                           out# [:clojure.spec.alpha/problems
                                 (mapv (partial s/explain-data sp#) (get m# nil))]]
                       (seq [(mf# m# out#)]))))
                 (catch Exception e#
                   (let [out# [:defm/anomalies
                               [{:defm/fname ~(keyword (str *ns*) (str fname))
                                 :defm/args in#
                                 :defm/anomaly e#}]]
                     (seq [(mf# m# out#)]))))))))))
