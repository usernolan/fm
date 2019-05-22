(ns defm.macros
  (:require
   [clojure.spec-alpha2 :as s]))

(defmacro defm
  "The `defm` macro."
  [fname args-schema return-spec & body]
  (let [qualified-fname (symbol (str *ns*) (str fname))
        schema-name (keyword (str *ns*) (str fname "__schema__"))]
    `(do (s/def ~schema-name (s/schema ~args-schema))
         (defn ~(symbol (name fname))
           [args#]
           (if (s/valid? (s/select ~schema-name [~'*]) args#)
             (let [{:keys ~args-schema} args#]
               (try
                 (let [ret# (do ~@body)]
                   (if (s/valid? ~return-spec ret#)
                     (assoc args# ~return-spec ret#)
                     #:defm{:fname '~qualified-fname
                            :args args#
                            :anomaly (s/explain-data ~return-spec ret#)}))
                 (catch Exception e#
                   #:defm{:fname '~qualified-fname
                          :args args#
                          :anomaly e#})))
             #:defm{:fname '~qualified-fname
                    :args args#
                    :anomaly (s/explain-data (s/select ~schema-name [~'*]) args#)})))))


(comment
  ;; worth doing schema implicitly?
  ;; make work with more flexible schema
  ;; merge fn
  (defm
    ^{:defm/merge (fn [] nil)}
    [::si1 ::si2]
    ::so
    ;; (si1 si2 => so)
    )
  )
