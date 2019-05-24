 (ns defm.macros
  (:require
   [clojure.spec-alpha2 :as s]
   [defm.utils :as utils]))

(defmacro defm
  "The `defm` macro."
  [fname args-schema ret-schema & body]
  (let [qualified-fname-sym (symbol (str *ns*) (str fname))
        f (or (:defm/f (meta args-schema)) `merge)
        comment (:defm/comment (meta args-schema))
        doc-string (apply str [args-schema " -> " ret-schema
                               (when comment (str "\n\n" comment))])
        args-ks (vec (utils/schema-keys args-schema))
        ret-ks (vec (utils/schema-keys ret-schema))
        res (or (:defm/res (meta ret-schema)) (first ret-ks))
        metadata {:defm/args `(s/schema ~args-schema)
                  :defm/ret `(s/schema ~ret-schema)
                  :defm/res res
                  :defm/f f
                  :doc doc-string}]
    `(let [args-select# (s/select ~(:defm/args metadata) ~args-ks)
           ret-select# (s/select ~(:defm/ret metadata) ~ret-ks)]
       (defn ~(symbol (name fname))
         ~metadata
         [{:keys ~args-ks :as args#}]
         (if (utils/anomaly? args#)
           args#
           (if (s/valid? args-select# args#)
             (try
               (let [res# (do ~@body)]
                 (if (s/valid? ~res res#)
                   (let [ret# (~f args# {~res res#})]
                     (if (s/valid? ret-select# ret#)
                       ret#
                       #:defm{:fname '~qualified-fname-sym
                              :args args#
                              :res {~res res#}
                              :anomaly (s/explain-data ret-select# ret#)}))
                   #:defm{:fname '~qualified-fname-sym
                          :args args#
                          :anomaly (s/explain-data ~res res#)}))
               (catch Exception e#
                 #:defm{:fname '~qualified-fname-sym
                        :args args#
                        :anomaly e#}))
             #:defm{:fname '~qualified-fname-sym
                    :args args#
                    :anomaly (s/explain-data args-select# args#)}))))))
