 (ns defm.macros
  (:require
   [clojure.spec-alpha2 :as s]
   [defm.utils :as utils]))

(defmacro defm
  "The `defm` macro."
  [fname args-schema ret-spec & body]
  (let [qualified-fname-sym (symbol (str *ns*) (str fname))
        f (or (:defm/f (meta args-schema)) `merge)
        comment (:defm/comment (meta args-schema))
        doc-string (apply str (let [x [args-schema " -> " ret-spec]]
                                (if comment (concat x ["\n\n" comment]) x)))
        args-ks (vec (utils/schema-keys args-schema))
        metadata (let [x {:defm/args `(s/schema ~args-schema)
                          :defm/ret ret-spec
                          :defm/f f
                          :doc doc-string}]
                   (if comment (conj x {:defm/comment comment}) x))]
    `(let [args-select# (s/select ~(:defm/args metadata) ~args-ks)]
       (defn ~(symbol (name fname))
         ~metadata
         [{:keys ~args-ks :as args#}]
         (if (utils/anomaly? args#)
           args#
           (if (s/valid? args-select# args#)
             (try
               (let [ret# (do ~@body)]
                 (if (s/valid? ~ret-spec ret#)
                   (~f args# {~ret-spec ret#})
                   #:defm{:fname '~qualified-fname-sym
                          :args args#
                          :anomaly (s/explain-data ~ret-spec ret#)}))
               (catch Exception e#
                 #:defm{:fname '~qualified-fname-sym
                        :args args#
                        :anomaly e#}))
             #:defm{:fname '~qualified-fname-sym
                    :args args#
                    :anomaly (s/explain-data args-select# args#)}))))))
