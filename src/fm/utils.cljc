(ns fm.utils
  (:require
   [clojure.spec-alpha2.gen :as gen]
   [clojure.spec-alpha2 :as s]))

(s/def ::anomaly (s/select [{:fm/anomaly any?}] [*]))
(def anomaly? (partial s/valid? ::anomaly))

(s/def ::args-anomaly (s/and ::anomaly (fn [{:keys [fm/args]}] (nil? args))))
(def args-anomaly? (partial s/valid? ::args-anomaly))

;; TODO: Improve clarity of form handling
;; TODO: Improve destructuring support
;; e.g. (s/and ::ns1/k ::ns2/xyz)
;; e.g. (s/cat ...)
(defn schema-keys
  [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond (vector? schema)        (distinct (mapcat f schema))
          (map? schema)           (keys schema)
          (or (keyword? schema)
              (s/schema? schema)) (schema-keys (second (s/form schema)))
          (seqable? schema)       (schema-keys (second schema)))))

(defn spec-form
  [x]
  (cond (or (vector? x) (map? x))     `(s/select ~x ~(vec (schema-keys x)))
        (keyword? x)                  `(when (s/form ~x) (s/get-spec ~x))
        (or (seqable? x) (symbol? x)) `(s/spec ~x)))

(defn args-form
  [x sym]
  (if (or (vector? x) (map? x))
    `{:keys ~(mapv symbol (schema-keys x))
      :as ~sym}
    sym))

(defn anomaly-form
  [x]
  (cond (and (list? x)
             (= 'fn (first x))) `~x
        (symbol? x)             x
        :else                   `(fn [~'_] ~x)))

(defn fm-form
  [{:keys [fm/fname fm/args fm/ret fm/body]}]
  (let [name-sym (or fname (gensym "fm__"))
        args-sym (or (:fm/as (meta (first body)))
                     (if (keyword? args)
                       (symbol (name args))
                       '$))
        anomaly  (or (:fm/anomaly (meta (first body)))
                     `identity)]
    `(let [args# ~(spec-form args)
           ret# ~(spec-form ret)
           anom# ~(anomaly-form anomaly)]
       ^{:fm/args args#
         :fm/ret ret#}
       (fn ~(symbol (name name-sym))
         [~(args-form args args-sym)]
         (if (s/valid? ::anomaly ~args-sym)
           (anom# ~args-sym)
           (if (s/valid? args# ~args-sym)
             (try
               (let [res# (do ~@body)]
                 (cond
                   (s/valid? ::anomaly res#)
                   (anom# res#)

                   (s/valid? ret# res#)
                   res#

                   :else
                   (anom# #:fm{:fname '~name-sym
                               :args ~args-sym
                               :anomaly (s/explain-data ret# res#)})))
               (catch Throwable e#
                 (anom# #:fm{:fname '~name-sym
                             :args ~args-sym
                             :anomaly e#})))
             (anom# #:fm{:fname '~name-sym
                         :anomaly (s/explain-data args# ~args-sym)})))))))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))

(comment

  (require
   '[clojure.spec-alpha2 :as s]
   '[clojure.spec-alpha2.gen :as gen]
   '[fm.macros :refer [fm defm]]
   :reload-all)

  ;; inline fn specs, default args symbol $
  (defm inc_ number? number? (inc $))
  (inc_ 1)

  ;; anomaly 1: bad input
  (inc_ 'a)

  ;; anomalies are data
  (:fm/anomaly (inc_ 'a)) ; output of s/explain-data
  (:fm/fname (inc_ 'a))   ; qualified name of function

  ;; anomaly 2: bad output
  (defm bad-output number? symbol? (inc $))
  (bad-output 1)

  ;; anomaly contains the input and output values
  (:fm/anomaly (bad-output 1))    ; output of s/explain-data
  (:fm/fname (bad-output 1))      ; qualified name of function
  (:fm/args (bad-output 1))       ; args that caused anomalistic output
  (:clojure.spec.alpha/value      ; output that triggered anomaly
   (:fm/anomaly (bad-output 1)))

  ;; anomaly 3: throws
  (defm throws any? any? (throw (Exception. "darn!")))
  (throws nil)

  (:fm/anomaly (throws [])) ; an #error { ... }
  (:fm/fname (throws []))   ; qualified name of function
  (:fm/args (throws []))    ; args that caused throw

  ;; anonymous fm
  ((fm number? number? (inc $)) 1)
  ((fm number? number? (inc $)) 'a)

  ;; rebind args symbol
  (defm inc_ number? number?
    ^{:fm/as n}
    (inc n))

  ;; custom anomaly handling
  (defm custom-anomaly any? any?
    ^{:fm/anomaly "dang!"}
    (throw (Exception.)))

  (custom-anomaly nil)

  (defm custom-anomaly2 any? any?
    ^{:fm/anomaly (fn [a]
                    (prn (:fm/fname a))
                    (prn (:fm/args a))
                    a)}
    (throw (Exception.)))

  (custom-anomaly2 'xyz)

  (s/def ::http-req
    (s/select
     [{:body string?}]
     [*]))

  (gen/generate (s/gen ::http-req))
  (gen/sample (s/gen ::http-req))

  (defm echo
    ::http-req        ; spec from registry
    [{:body string?}] ; inline select [*]
    (let [{:keys [body] :as resp} http-req]
      {:body (str "echo " body)}))

  ;; compiled args and ret specs accessible via metadata
  (:fm/args (meta echo))
  (:fm/ret (meta echo))

  ;; toward properties
  (gen/sample (s/gen (:fm/args (meta echo))))
  (gen/sample (s/gen (:fm/ret (meta echo))))
  (map echo (gen/sample (s/gen (:fm/args (meta echo)))))

  (defm exclaim
    [{:body string?}]
    [{:body string?}]
    {:body (str body "!")})

  ;; "wire up"
  (-> (gen/generate (s/gen ::http-req))
      (echo)
      (exclaim)
      )

  ;; anomaly pass-through
  (-> {:causes :anomaly :in :echo}
      (echo)
      (exclaim)
      #_(:fm/fname) ;; anomaly source
      )

  (s/def ::http-resp
    (s/select
     [{:statusCode #{200 503}
       :body string?}]
     [*]))

  ;; `sink`
  (defm response ::http-req ::http-resp
    ^{:fm/anomaly (fn [{:keys [fm/fname] :as anomaly}]
                    (prn (str "Bad happened in " fname "!"))
                    #_(logger/log! anomaly)
                    {:statusCode 503 :body "I failed!"})}
    (assoc http-req :statusCode 200))

  (-> {:body "hi"}
      (echo)
      (exclaim)
      (response))

  (-> {:causes :anomaly}
      (echo)
      (exclaim)
      (response))

  ;; experimental (broken)
  ;; defining ret spec dynamically as a function of args
  (defm echo-refined
    ::http-req
    [{:body #(= % (str "echo " http-req))}]
    (let [{:keys [body] :as resp} http-req]
      {:body (str "echo " body)}))

  )
