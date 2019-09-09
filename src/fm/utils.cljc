(ns fm.utils
  (:require
   [clojure.spec-alpha2 :as s]))

(s/def :fm/anomaly (s/and map? #(contains? % :fm/anomaly)))
(def anomaly? (partial s/valid? :fm/anomaly))

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
        (symbol? x) x
        :else `(fn [~'_] ~x)))

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
         (if (s/valid? :fm/anomaly ~args-sym)
           (anom# ~args-sym)
           (if (s/valid? args# ~args-sym)
             (try
               (let [res# (do ~@body)]
                 (if (s/valid? ret# res#)
                   res#
                   (anom# #:fm{:fname '~name-sym
                               :args ~args-sym
                               :anomaly (s/explain-data ret# res#)})))
               (catch Throwable e#
                 (anom# #:fm{:fname '~name-sym
                             :args ~args-sym
                             :anomaly e#})))
             (anom# #:fm{:fname '~name-sym
                         :anomaly (s/explain-data args# ~args-sym)})))))))

(comment

  (require '[clojure.spec-alpha2 :as s])
  (require '[clojure.spec-alpha2.gen :as gen])
  (require '[fm.macros :refer [fm defm]])

  ;; inline fn specs, default args symbol $
  (defm inc_ number? number? (inc $))
  (inc_ 1)

  ;; anomaly 1: bad input
  (inc_ 'a)

  ;; anomaly 2: bad output
  (defm bad-return number? symbol? (inc $))
  (bad-return 1)

  ;; anomaly 3: throws
  (defm throws
    fn?
    any?
    ^{:fm/as f}
    (f))
  (throws #(throw (Exception. "darn!")))

  ;; anonymous fm
  ((fm number? number? (inc $)) 1)
  ((fm number? number? (inc $)) 'a)

  ;; rebind args symbol
  (defm inc_ number? number? ^{:fm/as n} (inc n))

  ;; custom anomaly handling
  (defm throws2
    fn?
    any?
    ^{:fm/anomaly "dang!" ; implicit `do`
      :fm/as f}
    (f))
  (throws2 #(throw (Exception. "shoot!")))

  (s/def ::http-req
    (s/select
     [{:body string?}]
     [*]))

  (gen/generate (s/gen ::http-req))
  (gen/sample (s/gen ::http-req))

  (defm echo
    ::http-req        ; from registry
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
  (-> {:body "hi"}
      (echo)
      (exclaim))

  ;; anomaly pass-through
  (-> {:causes :anomaly}
      (echo)
      (exclaim)
      #_(:fm/fname) ; source of anomaly
      )

  (s/def ::http-resp
    (s/select
     [{:statusCode #{200 503}
       :body string?}]
     [*]))

  ;; `sink`
  (defm sink ::http-req ::http-resp
    ^{:fm/anomaly (fn [anom]
                    (prn "Bad happened! Logging somewhere...")
                    #_(logger/log! anom)
                    {:statusCode 503 :body "I failed!"})}
    (assoc http-req :statusCode 200))

  (-> {:body "hi"}
      (echo)
      (exclaim)
      (sink))

  (-> {:causes :anomaly}
      (echo)
      (exclaim)
      (sink))

  ;; experimental (broken)
  ;; defining ret spec dynamically as a function of args
  (defm echo-refined
    ::http-req
    [{:body #(= % (str "echo " http-req))}]
    (let [{:keys [body] :as resp} http-req]
      {:body (str "echo " body)}))

  )
