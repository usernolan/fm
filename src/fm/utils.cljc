(ns fm.utils
  (:require
   [clojure.spec-alpha2.gen :as gen]
   [clojure.spec-alpha2 :as s]
   [clojure.walk :as walk]))

(s/def ::anomaly
  (s/select
   [{:fm/anomaly any?}]
   [*]))

(def anomaly?
  (partial s/valid? ::anomaly))

(s/def ::args-anomaly
  (s/and
   ::anomaly
   (fn [{:keys [fm/anomaly]}]
     (vector? anomaly))))

(def args-anomaly?
  (partial s/valid? ::args-anomaly))

(defn schema-keys
  [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond
      (vector? schema)     (distinct (mapcat f schema))
      (map? schema)        (keys schema)
      (or
       (keyword? schema)
       (s/schema? schema)) (schema-keys (second (s/form schema)))
      (seqable? schema)    (schema-keys (second schema)))))

(s/def ::spec-form
  (fn [x]
    (and
     (seqable? x)
     (=
      (first x)
      `s/spec))))

(defn spec-form
  [x]
  (cond
    (vector? x)      (mapv spec-form x)
    (keyword? x)     `(when (s/form ~x) (s/get-spec ~x))
    (map? x)         `(s/select [~x] ~(vec (schema-keys x)))
    (or
     (symbol? x)
     (and
      (not
       (s/valid?
        ::spec-form
        x))
      (seqable? x))) `(s/spec ~x)
    :else            x))

(defn fmt-arg
  [arg]
  (cond
    (vector? arg) (mapv fmt-arg arg)
    (map? arg)    (if (:as arg)
                    arg
                    (assoc arg :as (gensym "arg__")))
    :else         arg))

(defn args-syms-walk
  [x]
  (if (map? x)
    (:as x)
    x))

(def fn-symbols-set
  #{'fn 'fn* `fn 'fm})

(s/def ::fn-form
  (fn [x]
    (and
     (seqable? x)
     (fn-symbols-set (first x)))))

(defn anomaly-form
  [x]
  (cond
    (s/valid? ::fn-form x) `~x
    (symbol? x)            x
    :else                  `(fn [~'_] ~x)))

(defn trace-form
  [x]
  (cond
    (s/valid? ::fn-form x) `~x
    (symbol? x)            x
    :else                  `(fn [~'_] (prn ~x))))

(defn zip-syms-specs
  [syms specs]
  (mapv
   (fn [sym spec]
     (if (symbol? sym)
       [sym spec]
       (zip-syms-specs sym spec)))
   syms
   specs))

(defn rreduce
  [recurse? f init xs]
  (reduce
   (fn [acc x]
     (let [x (if (recurse? x)
               (rreduce recurse? f init x)
               x)]
       (f recurse? init xs acc x)))
   init
   xs))

(defn args-anomaly?*
  [args]
  (rreduce
   vector?
   (fn [_ _ _ _ arg]
     (cond
       (true? arg)              (reduced true)
       (s/valid? ::anomaly arg) (reduced true)
       :else                    false))
   false
   args))

(s/def ::zipped-arg-spec
  (fn [v]
    (and
     (vector? v)
     (= (count v) 2)
     (or
      (s/spec? (second v))
      (and
       (keyword? (second v))
       (s/get-spec (second v)))))))

(s/def ::args-recurse
  (fn [v]
    (and
     (vector? v)
     (not (s/valid? ::zipped-arg-spec v)))))

(defn args-valid?*
  [zipped]
  (rreduce
   (partial s/valid? ::args-recurse)
   (fn [_ _ _ _ x]
     (cond
       (false? x)                          (reduced false)
       (or
        (true? x)
        (and
         (s/valid? ::zipped-arg-spec x)
         (s/valid? (second x) (first x)))) true
       :else                               (reduced false)))
   true
   zipped))

(defn explain*
  [zipped]
  (rreduce
   (partial s/valid? ::args-recurse)
   (fn [_ _ _ acc x]
     (if (s/valid? ::zipped-arg-spec x)
       (conj acc (s/explain-data (second x) (first x)))
       (conj acc x)))
   []
   zipped))

(defn fm-form
  [{:keys [fm/sym fm/args-form fm/body]}]
  (let [sym        (or sym (gensym "fm__"))
        metadata   (meta args-form)
        args-fmt   (mapv fmt-arg args-form)
        args-syms  (walk/postwalk args-syms-walk args-fmt)
        args-specs (some->>
                    (or
                     (:fm/args metadata)
                     (if (empty? args-form)
                       nil
                       (walk/postwalk
                        (fn [x] (if (symbol? x) `any? x))
                        args-syms)))
                    ((fn [specs]
                       (if (not (vector? specs))
                         [specs]
                         specs)))
                    (rreduce
                     vector?
                     (fn [_ _ _ acc spec]
                       (conj acc (spec-form spec)))
                     []))
        zipped     (zip-syms-specs args-syms args-specs)
        ret-spec   (->>
                    (or (:fm/ret metadata) `any?)
                    (spec-form))
        anomaly    (->>
                    (or (:fm/anomaly metadata) `identity)
                    (anomaly-form))
        trace      (when-let [trace (:fm/trace metadata)]
                     (->>
                      (if (true? trace) `prn trace)
                      (trace-form)))
        res-sym    (gensym "res__")]

    `(let [args# ~args-specs
           ret#  ~ret-spec
           anom# ~anomaly]

       ^{:fm/sym     '~sym
         :fm/args    args#
         :fm/ret     ret#
         :fm/anomaly anom#}

       (fn ~(symbol (name sym))
         ~args-fmt

         ~(when (:fm/trace metadata)
            `(~trace {:fm/sym  '~sym
                      :fm/args ~args-syms}))

         (if (args-anomaly?* ~args-syms)
           (anom# ~args-syms)

           (if (args-valid?* ~zipped)
             (try
               (let [~res-sym (do ~@body)]

                 ~(when (:fm/trace metadata)
                    `(~trace {:fm/sym '~sym
                              :fm/res ~res-sym}))

                 (cond
                   (s/valid? ::anomaly ~res-sym)
                   (anom# ~res-sym)

                   (s/valid? ret# ~res-sym)
                   ~res-sym

                   :else
                   (anom# #:fm{:sym     '~sym
                               :args    ~args-syms
                               :anomaly (s/explain-data ret# ~res-sym)})))

               (catch Throwable e#
                 (anom# #:fm{:sym     '~sym
                             :args    ~args-syms
                             :anomaly e#})))

             (anom# #:fm{:sym     '~sym
                         :args    ~args-syms
                         :anomaly (explain* ~zipped)})))))))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))

(comment

  (require
   '[clojure.spec-alpha2.gen :as gen]
   '[clojure.spec-alpha2 :as s]
   '[fm.macros :refer [fm defm]]
   '[fm.utils :as fm]
   :reload-all)

  ;; control group
  (defn inc_
    [n]
    (inc n))

  (inc_ 1)
  (inc_ 'a)

  ;; fm is just fn
  (macroexpand '(defm inc_
                  [n]
                  (inc n)))

  (defm inc_
    [n]
    (inc n))

  (inc_ 1)
  (inc_ 'a) ;; this is anomaly 3: throw

  (defm inc_
    ^{:fm/args number?}
    [n]
    (inc n))

  (inc_ 1)
  (inc_ 'a) ;; anomaly 1: argument(s)

  ;; anomalies are data
  (:fm/sym (inc_ 'a))     ; qualified function symbol
  (:fm/args (inc_ 'a))    ; args that triggered the anomaly
  (:fm/anomaly (inc_ 'a)) ; output of `s/explain-data`, same shape as args

  ;; anomaly 2: return
  (defm bad-output
    ^{:fm/args number?
      :fm/ret  symbol?}
    [n]
    (inc n))

  (bad-output 1)

  ;; anomaly contains the input and output values
  (:fm/sym (bad-output 1))       ; qualified function symbol
  (:fm/args (bad-output 1))      ; args that caused anomalistic output
  (:fm/anomaly (bad-output 1))   ; output of `s/explain-data`
  (:clojure.spec.alpha/value
   (:fm/anomaly (bad-output 1))) ; output that triggered anomaly

  ;; another anomaly 3: throw
  (defm throws
    []
    (throw (Exception. "darn!")))

  (throws)

  (:fm/sym (throws))     ; qualified function symbol
  (:fm/args (throws))    ; args that caused throw, same shape as args
  (:fm/anomaly (throws)) ; an #error { ... }

  ;; anonymous fm
  ((fm ^{:fm/args number?} [n] (inc n)) 1)
  ((fm ^{:fm/args number?} [n] (inc n)) 'a)

  ;; variadic signatures aren't ready yet, but otherwise...
  (defm add_
    ^{:fm/args [number? [float? [int? int?]]]}
    [x [y [z w]]]
    (+ x y z w))

  (add_ 1/2 [2.5 [36 3]])

  ;; anomalies are reported according to the shape of the arguments
  (add_ 'a [2.5 ['b 3]])

  (first (:fm/args    (add_ 'a [2.0 ['b 4]])))
  (first (:fm/anomaly (add_ 'a [2.0 ['b 4]])))
  (first (second (second (:fm/args    (add_ 'a [2.0 ['b 4]])))))
  (first (second (second (:fm/anomaly (add_ 'a [2.0 ['b 4]])))))

  ;; custom anomaly handling
  (defm custom-anomaly
    ^{:fm/anomaly "dang!"}
    []
    (throw (Exception. "darn!")))

  (custom-anomaly)

  (defn log!
    [a]
    (prn "logging!")
    a)

  (defm custom-anomaly2
    ^{:fm/anomaly log!}
    []
    (throw (Exception. "darn!")))

  (custom-anomaly2)

  (defm custom-anomaly3
    ^{:fm/anomaly (fn [anomaly] (prn anomaly))}
    []
    (throw (Exception. "darn!")))

  (custom-anomaly3)

  (s/def ::http-req
    (s/select
     [{:body (s/and string? not-empty)}]
     [*]))

  (s/def ::http-resp
    (s/select
     [{:status #{200 400 503}
       :body   (s/and string? not-empty)}]
     [*]))

  (gen/generate (s/gen ::http-req))
  (gen/sample (s/gen ::http-resp))

  (s/def ::echo-resp
    (s/and
     ::http-resp
     (fn [{:keys [status body]}]
       (and
        (= status 200)
        (clojure.string/starts-with? body "echo: ")))))

  (defm echo
    ^{:fm/args ::http-req
      :fm/ret  ::echo-resp}
    [{:keys [body]}]
    {:status 200
     :body   (str "echo: " body)})

  (echo nil)
  (echo {})
  (echo {:body nil})
  (echo {:body ""})
  (echo {:body "hi"})

  ;; fm metadata; AdS/CFT
  (meta echo)

  ;; toward properties
  (->>
   (:fm/args (meta echo))
   (first)
   (s/gen)
   (gen/sample)
   (map echo))

  (s/def ::exclaim-resp
    (s/and
     ::echo-resp
     (fn [{:keys [status body]}]
       (and
        (= status 200)
        (clojure.string/ends-with? body "!")))))

  (defm exclaim
    ^{:fm/args ::echo-resp
      :fm/ret  ::exclaim-resp}
    [{:keys [body]}]
    {:status 200
     :body   (str body "!")})

  ;; "wire up"
  (def echo-exclaim
    (comp exclaim echo))

  (->>
   (gen/sample (s/gen ::http-req))
   (into [] (map echo-exclaim)))

  (->
   {:body "hi"}
   (echo)
   (exclaim))

  (->
   {:causes :anomaly}
   (echo)
   (exclaim)) ;; surprise anomaly 4: received

  ;; the anomaly occured in `echo`, and then was
  ;; received and propagated by `exclaim`
  (->
   {:causes :anomaly}
   (echo)
   (exclaim)
   (first)
   (:fm/sym))

  ;; summary          |  anomaly handler receives:
  ;; anomaly 1: args  => anomaly map; `:fm/anomaly` is a vector
  ;; anomaly 2: ret   => anomaly map; `:fm/anomaly` is a map
  ;; anomaly 3: throw => anomaly map; `:fm/anomaly` is throwable
  ;; anomaly 4: recd  => argument vector containing one or more anomaly maps

  ;; anomaly handling
  (def http-503
    {:status 503
     :body   "darn!"})

  (defn http-400
    [source]
    {:status 400
     :body   (if source
               (str "bad request in " source "!")
               "bad request!")})

  (defn http-anomaly-handler
    [{:keys [fm/args]
      :as   anomaly}]
    (cond
      (s/valid? ::http-resp (first args))       (first args)                 ;; propagate previous anomalistic response
      (s/valid? :fm.utils/args-anomaly anomaly) (http-400 (:fm/sym anomaly)) ;; anomaly 1: argument(s) may indicate an anomalistic (bad) request
      :else                                     http-503))

  (defm echo2
    ^{:fm/args    ::http-req
      :fm/ret     ::echo-resp
      :fm/anomaly http-anomaly-handler}
    [{:keys [body]}]
    {:status 200
     :body   (str "echo: " body)})

  (echo2 {:body nil})

  (defm exclaim2
    ^{:fm/args    ::echo-resp
      :fm/ret     ::exclaim-resp
      :fm/anomaly http-anomaly-handler}
    [{:keys [body]}]
    {:status 200
     :body   (str body "!")})

  (->>
   {:body "hi"}
   (echo2)
   (exclaim2))

  (->>
   {:causes :anomaly}
   (echo2)
   (exclaim2))

  ;; thin trace facility
  (defm traced
    ^{:fm/trace true} ;; defaults to `clojure.core/prn`
    []
    (rand))

  (traced)

  (defm traced2
    ^{:fm/trace
      (fn [{:keys [fm/sym fm/args fm/res]}]
        (prn sym (symbol "trace:") args res))}
    []
    (rand))

  (traced2)

  (def state-atom (atom 0))

  (defm traced3
    ^{:fm/trace @state-atom}
    []
    (swap! state-atom inc)
    (rand))

  (traced3)
  (traced3)

  ;; experimental (broken)
  ;; defining ret spec dynamically as a function of args
  (defm echo-refined
    ^{:fm/args ::http-req
      :fm/ret  {:body (fn [resp]
                        (= (:body resp)
                           (str "echo: " (:body req))))}}
    [{:keys [body] :as req}]
    {:status 200
     :body   (str "echo: " body)}))
