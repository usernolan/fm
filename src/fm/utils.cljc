(ns fm.utils
  (:require
   [clojure.alpha.spec.gen :as gen]
   [clojure.alpha.spec :as s]))

(defn arg-fmt*
  [arg]
  (cond
    (vector? arg) (mapv arg-fmt* arg)
    (map? arg)    (update arg :as (fnil identity (gensym "arg__")))
    :else         arg))

(defn arg-sym*
  [arg]
  (cond
    (vector? arg) (mapv arg-sym* arg)
    (map? arg)    (:as arg)
    :else         arg))

(defn any?*
  [x]
  (if (vector? x)
    (mapv any?* x)
    `any?))

(defn schema-keys*
  [schema]
  (let [f #(cond (keyword? %) [%] (map? %) (keys %))]
    (cond
      (vector? schema)     (distinct (mapcat f schema))
      (map? schema)        (keys schema)
      (or
       (keyword? schema)
       (s/schema? schema)) (schema-keys* (second (s/form schema)))
      (seqable? schema)    (schema-keys* (second schema)))))

(s/def ::spec-form
  (fn [x]
    (and
     (seqable? x)
     (= (first x) `s/spec))))

(defn spec-form?
  [x]
  (s/valid? ::spec-form x))

(defn args-spec-form*
  [x]
  (cond
    (map? x)     `(s/select [~x] ~(vec (schema-keys* x)))
    (and
     (vector? x)
     (empty? x)) `(s/spec #{[]})
    (vector? x)  `(s/tuple ~@(mapv args-spec-form* x))
    :else        x))

(defn ret-spec-form
  [x]
  (cond
    (keyword? x)             `(when (s/form ~x) (s/get-spec ~x))
    (map? x)                 `(s/select [~x] ~(vec (schema-keys* x)))
    (or
     (symbol? x)
     (set? x)
     (and
      (seqable? x)
      (not (spec-form? x)))) `(s/spec ~x)
    :else                    x))

(def fn-symbols-set
  #{'fn 'fn* `fn 'fm})

(s/def ::fn-form
  (fn [x]
    (and
     (seqable? x)
     (fn-symbols-set (first x)))))

(defn fn-form?
  [x]
  (s/valid? ::fn-form x))

(defn anomaly-form
  [x]
  (cond
    (fn-form? x) `~x
    (symbol? x)  x
    :else        `(fn [~'_] ~x)))

(defn trace-form
  [x]
  (cond
    (fn-form? x) `~x
    (symbol? x)  x
    :else        `(fn [~'_] (prn ~x))))

(defn reduce*
  ([recur? f init xs]
   (reduce* recur? f f init xs))
  ([recur? cf f init xs]
   (reduce
    (fn [acc x]
      (if (recur? acc x)
        (cf (reduce* recur? cf f acc x) x)
        (f acc x)))
    init
    xs)))

;; TODO: 20200126 namespaced keys work strangely in
;; the current `s/select`. the following returns true for
;; `(s/valid? :fm.anomaly/args {:fm.anomaly/type nil})`
#_(s/def :fm.anomaly/args
    (s/select
     [{:fm.anomaly/type #{:fm.anomaly/args}}]
     [:fm.anomaly/type]))

(s/def :fm.anomaly/args
  (s/and
   map?
   (fn [{t :fm.anomaly/type}]
     (= t :fm.anomaly/args))))

(defn args-anomaly?
  [x]
  (s/valid? :fm.anomaly/args x))

(s/def :fm.anomaly/ret
  (s/and
   map?
   (fn [{t :fm.anomaly/type}]
     (= t :fm.anomaly/ret))))

(defn ret-anomaly?
  [x]
  (s/valid? :fm.anomaly/ret x))

(s/def :fm.anomaly/throw
  (s/and
   map?
   (fn [{t    :fm.anomaly/type
         data :fm.anomaly/data}]
     (and
      (= t :fm.anomaly/throw)
      (instance? Throwable data)))))

(defn throw-anomaly?
  [x]
  (s/valid? :fm.anomaly/throw x))

(defn contains-anomaly?*
  [recur? xs]
  (reduce*
   recur?
   (fn [acc _]
     (if (true? acc)
       (reduced true)
       false))
   (fn [_ x]
     (if (s/valid?
          (s/or
           ::args  :fm.anomaly/args
           ::ret   :fm.anomaly/ret
           ::throw :fm.anomaly/throw)
          x)
       (reduced true)
       false))
   false
   xs))

(defn args-anomaly-recur?
  [_ x]
  (vector? x))

(defn args-anomaly?*
  [args]
  (contains-anomaly?* args-anomaly-recur? args))

(s/def :fm.anomaly/received
  (s/and
   vector?
   not-empty
   args-anomaly?*))

(defn received-anomaly?
  [x]
  (s/valid? :fm.anomaly/received x))

(s/def :fm/anomaly
  (s/or
   :fm.anomaly/args     :fm.anomaly/args
   :fm.anomaly/ret      :fm.anomaly/ret
   :fm.anomaly/throw    :fm.anomaly/throw
   :fm.anomaly/received :fm.anomaly/received))

(defn anomaly?
  [x]
  (s/valid? :fm/anomaly x))

(defn fm-form
  [{:keys [fm/sym fm/args-form fm/body]}]
  (let [sym       (or sym (gensym "fm__"))
        metadata  (meta args-form)
        args-fmt  (arg-fmt* args-form)
        args-syms (arg-sym* args-fmt)
        args-spec (->>
                   (:fm/args metadata (any?* args-syms))
                   ((fn [x] (if (vector? x) x [x])))
                   (args-spec-form*))
        ret-sym   (gensym "ret__")
        ret-spec  (ret-spec-form (:fm/ret metadata `any?))
        anomaly   (anomaly-form (:fm/anomaly metadata `identity))
        trace     (when-let [trace (:fm/trace metadata)]
                    (->>
                     (if (true? trace) `prn trace)
                     (trace-form)))]

    `(let [args# ~args-spec
           ret#  ~ret-spec
           anom# ~anomaly]

       ^{:fm/sym     '~sym
         :fm/args    args#
         :fm/ret     ret#
         :fm/anomaly anom#}

       (fn ~(symbol (name sym))
         ~args-fmt

         ~(when (:fm/trace metadata)
            `(~trace #:fm.trace{:sym  '~sym
                                :args ~args-syms}))

         (if (args-anomaly?* ~args-syms)
           (anom# ~args-syms)

           (if (s/valid? args# ~args-syms)
             (try
               (let [~ret-sym (do ~@body)]

                 ~(when (:fm/trace metadata)
                    `(~trace #:fm.trace{:sym '~sym
                                        :ret ~ret-sym}))

                 (cond
                   (s/valid? :fm/anomaly ~ret-sym)
                   (anom# ~ret-sym)

                   (s/valid? ret# ~ret-sym)
                   ~ret-sym

                   :else
                   (anom# #:fm.anomaly{:type :fm.anomaly/ret
                                       :sym  '~sym
                                       :args ~args-syms
                                       :data (s/explain-data ret# ~ret-sym)})))

               (catch Throwable e#
                 (anom# #:fm.anomaly{:type :fm.anomaly/throw
                                     :sym  '~sym
                                     :args ~args-syms
                                     :data e#})))

             (anom# #:fm.anomaly{:type :fm.anomaly/args
                                 :sym  '~sym
                                 :args ~args-syms
                                 :data (s/explain-data args# ~args-syms)})))))))

(defn genform
  [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      x
      (gen/generate (s/gen (first c))))))

(comment

  (require '[fm.utils :as fm.utils] :reload-all)
  (require '[fm.macros :refer [defm fm]])
  (require '[clojure.alpha.spec :as s]
           '[clojure.alpha.spec.gen :as gen])

  (def test-vec1 ['a ['b 'c]])
  (def bad-vec1 [1 [2 3]])

  (s/def ::cat1
    (s/cat
     :a symbol?
     :nested1 (s/cat :b symbol? :c symbol?)))

  (s/explain ::cat1 test-vec1) ; unexpected...

  (s/def ::cat2
    (s/cat
     :a symbol?
     :G__29109 (s/tuple symbol? symbol?))) ; gensym'd

  (s/explain ::cat2 test-vec1) ; works
  (s/conform ::cat2 test-vec1) ; might be brtual to work with in practice

  (s/explain-data ::cat2 bad-vec1) ; only first problem reported

  (s/def ::tuple1
    (s/tuple symbol? (s/tuple symbol? symbol?)))

  (s/explain ::tuple1 test-vec1)
  (s/conform ::tuple1 test-vec1) ; maintains structure

  (s/explain-data ::tuple1 bad-vec1) ; agh, beautiful

  (defm inc1
    ^{:fm/args int?
      :fm/ret  int?}
    [n]
    (inc n))

  (inc1 1)
  (inc1 'a)

  (macroexpand '(defm inc1
                  ^{:fm/args int?
                    :fm/ret  int?}
                  [n]
                  (inc n)))

  (s/def ::n int?)

  (defm inc2
    ^{:fm/args (s/cat :n int? :m ::n)
      :fm/ret  ::n}
    [[n m]]
    (inc (+ n m)))

  (inc2 [1 2])
  (inc2 'a) ; ah! inherits `defn` behavior but may present an opportunity...
  (inc2 [1 'a]) ; potentially confusing `:path` (see aside)

  (def inc2-anom (inc2 [1 'a]))
  (def inc2-problems (:clojure.spec.alpha/problems (:fm.anomaly/data inc2-anom)))
  (def inc2-args (:fm.anomaly/args inc2-anom)) ; alternatively (:clojure.spec.alpha/value (:fm.anomaly/data inc2-anom))
  (get-in inc2-args (:path (first inc2-problems))) ; nil because of how `s/cat` conforms

  (defm inc3
    ^{:fm/args (s/tuple int? ::n)
      :fm/ret  ::n}
    [[n m]]
    (inc (+ n m)))

  (inc3 [1 2])
  (inc3 [1 'a])

  (def inc3-anom (inc3 [1 'a]))
  (def inc3-problems (:clojure.spec.alpha/problems (:fm.anomaly/data inc3-anom)))
  (def inc3-args (:fm.anomaly/args inc3-anom))
  (get-in inc3-args (:path (first inc3-problems))) ; potentially useful

  (->>
   (meta inc3)                 ; fm        -> meta
   (:fm/args)                  ; meta      -> args-spec
   (s/gen)                     ; args-spec -> generator
   (gen/sample)                ; generator -> args seq
   (map (partial apply inc3))) ; args seq  -> ret seq

  (defm inc4
    ^{:fm/args (s/cat :n int? :m int?)
      :fm/ret  (s/and int? pos?)}
    [[n m]]
    (let [i (inc (+ n m))]
      (* i i)))

  (inc4 [1 2])
  (inc4 [1 -2]) ; ah! nice
  (inc4 [-1 -3])

  (defm inc5
    ^{:fm/args [(fn [n] (= n 1))]
      :fm/ret  (fn [n] (= n 2))}
    [n]
    (inc n))

  (inc5 1)
  (inc5 'a)

  (macroexpand '(defm inc5
                  ^{:fm/args [(fn [n] (= n 1))]
                    :fm/ret  (fn [n] (= n 2))}
                  [n]
                  (inc n)))

  (s/explain (:fm/args (meta inc5)) [1])
  (s/explain (:fm/args (meta inc5)) [2])

    ;; does `s/spec` nest?
  (s/valid? (s/spec (s/cat :n int? :m int?)) [1 2]) ; seemingly
  (s/conform (s/cat :n int? :m int?) [1 2])
  (s/conform (s/spec (s/cat :n int? :m int?)) [1 2]) ; maybe...
  (s/conform (s/spec (s/spec (s/cat :n int? :m int?))) [1 2]) ; i wonder why this isn't a no-op

  (defm inc6
    ^{:fm/args #{1}
      :fm/ret  #{2}}
    [n]
    (inc n))

  (inc6 1)
  (inc6 2) ; sweet
  )
