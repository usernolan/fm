(ns fm.usage
  (:require
   [clojure.alpha.spec :as s]
   [clojure.alpha.spec.gen :as gen]
   [clojure.alpha.spec.test :as stest]
   [fm.anomaly :as fm.anomaly]
   [fm.api :as fm]
   [fm.macro :refer [fm defm]]
   [fm.test.check :as fm.check]))

  ;; control group
(defn fn1
  [n]
  (inc n))

(fn1 1)
(fn1 'a)

(meta fn1)
(meta #'fn1)

  ;; fm is just (with-meta fn ,,,)
(macroexpand '(fm [n] (inc n)))

(defm fm1
  [n]
  (inc n))

(fm1 1)
(fm1 'a) ; `:fm.anomaly/throw`

(meta fm1)
(meta #'fm1)

(defm fm2
  ^{:fm/args number?}
  [n]
  (inc n))

(fm2 1)
(fm2 'a) ; `:fm.anomaly/args`

  ;; anomalies are data
(:fm.anomaly/sym  (fm2 'a)) ; where'd you come from, anomaly?
(:fm.anomaly/spec (fm2 'a)) ; oof, what happened?
(:fm.anomaly/args (fm2 'a)) ; and those `:fm.anomaly/args` were?
(:fm.anomaly/data (fm2 'a)) ; why are they anomalous?
  ;; thanks, `fm.usage/fm2`.

(def failed-val
  (comp
   :val
   first
   :clojure.spec.alpha/problems
   :fm.anomaly/data))

(def failed-pred
  (comp
   :pred
   first
   :clojure.spec.alpha/problems
   :fm.anomaly/data))

(failed-val       (fm2 'a))
(failed-pred      (fm2 'a))

  ;; fishing for `:fm.anomaly/ret`
(defm bad-ret
  ^{:fm/args number?
    :fm/ret  symbol?} ; the bait
  [n]
  (inc n))

(bad-ret 1) ; we caught one

(:fm.anomaly/sym  (bad-ret 1))
(:fm.anomaly/spec (bad-ret 1))
(:fm.anomaly/args (bad-ret 1))
(:fm.anomaly/data (bad-ret 1))
(failed-val       (bad-ret 1))
(failed-pred      (bad-ret 1))

  ;; `:fm.anomaly/rel`
(defm bad-rel
  ^{:fm/args number?
    :fm/ret  number?
    :fm/rel  (fn [{[n] :args ret :ret}]
               (> n ret))} ; the bait
  [n]
  (inc n))

(bad-rel 1)

(:fm.anomaly/sym  (bad-rel 1))
(:fm.anomaly/spec (bad-rel 1))
(:fm.anomaly/args (bad-rel 1))
(:fm.anomaly/data (bad-rel 1))
(failed-val       (bad-rel 1))
(failed-pred      (bad-rel 1))

  ;; `fm.anomaly/throw` again
(defm throws
  []
  (throw (ex-info "darn!" {:severity :big-time})))

(throws)

(:fm.anomaly/sym   (throws))
(:fm.anomaly/spec  (throws))
(:fm.anomaly/args  (throws))
(:fm.anomaly/data  (throws))
(failed-val        (throws))
(failed-pred       (throws))
(ex-data
 (:fm.anomaly/data (throws)))

  ;; anonymous fm
((fm ^{:fm/args number?} [n] (inc n)) 1)
((fm ^{:fm/args number?} [n] (inc n)) 'a)

  ;; variadic signatures aren't ready yet, but otherwise...
(defm fm3
  ^{:fm/args [number? [float? [int? int?]] {:body even?}]}
  [x [y [z w]] {:keys [body]}]
  (+ x y z w body))

(fm3 1/2 [2.5 [36 1]] {:body 2})

  ;; anomalies are reported as a sequence of `:clojure.spec.alpha/problems`
(->>
 (fm3 'a [2.5 ['b 1]] {:body 2})
 (:fm.anomaly/data)
 (:clojure.spec.alpha/problems))

  ;; doc
(defm docd
  ^{:fm/doc "Is documented."}
  [n]
  (inc n))

(meta docd)
(meta #'docd)

  ;; anomaly handling
(defm handled1
  ^{:fm/handler "dang!"}
  []
  (throw (ex-info "darn!" {})))

(handled1)

(defn log!
  [a]
  (prn "logging!")
  a)

(defm handled2
  ^{:fm/handler log!}
  []
  (throw (ex-info "darn!" {})))

(handled2)

(defm handled3
  ^{:fm/handler (fn [anomaly] (prn anomaly))}
  []
  (throw (ex-info "darn!" {})))

(handled3)

  ;; thin trace facility
(defm traced1
  ^{:fm/trace true} ; defaults to `clojure.core/prn`
  []
  (rand))

(traced1)

(defm traced2
  ^{:fm/trace (fn [{:keys [fm.trace/sym fm.trace/args fm.trace/ret]}]
                (prn sym (symbol "trace:") args ret))}
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

(defm traced4
  ^{:fm/trace true}
  []
  (inc (traced1)))

(traced4)

(defm conformed1
  ^{:fm/doc     "conformer beware."
    :fm/conform #{:fm/args}
    :fm/args    (s/coll-of int? :into #{})}
  [ns]
  (conj ns 4))

(conformed1 [1 2 3])

(defm conformed2
  ^{:fm/conform #{:fm/args}
    :fm/args    [{:a (s/coll-of int? :into #{})
                  :b (s/coll-of string? :into [])}
                 (s/coll-of boolean? :into #{})]}
  [{:keys [a b] :as x} c]
  [a b x c])

(conformed2
 {:a [1 2 2 3 3 3]
  :b #{"b1" "b2" "b3"}}
 [true false false true])

(s/def ::req
  (s/select
   [{:body (s/and string? not-empty)}]
   [*]))

(s/def ::resp
  (s/select
   [{:status #{200 400 500}
     :body   (s/and string? not-empty)}]
   [*]))

(gen/generate (s/gen ::req))
(gen/sample (s/gen ::resp))

(s/def ::echo-resp
  (s/and
   ::resp
   (fn [{:keys [status body]}]
     (and
      (= status 200)
      (clojure.string/starts-with? body "echo: ")))))

(defm echo
  ^{:fm/args ::req
    :fm/ret  ::echo-resp}
  [{:keys [body]}]
  {:status 200
   :body   (str "echo: " body)})

(macroexpand '(defm echo
                ^{:fm/args ::req
                  :fm/ret  ::echo-resp}
                [{:keys [body]}]
                {:status 200
                 :body   (str "echo: " body)}))

(failed-pred (echo nil))
(failed-pred (echo {}))
(failed-pred (echo {:body nil}))
(failed-pred (echo {:body ""}))
(failed-pred (echo {:body "hi"}))
(echo {:body "hi"})

  ;; function metadata
(meta echo)

  ;; toward properties
(->>
 (meta echo)                 ; fm        -> meta
 (:fm/args)                  ; meta      -> args spec
 (s/gen)                     ; args spec -> generator
 (gen/sample)                ; generator -> args seq
 (map (partial apply echo))) ; args seq  -> ret seq

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
  (comp
   exclaim
   echo))

(def first-arg-spec
  (comp
   s/get-spec
   second
   s/form
   :fm/args))

  ;; wait are they wirable?
(=
 (:fm/ret        (meta echo))
 (first-arg-spec (meta exclaim)))

(->>
 (s/gen ::req)
 (gen/sample)
 (map echo-exclaim))

(->>
 {:body "hi"}
 (echo)
 (exclaim))

(->>
 {:causes :anomaly}
 (echo)
 (exclaim)) ; surprise `:fm.anomaly/received`

(->>
 {:causes :anomaly}
 (echo)
 (exclaim)
 (s/conform :fm/anomaly)
 (first))

  ;; the anomaly occurs in `echo`, and is
  ;; received and propagated by `exclaim`
(->>
 {:causes :anomaly}
 (echo)
 (exclaim)
 (first)            ; ask the anomaly
 (:fm.anomaly/sym)) ; "where did you occur?"

  ;; `:fm/anomaly` sum
(s/describe :fm/anomaly)
(s/describe :fm.anomaly/args)
(s/describe :fm.anomaly/ret)
(s/describe :fm.anomaly/rel)
(s/describe :fm.anomaly/throw)
(s/describe :fm.anomaly/received)

  ;; anomaly handling
(def http-500
  {:status 500
   :body   "darn!"})

(defn http-400
  [source]
  {:status 400
   :body   (if source
             (str "bad request in " source "!")
             "bad request!")})

(defn http-anomaly-handler
  [{:keys [fm.anomaly/args]
    :as   anomaly}]
  (cond
    ;; propagate any previous anomalous response
    (s/valid? ::resp (first args))
    (first args)
    ;; `:fm.anomaly/args` treated as bad request
    (s/valid? :fm.anomaly/args anomaly)
    (http-400 (:fm.anomaly/sym anomaly))
    ;; ...
    :else
    http-500))

(defm echo2
  ^{:fm/args    ::req
    :fm/ret     ::echo-resp
    :fm/handler http-anomaly-handler}
  [{:keys [body]}]
  {:status 200
   :body   (str "echo: " body)})

(echo2 {:body nil})

(defm exclaim2
  ^{:fm/args    ::echo-resp
    :fm/ret     ::exclaim-resp
    :fm/handler http-anomaly-handler}
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

(defm echo3
  ^{:fm/args ::req
    :fm/ret  ::resp
    :fm/rel  (fn [{[req] :args resp :ret}] ; same interface as `:fn` in `s/fdef`
               (=
                (:body resp)
                (str "echo: " (:body req))))}
  [{:keys [body]}]
  {:status 200
   :body   (str "echo: " body)})

(contains?
 (s/registry)
 (:fm/sym (meta echo3)))

(fm.check/fdef-fm! echo3) ; generates fdefs from fms

(contains?
 (s/registry)
 (:fm/sym (meta echo3)))

(->>
 (meta echo3)
 (:fm/sym)
 (stest/check)) ; generates tests from fdefs

(s/def ::n int?)

(defm fm4
  ^{:fm/args (s/tuple int? ::n)
    :fm/ret  ::n}
  [[n m]]
  (inc (+ n m)))

(fm4 [1 2])
(fm4 [1 'a])

(defm fm5
  ^{:fm/args (s/tuple int? int?)
    :fm/ret  (s/and int? pos?)}
  [[n m]]
  (let [i (inc (+ n m))]
    (* i i)))

(fm5 [1 2])
(fm5 [1 -2])
(fm5 [-1 -3])

(defm fm6
  ^{:fm/args [(fn [n] (= n 1))]
    :fm/ret  (fn [n] (= n 2))}
  [n]
  (inc n))

(fm6 1)
(fm6 'a)

(defm fm7
  ^{:fm/args #{1}
    :fm/ret  #{2}}
  [n]
  (inc n))

(fm7 1)
(fm7 'a)

(defm fm8
  ^{:fm/args {:body int?}
    :fm/ret  {:body int?}}
  [{:keys [body]}]
  {:body (inc body)})

(fm8 {:body 1})
(fm8 {:a 1})
(fm8 {:body 'a})
(fm8 nil)
(fm8 'a)

(defm fm9
  [a [b [c [d [e f] g]]] h]
  [a b c d e f g h])

(fm9 1 [2 [3 [4 [5 6] 7]]] 8)
(fm9 1 [2 [3 [4 [5 {:fm.anomaly/spec :fm.anomaly/args}] 7]]] 8)
