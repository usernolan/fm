(ns fm.usage
  (:require
   [clojure.alpha.spec.gen :as gen]
   [clojure.alpha.spec :as s]
   [fm.macros :refer [fm defm]]
   [fm.utils :as fm]))

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
(inc_ 'a) ; this is anomaly 3: `:fm.anomaly/throw`

(defm inc_
  ^{:fm/args number?}
  [n]
  (inc n))

(inc_ 1)
(inc_ 'a) ; anomaly 1: `:fm.anomaly/args`

  ;; anomalies are data
(:fm.anomaly/sym  (inc_ 'a)) ; qualified function symbol
(:fm.anomaly/args (inc_ 'a)) ; args that triggered the anomaly
(:fm.anomaly/data (inc_ 'a)) ; output of `s/explain-data`

  ;; what kind of anomaly are you?
(->>
 (inc_ 'a)
 (s/conform :fm/anomaly)
 (first))

  ;; or just
(:fm.anomaly/spec (inc_ 'a))

  ;; anomaly 2: `:fm.anomaly/ret`
(defm bad-ret
  ^{:fm/args number?
    :fm/ret  symbol?}
  [n]
  (inc n))

(bad-ret 1)

  ;; anomaly contains the args and ret values
(:fm.anomaly/sym   (bad-ret 1))  ; qualified function symbol
(:fm.anomaly/args  (bad-ret 1))  ; args that caused anomalous ret
(:fm.anomaly/data  (bad-ret 1))  ; output of `s/explain-data`
(:clojure.spec.alpha/value
 (:fm.anomaly/data (bad-ret 1))) ; return value that triggered anomaly

  ;; anomaly 3 again: `:fm.anomaly/throw`
(defm throws
  []
  (throw (ex-info "darn!" {:severity :big-time})))

(throws)

(:fm.anomaly/sym   (throws))  ; qualified function symbol
(:fm.anomaly/args  (throws))  ; args that caused the throw, same shape as args
(:fm.anomaly/data  (throws))  ; a throwable
(ex-data
 (:fm.anomaly/data (throws))) ; uh...

  ;; anonymous fm
((fm ^{:fm/args number?} [n] (inc n)) 1)
((fm ^{:fm/args number?} [n] (inc n)) 'a)

  ;; variadic signatures aren't ready yet, but otherwise...
(defm add_
  ^{:fm/args [number? [float? [int? int?]] {:body even?}]}
  [x [y [z w]] {:keys [body]}]
  (+ x y z w body))

(add_ 1/2 [2.5 [36 1]] {:body 2})

  ;; anomalies are reported as a sequence of `:clojure.spec.alpha/problems`
(add_ 'a [2.5 ['b 1]] {:body 2})

  ;; custom anomaly handling
(defm custom-anomaly
  ^{:fm/handler "dang!"}
  []
  (throw (Exception. "darn!")))

(custom-anomaly)

(defn log!
  [a]
  (prn "logging!")
  a)

(defm custom-anomaly2
  ^{:fm/handler log!}
  []
  (throw (Exception. "darn!")))

(custom-anomaly2)

(defm custom-anomaly3
  ^{:fm/handler (fn [anomaly] (prn anomaly))}
  []
  (throw (Exception. "darn!")))

(custom-anomaly3)

(s/def ::http-req
  (s/select
   [{:body (s/and string? not-empty)}]
   [*]))

(s/def ::http-resp
  (s/select
   [{:status #{200 400 500}
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

(macroexpand '(defm echo
                ^{:fm/args ::http-req
                  :fm/ret  ::echo-resp}
                [{:keys [body]}]
                {:status 200
                 :body   (str "echo: " body)}))

(def failed-pred
  (comp
   :pred
   first
   :clojure.spec.alpha/problems
   :fm.anomaly/data))

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
 (meta echo)                 ; fm        => meta
 (:fm/args)                  ; meta      => args spec
 (s/gen)                     ; args spec => generator
 (gen/sample)                ; generator => args seq
 (map (partial apply echo))) ; args seq  => ret seq

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

  ;; wait are they wirable?
(=
 (s/form (s/get-spec (second (s/form (:fm/args (meta exclaim))))))
 (s/form (:fm/ret (meta echo))))

(->>
 (s/gen ::http-req)
 (gen/sample)
 (map echo-exclaim))

(->>
 {:body "hi"}
 (echo)
 (exclaim))

(->>
 {:causes :anomaly}
 (echo)
 (exclaim)) ; surprise anomaly 4: `:fm.anomaly/received`

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

(s/describe :fm/anomaly)          ; anomaly sum
(s/describe :fm.anomaly/args)     ; anomaly 1, args
(s/describe :fm.anomaly/ret)      ; anomaly 2, ret
(s/describe :fm.anomaly/throw)    ; anomaly 3, throw
(s/describe :fm.anomaly/received) ; anomaly 4, received

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
    (s/valid? ::http-resp (first args))
    (first args)

    ;; `:fm.anomaly/args` treated as bad request
    (s/valid? :fm.anomaly/args anomaly)
    (http-400 (:fm.anomaly/sym anomaly))

    ;; ...
    :else
    http-500))

(defm echo2
  ^{:fm/args    ::http-req
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

  ;; thin trace facility
(defm traced
  ^{:fm/trace true} ; defaults to `clojure.core/prn`
  []
  (rand))

(traced)

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
  (inc (traced)))

(traced4)

(s/def ::n int?)

(defm inc_
  ^{:fm/args (s/tuple int? ::n)
    :fm/ret  ::n}
  [[n m]]
  (inc (+ n m)))

(inc_ [1 2])
(inc_ [1 'a])

(defm inc_
  ^{:fm/args (s/tuple int? int?)
    :fm/ret  (s/and int? pos?)}
  [[n m]]
  (let [i (inc (+ n m))]
    (* i i)))

(inc_ [1 2])
(inc_ [1 -2])
(inc_ [-1 -3])

(defm inc_
  ^{:fm/args [(fn [n] (= n 1))]
    :fm/ret  (fn [n] (= n 2))}
  [n]
  (inc n))

(inc_ 1)
(inc_ 'a)

(defm inc_
  ^{:fm/args #{1}
    :fm/ret  #{2}}
  [n]
  (inc n))

(inc_ 1)
(inc_ 'a)

(defm inc_
  ^{:fm/args {:body int?}
    :fm/ret  {:body int?}}
  [{:keys [body]}]
  {:body (inc body)})

(inc_ {:body 1})
(inc_ {:a 1})
(inc_ {:body 'a})
(inc_ nil)
(inc_ 'a)

  ;; WIP `:fm/rel`
(defm echo-refined
  ^{:fm/args ::http-req
    :fm/ret  ::http-resp
    :fm/rel  (fn [{[req] :args resp :ret}] ; same interface as `:fn` in `s/fdef`
               (=
                (:body resp)
                (str "echo: " (:body req))))}
  [{:keys [body]}]
  {:status 200
   :body   (str "echo: " body)})

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Now let's test everything out 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(ns fm.usage.check
  (:require [fm.macros :refer [defm]]
            [fm.test.check :as fm.check]
            [fm.test.report :as fm.report]))

;; This should pass
(defm add1
  ^{:fm/args int?
    :fm/ret int?}
  [n]
  (inc n))

;; This should fail
(defm add5
  ^{:fm/args nil? ;; let's cause an exception to be thrown by check
    :fm/ret int?}
  [n]
  (+ n 5))

; This should also fail
(defm expected-spec-failure
  ^{:fm/args any? ;; let's cause an exception to be thrown by check
    :fm/ret int?}
  [x]
  x)

;; Can provide as many ns' as you want
(def namespaces-to-test '[fm.usage.check])

;; Run the test
;;
;; fm.test.check/check will map over all fm-enabled fns,
;; register them against s/fdef, and finally execute each with
;; clojure.alpha.spec.test/check. It returns a seq of all check
;; results for all defm's tested.
(def check-result
  (fm.check/check namespaces-to-test)) 

;; Group the results
;;
;; The check-result is nice if you wanna do your own spelunking, but
;; we got you covered there with test.check.report/group-result-data.
(def check-result-data
  (fm.report/group-result-data check-result))

(:pass? check-result-data)  ;; => did all fms pass?
(:passed check-result-data) ;; => map of passed fns keyed by the fn symbol
(:failed check-result-data) ;; => map of failed fns keyed by the fn symbol

(get-in check-result-data [:total :fns])             ;; => how many fns did we test
(get-in check-result-data [:total :num-tests])       ;; => how many aggregate tests were generated by test.check?
(get-in check-result-data [:total :passed])          ;; => how many passed?
(get-in check-result-data [:total :failed])          ;; => how many failed
(get-in check-result-data [:total :time-elapsed-ms]) ;; => how long did it take?

;; Get a report printed to *out* (open your repl)
(fm.report/explain-run check-result-data)

