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
(inc_ 'a) ; this is anomaly 3: throw

(defm inc_
  ^{:fm/args number?}
  [n]
  (inc n))

(inc_ 1)
(inc_ 'a) ; anomaly 1: argument(s)

  ;; anomalies are data
(:fm/sym     (inc_ 'a)) ; qualified function symbol
(:fm/args    (inc_ 'a)) ; args that triggered the anomaly
(:fm/anomaly (inc_ 'a)) ; args reduced with `s/explain-data`, same shape as args

  ;; what kind of anomaly are you?
(->>
 (inc_ 'a)
 (s/conform
  ::fm/anomaly)
 (first)) ; i'm an anomaly 1: args

  ;; anomaly 2: return
(defm bad-ret
  ^{:fm/args number?
    :fm/ret  symbol?}
  [n]
  (inc n))

(bad-ret 1)

  ;; anomaly contains the args and ret values
(:fm/sym      (bad-ret 1))  ; qualified function symbol
(:fm/args     (bad-ret 1))  ; args that caused anomalous ret
(:fm/anomaly  (bad-ret 1))  ; output of `s/explain-data`
(:clojure.spec.alpha/value
 (:fm/anomaly (bad-ret 1))) ; return value that triggered anomaly

  ;; anomaly 3 again: throw
(defm throws
  []
  (throw (Exception. "darn!")))

(throws)

(:fm/sym     (throws)) ; qualified function symbol
(:fm/args    (throws)) ; args that caused the throw, same shape as args
(:fm/anomaly (throws)) ; a throwable

  ;; anonymous fm
((fm ^{:fm/args number?} [n] (inc n)) 1)
((fm ^{:fm/args number?} [n] (inc n)) 'a)

  ;; variadic signatures aren't ready yet, but otherwise...
(defm add_
  ^{:fm/args [number? [float? [int? int?]] {:body even?}]}
  [x [y [z w]] {:keys [body]}]
  (+ x y z w body))

(add_ 1/2 [2.5 [36 1]] {:body 2})

  ;; anomalies are reported according to the shape of the arguments
(add_ 'a [2.5 ['b 1]] {:body 2})

(first (:fm/args    (add_ 'a [2.5 ['b 1]] {:body 2})))
(first (:fm/anomaly (add_ 'a [2.5 ['b 1]] {:body 2})))
(first (second (second (:fm/args    (add_ 'a [2.5 ['b 1]] {:body 2})))))
(first (second (second (:fm/anomaly (add_ 'a [2.5 ['b 1]] {:body 2})))))

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

  ;; look away!
(def failed-pred
  (comp
   :pred
   first
   :clojure.spec.alpha/problems
   first
   :fm/anomaly))

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
 (meta echo)
 (:fm/args)
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
  (comp
   exclaim
   echo))

  ;; wait are they wirable?
(=
 (first (:fm/args (meta exclaim)))
 (:fm/ret         (meta echo)))

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
 (exclaim)) ; surprise anomaly 4: received

  ;; the anomaly occurs in `echo`, and is
  ;; received and propagated by `exclaim`
(->>
 {:causes :anomaly}
 (echo)
 (exclaim)
 (first)    ; ask the anomaly
 (:fm/sym)) ; "where did you occur?"

  ;; summary          |  anomaly handler receives:
  ;; anomaly 1: args  => anomaly map; `:fm/anomaly` is a vector
  ;; anomaly 2: ret   => anomaly map; `:fm/anomaly` is a map
  ;; anomaly 3: throw => anomaly map; `:fm/anomaly` is throwable
  ;; anomaly 4: recd  => argument vector containing one or more anomaly maps

(s/describe ::fm/anomaly)          ; anomaly sum
(s/describe ::fm/args-anomaly)     ; anomaly 1, args
(s/describe ::fm/ret-anomaly)      ; anomaly 2, ret
(s/describe ::fm/throw-anomaly)    ; anomaly 3, throw
(s/describe ::fm/received-anomaly) ; anomaly 4, received

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
    ;; propagate any previous anomalous response
    (s/valid? ::http-resp (first args))
    (first args)
    ;; anomaly 1: args, treated as bad request
    (s/valid? :fm.utils/args-anomaly anomaly)
    (http-400 (:fm/sym anomaly))
    ;; ...
    :else
    http-503))

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
  ^{:fm/trace true} ; defaults to `clojure.core/prn`
  []
  (rand))

(traced)

(defm traced2
  ^{:fm/trace
    (fn [{:keys [fm/sym fm/args fm/ret]}]
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

  ;; experimental (broken)
  ;; defining ret spec dynamically as a function of args
(defm echo-refined
  ^{:fm/args ::http-req
    :fm/ret  {:body (fn [resp]
                      (= (:body resp)
                         (str "echo: " (:body req))))}}
  [{:keys [body] :as req}]
  {:status 200
   :body   (str "echo: " body)})
