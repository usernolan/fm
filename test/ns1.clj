(ns fm.test.ns1
  (:require [fm.macros :refer [fm defm]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as stest]))

(defm defm-test-with-args-and-ret
  ^{:fm/args int?
    :fm/ret int?}
  [n]
  (inc n))

(defm defm-test-with-invalid-ret
  ^{:fm/args [int?]
    :fm/ret int?}
  [n]
  nil)

(defm defm-test-with-args-and-without-ret
  ^{:fm/args [int?]}
  [n]
  (inc n))

(defm defm-test-without-args-and-ret
  [n]
  (inc n))

(defn defn-test-inc
  [n]
  (inc n))

(comment
  ;; Create a test
  (fm.macros/defm test-fn
    ^{:fm/args int? :fm/ret int?}
    [n]
    (inc n))

  ;; fmdef! will fail on args
  (fm.testing/fmdef! test-fn)

  ;; because we're asserting the args should conform to :fm/meta
  (clojure.alpha.spec/explain-data
   :fm/meta
   (meta test-fn))


  #_(require '[fm.testing :as t])                  ;; Pull in the testing namespace
  #_(fm.testing/fmdef! (meta @#'fm.test.ns1/defm-test-with-args-and-ret))
  #_(def result (t/check '[fm.test.ns1]))          ;; Invoke stest/check against all fm'd functions in namespace
  #_result
  #_(def result-data (t/group-result-data result)) ;; Aggregate the results
  #_(:fm.anomaly/data result-data)
  #_(:passed result-data)                          ;; Get the passed map (keyed by fm symbol)
  #_(:failed result-data)                          ;; Get the failures map (keyed by fm symbol)
  #_(:total result-data)                           ;; Get the total result data
  #_(t/explain-run result-data)                    ;; writes to console

  )
