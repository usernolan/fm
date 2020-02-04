(ns fm.test.usage
  (:require
   [clojure.alpha.spec.test :as stest]
   [fm.macros :refer [defm]]
   [fm.test.check :as fm.check]
   [fm.test.report :as fm.report]))

;; This should pass
(defm add1
  ^{:fm/args int?
    :fm/ret  int?}
  [n]
  (inc n))

;; This should fail
(defm add5
  ^{:fm/args nil? ;; let's cause an exception to be thrown by check
    :fm/ret  int?}
  [n]
  (+ n 5))

;; This should also fail
(defm expected-spec-failure
  ^{:fm/args any? ;; let's cause an exception to be thrown by check
    :fm/ret  int?}
  [x]
  x)

(def namespaces-to-test
  '[fm.test.usage])

(def check-result
  (fm.check/check! namespaces-to-test)) ; raw stest/check seq

(def check-result-data
  (fm.report/group-result-data check-result))

(:total  check-result-data) ; a summary!
(:pass?  check-result-data) ; did all fms pass?
(:passed check-result-data) ; map of passed fns keyed by the fn symbol
(:failed check-result-data) ; map of failed fns keyed by the fn symbol

(get-in check-result-data [:total :fns])             ; how many fns did we test?
(get-in check-result-data [:total :num-tests])       ; how many tests were generated?
(get-in check-result-data [:total :passed])          ; how many passed?
(get-in check-result-data [:total :failed])          ; how many failed
(get-in check-result-data [:total :time-elapsed-ms]) ; how long did it take?

  ;; print report to *out*
(fm.report/explain-run check-result-data)
