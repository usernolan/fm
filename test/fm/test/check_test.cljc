(ns fm.test.check-test
  (:require
   [clojure.test :as t]
   [fm.macro :refer [defm]]
   [fm.test.check :as check]
   [fm.test.report :as report]))

;; Passing case
(defm defm-test-with-args-and-ret
  ^{:fm/args int?
    :fm/ret int?}
  [n]
  (inc n))

;; Passing case
(defm defm-test-with-args-and-without-ret
  ^{:fm/args int?}
  [n]
  (inc n))

;; Failing case
(defm defm-test-with-invalid-ret1
  ^{:fm/args [int?]
    :fm/ret int?}
  [n]
  nil)

;; Failing case
(defm defm-test-with-invalid-ret2
  ^{:fm/args [int?]
    :fm/ret nil?}
  [n]
  (inc n))

;; See #3 for info on resolving this no-op case.
#_(defm defm-test-without-args-and-ret
    [n]
    (inc n))

;; Should be skipped by test runner
(defn defn-test-inc
  [n]
  (inc n))

(def expected-passing-fns
  `[defm-test-with-args-and-ret
    defm-test-with-args-and-without-ret])

(def expected-failing-fns
  `[defm-test-with-args-and-ret
    defm-test-with-args-and-without-ret])

(def expected-skipped-fns
  `[defn-test-inc])

(t/deftest check-test
  (t/testing "single namespace"
    (let [expected-pass-count    (count expected-passing-fns)
          expected-fail-count    (count expected-failing-fns)
          total-fns              (+ expected-pass-count expected-fail-count)
          expected-min-test-runs (* 1000 expected-pass-count)
          check-results          (check/check '[fm.test.check-test])
          aggregate              (report/group-result-data check-results)]
      (t/is (= false (:pass? aggregate)))
      (t/is (= total-fns (+ expected-pass-count expected-fail-count)))
      (t/is (= total-fns (count check-results)))
      (t/is (= expected-pass-count
               (count (:passed aggregate))
               (get-in aggregate [:total :passed])))
      (t/is (= expected-fail-count
               (count (:failed aggregate))
               (get-in aggregate [:total :failed])))
      (t/is (= total-fns (get-in aggregate [:total :fns])))
      (t/is (<= expected-min-test-runs (get-in aggregate [:total :num-tests]))))))
