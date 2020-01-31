(ns fm.test.report
  (:require [clojure.alpha.spec :as s]
            [clojure.pprint :as pp]
            [fm.macros :refer [defm]]
            [fm.test.check :as check]))

(defn cleanup-check-result
  [result]
  (->
   result
   (update :spec s/describe)))

(defn update-result-total
  ^{:fm/doc ""
    :fm/args ::update-result-total-args
    :fm/ret  ::group-result-data-total}
  [pass-or-fail num-tests time-elapsed-ms total]
  (->
   total
   (update :fns (fnil inc 0))
   (update (if (= pass-or-fail :pass) :passed :failed) (fnil inc 0))
   (update :num-tests + num-tests)
   (update :time-elapsed-ms + time-elapsed-ms)))


(defmulti group-result-data-reducer
  (fn [_acc {{pass? :pass?} :clojure.spec.test.check/ret}]
    (if pass? :pass :fail)))

(defmethod group-result-data-reducer
  :pass
  [acc {:keys [clojure.spec.test.check/ret sym] :as result}]
  (prn "check results: " result)
  (let [{:keys [num-tests time-elapsed-ms]} ret]
    (->
     acc
     (assoc-in [:passed sym] (cleanup-check-result result))
     (update
      :total
      (partial update-result-total :pass num-tests time-elapsed-ms)))))

(defmethod group-result-data-reducer
  :fail
  [acc {:keys [clojure.spec.test.check/ret sym] :as result}]
  (let [{:keys [num-tests failed-after-ms]} ret]
    (->
     acc
     (assoc-in [:failed sym] (cleanup-check-result result))
     (update
      :total
      (partial update-result-total :fail num-tests failed-after-ms)))))

(def initial-group-result-data
  {:passed {}
   :failed {}
   :total  {:failed          0
            :fns             0
            :num-tests       0
            :passed          0
            :time-elapsed-ms 0}})

(defn set-final-pass-fail
  [{:keys [failed] :as total}]
  (assoc total :pass? (<= failed 0)))

(defn finalize-result
  [grouped-result]
  (update grouped-result :total set-final-pass-fail))

(defn group-result-data
  "Takes a sequence of check results and groups the results into :passed and
  :failed maps keyed by the function symbol"
  [check-results]
  (->>
   check-results
   (reduce group-result-data-reducer initial-group-result-data)
   (finalize-result)))

(defn explain-run
  "Prints test result explanation to *out*. Use group-result-data instead if you
  prefer an aggregated data representation of the result."
  [{passed :passed
    failed :failed
    total  :total}]
  (let [failed-count (count failed)]
    (pp/pprint total)
    (prn)
    (pp/pprint (str "Test Result: " (if (> 0 failed-count) "FAIL" "PASS")))))
