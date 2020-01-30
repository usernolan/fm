(ns fm.testing.reporting
  (:require [clojure.alpha.spec :as s]
            [fm.macros :refer [defm]]))

(s/def ::total-fns nat-int?)
(s/def ::total-num-tests nat-int?)
(s/def ::total-time-elapsed-ms nat-int?)

(s/def ::group-result-data-total
  (s/schema
   {
    :fns             ::total-fns
    :num-tests       ::total-num-tests
    :passed          (s/map-of symbol? map?)
    :time-elapsed-ms ::total-time-elapsed-ms}))

(s/def ::group-result-data
  (s/schema
   {:failed (s/map-of symbol? ::check-result)
    :passed (s/map-of symbol? ::check-result)
    :total ::group-result-data-total}))

(s/def ::update-result-total-args
  (s/tuple
   ::total-num-tests
   ::total-time-elapsed-ms
   ::group-result-data-total))

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
  (clojure.pprint/pprint pass-or-fail)
  (clojure.pprint/pprint num-tests)
  (clojure.pprint/pprint time-elapsed-ms)
  (clojure.pprint/pprint total)
  (->
   total
   (update :fns (fnil inc 0))
   (update (if (pass-or-fail :pass) :passed :failed) (fnil inc 0))
   (update :num-tests + num-tests)
   (update :time-elapsed-ms + time-elapsed-ms)))


(defmulti group-result-data-reducer
  (fn [_acc {{pass? :pass?} :stc/ret}]
    (if pass? :pass :fail)))

(defmethod group-result-data-reducer
  :pass
  [acc {:keys [stc/ret sym] :as result}]
  (let [{:keys [num-tests time-elapsed-ms]} ret]
    (->
     acc
     (assoc-in [:passed sym] (cleanup-check-result result))
     (update
      :total
      (partial update-result-total :pass num-tests time-elapsed-ms)))))

(defmethod group-result-data-reducer
  :fail
  [acc {:keys [stc/ret sym] :as result}]
  (let [{:keys [num-tests failed-after-ms]} ret]
    (->
     acc
     (assoc-in [:failed sym] (cleanup-check-result result))
     (update
      :total
      (partial update-result-total :fail num-tests failed-after-ms)))))

(defm initial-group-result-data
  ^{:fm/doc "Initial data for a group-result-data reduction"
    :fm/ret ::group-result-data}
  []
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

(defm group-result-data
  ^{:fm/doc "Takes a sequence of check results and groups the results into
              :passed and :failed maps keyed by the function symbol"
    :fm/args ::check-results
    :fm/ret ::group-result-data}
  [check-results]
  (->>
   check-results
   (reduce group-result-data-reducer (initial-group-result-data))
   (finalize-result)))

(defm explain-run
  ^{:fm/doc "Prints test result explanation to *out*. Use group-result-data
             instead if you prefer an aggregated data representation of the
             result."
    :fm/args ::group-result-data
    :fm/ret nil?}
  [{passed :passed
    failed :failed
    total  :total}]
  (let [failed-count (count failed)]
    (prn (str "fms:     " (:fns total)))
    (prn (str "tests:   " (:num-tests total)))
    (prn (str "elapsed: " (float (/ (:time-elapsed-ms total) 1000)) "s"))
    (prn (str "passed:  " (count passed)))
    (prn (str "failed:  " failed-count))
    (prn)
    (prn (str "Overall Result: " (if (> failed-count 1) "FAIL" "PASS")))))

