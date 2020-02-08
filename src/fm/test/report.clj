(ns fm.test.report
  (:require
   [clojure.alpha.spec :as s]
   [clojure.pprint :as pp]
   [fm.test.check :as fm.check]
   [fm.utils :as fm]))

(defn cleanup-check-result
  [result]
  (update result :spec s/describe))

(defn update-result-total
  [pass-or-fail num-tests time-elapsed-ms total]
  (->
   total
   (update :fns (fnil inc 0))
   (update (if (= pass-or-fail :pass) :passed :failed) (fnil inc 0))
   (update :num-tests + num-tests)
   (update :time-elapsed-ms + time-elapsed-ms)))

(defn first-or-invalid
  [x]
  (if (coll? x) (first x) x))

(defmulti group-result-data-reducer
  (fn [_acc {:keys [clojure.spec.test.check/ret]}]
    (->>
     (s/conform :clojure.spec.test.check/ret ret)
     (first-or-invalid))))

(defmethod group-result-data-reducer :pass
  [acc {:keys [clojure.spec.test.check/ret sym] :as result}]
  (let [{:keys [num-tests time-elapsed-ms]} ret]
    (->
     acc
     (assoc-in [:passed sym] (cleanup-check-result result))
     (update
      :total
      (partial update-result-total :pass num-tests time-elapsed-ms)))))

(defmethod group-result-data-reducer :fail
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

(defn finalize-result
  [grouped-result]
  (assoc
   grouped-result
   :pass? (< (get-in grouped-result [:total :failed]) 1)))

(defn group-result-data
  "Takes a sequence of check results and groups the results into :passed and
  :failed maps keyed by the function symbol"
  [check-results]
  (->>
   check-results
   (reduce
    group-result-data-reducer
    initial-group-result-data)
   (finalize-result)))

(defn get-ex-data-from-failure
  "Pull the deeply nested exception from the failure and convert it into a map"
  [failure]
  (->
   failure
   (get-in
    [:clojure.spec.test.check/ret
     :result-data
     :clojure.test.check.properties/error])
   (Throwable->map)))

(defn get-anomalies-from-ex-data
  "All check failures will be the result of the fdef :ret coming back as an
  :fm.anomaly (indicating failure). We should then discard the outer spec
  as it will always be identical."
  [ex-data]
  (->>
   (get-in ex-data [:data :clojure.spec.alpha/problems])
   (map :val)))

(defmulti decompile-problem-specs
  "When viewing an inner anomaly spec problem, we want to see the compiled spec
  so that we can more easily understand what's going on for a given failure."
  (fn [anomaly]
    (->>
     (s/conform :fm/anomaly anomaly)
     (first-or-invalid))))

(defmethod decompile-problem-specs :fm.anomaly/throw
  [anomaly]
  anomaly)

(defmethod decompile-problem-specs :default
  [anomaly]
  (update-in
   anomaly
   [:fm.anomaly/data :clojure.spec.alpha/spec]
   s/form))

(defn extract-spec-problems-from-failure
  "Gets the deeply nested exception data from a failure"
  [failure]
  (->>
   failure
   (get-ex-data-from-failure)
   (get-anomalies-from-ex-data)
   (map decompile-problem-specs)))

(defn explain-failures
  "Pretty-print check failures"
  [check-result-failures]
  (doseq [[sym failure] check-result-failures]
    (prn)
    (pp/pprint (str "[FAIL] " sym))
    (pp/pprint (extract-spec-problems-from-failure failure))
    (prn)))

(defn explain-run
  "Prints test result explanation to *out*. Use group-result-data instead if you
  prefer an aggregated data representation of the result."
  [{:keys [pass? total] :as grouped-result-data}]
  (when-not pass?
    (explain-failures (:failed grouped-result-data)))
  (pp/pprint (str "Test Result: " (if pass? "PASS" "FAIL")))
  (pp/pprint total))
