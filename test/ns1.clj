(ns fm.test.ns1
  (:require [fm.macros :refer [fm defm]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as stest]))

(defm defm-test-with-args-and-ret
  ^{:fm/args (s/cat :n number?)
    :fm/ret number?}
  [n]
  (inc n))

(defm defm-test-with-invalid-ret
  ^{:fm/args (s/cat :n number?)
    :fm/ret number?}
  [n]
  nil)

(defm defm-test-with-args-and-without-ret
  ^{:fm/args (s/cat :n number?)}
  [n]
  (inc n))

(defm defm-test-without-args-and-ret
  [n]
  (inc n))

(defn defn-test-inc
  [n]
  (inc n))

(comment

  (meta @#'defm-test-with-args-and-ret)
  (gen/generate (gen/gen-for-pred int?))
  (gen/generate (gen/gen-for-pred int?))

  (require '[fm.testing :as t])

  (s/fdef defm-test-with-args-and-ret :args (s/cat :n number?) :ret) 
  (s/get-spec 'fm.test.ns1/defm-test-with-args-and-ret)
  #_(:spec :clojure.spec.test.check/ret :sym :failure)
  (:failure (first (stest/check 'fm.test.ns1/defm-test-with-args-and-ret)))
  (first (stest/check 'fm.test.ns1/defm-test-with-args-and-ret))

  (keys (s/registry))

  )
