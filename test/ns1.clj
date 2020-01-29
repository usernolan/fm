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

  (require '[fm.testing :as t])

  (defm fn-to-test
    ^{:fm/args int?
      :fm/ret int?}
    [n]
    (inc n))

  ;; This appears to work if you _don't_ unquote args
  ;; but check will fail.
  ;;
  ;; If you _do_ unquote args, you won't be able to evaluate the form,
  ;; you'll get an exception saying that the reader can't place the object.
  (defn eval-fmdef!
    ^{:fm/doc "Takes a fn that has fm metadata on it and passes it through to s/fdef."
      :fm/args :fm/meta
      :fm/ret symbol?}
    [{:keys [:fm/sym :fm/args :fm/ret :fm/rel]}]
    (eval `(s/fdef ~sym
             :args (s/or ~args any?)
             :ret not-anomaly?)))

  (defmacro macro-fmdef!
    "Takes a fn that has fm metadata on it and passes it through to s/fdef."
    [fm-metadata]
    `(s/fdef (unquote (:fm/sym ~fm-metadata))
       :args (s/or (unquote (:fm/args ~fm-metadata)) any?)
       :ret fm.utils/not-anomaly?))

  (def m (meta @#'fn-to-test))
  (def sym (:fm/sym m))
  (def args (:fm/args m))

  ;; ------ CASE eval
  (eval-fmdef! m)
  (s/describe (s/get-spec 'fm.test.ns1/fn-to-test))
  (first (stest/check 'fm.test.ns1/defm-test-with-args-and-ret))

  ;; ------- CASE macro
  (macro-fmdef! sym args)
  (s/describe (s/get-spec 'fm.test.ns1/fn-to-test))
  (first (stest/check 'fm.test.ns1/defm-test-with-args-and-ret))

  ;; ------- CASE direct fdef
  (s/fdef defm-test-with-args-and-ret
           :args (:fm/args (meta @#'fn-to-test))
           :ret fm.utils/not-anomaly?)
  (s/describe (s/get-spec 'fm.test.ns1/fn-to-test))
  (first (stest/check 'fm.test.ns1/defm-test-with-args-and-ret))

  )
