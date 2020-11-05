(ns fm.form.sequent
  (:require
   [clojure.spec.alpha :as s]
   [fm.form.lib :as lib]))

(s/def ::signature
  (s/cat
   ::left vector? #_:fm.form/seqv
   ::right vector? #_:fm.form/seqv
   :fm.form/body (s/* any?)))

(s/def ::signatures
  (s/+
   (s/spec ::signature)))

(s/def ::definition
  (s/cat
   :fm.definition/simple-symbol (s/? simple-symbol?)
   :fm.definition/rest
   (s/alt
    ::signature  ::signature
    ::signatures ::signatures)))
