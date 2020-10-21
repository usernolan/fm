(ns fm.form.sequent
  (:require
   [clojure.spec.alpha :as s]
   [fm.form.lib :as lib]))

(s/def ::signature
  (s/cat
   ::left vector?
   ::right vector?
   ::body (s/* any?)))

(s/def ::signatures
  (s/+
   (s/spec ::signature)))

(s/def ::definition
  (s/cat
   ::simple-symbol? (s/? simple-symbol?)
   ::rest
   (s/alt
    ::signature  ::signature
    ::signatures ::signatures)))

