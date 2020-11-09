(ns fm.form.sequent
  (:require
   [clojure.spec.alpha :as s]))

(s/def ::signature
  (s/cat
   ::left :fm.form/seqv
   ::right (s/? :fm.form/seqv)
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

(s/def ::keyword-or-destructure-form
  (s/or
   :keyword keyword?
   ::destructure-form (some-fn simple-symbol? vector? map?)))

(s/def ::destructure-map
  (s/map-of
   ::keyword-or-destructure-form
   ::keyword-or-destructure-form))

(s/def ::arg
  (s/or
   :keyword keyword?
   ::destructure-map ::destructure-map))

#_(s/def ::reversible-signature
    (s/cat
     ::left :fm.form/seqv
     ::right :fm.form/seqv ; ALT: `s/?`
     ::left->right any?
     ::right->left any?))

#_(s/def ::reversible-signatures
    (s/+
     (s/spec ::reversible-signature)))

#_(s/def ::reversible-definition
    (s/cat
     :fm.definition/simple-symbol (s/? simple-symbol?)
     :fm.definition/rest
     (s/alt
      ::reversible-signature ::reversible-signature
      ::reversible-signatures ::reversible-signatures)))
