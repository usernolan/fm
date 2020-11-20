(ns fm.form.sequent
  (:require
   [clojure.spec.alpha :as s]))

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
