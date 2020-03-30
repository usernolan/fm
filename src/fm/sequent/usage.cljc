(ns fm.sequent.usage
  (:require
   [clojure.alpha.spec :as s]
   [fm.sequent.macro
    :refer
    [defnonsequent
     defconsequent
     defmergesequent
     defsequent]]))

(s/def ::a #{::a})
(s/def ::b #{::b})

(defnonsequent   a<b> ; id
  [::a]
  [::b]
  ::b)

(a<b> ::a)

(defconsequent   a->b ; directional xf
  [::a]
  [::b]
  ::b)

(a->b ::a)

(defmergesequent a>>b ; merge xf
  [::a]
  [::b]
  ::b)

(a>>b ::a)

(defsequent      a<>b ; bidirectional xf
  [::a]
  [::b]
  ::b
  ::a)

(a<>b ::a)
(a<>b ::b)
