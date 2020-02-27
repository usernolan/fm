(ns fm.sequent.usage
  (:require
   [fm.sequent.macro
    :refer
    [defnonsequent
     defconsequent
     defsequent
     defmergesequent]]))

(s/def ::a int?)
(s/def ::b int?)
(s/def ::c int?)
(s/def ::d int?)

(def ! (comp (constantly 0) prn))

(defnonsequent   AB<>AB   ; id
  [::a ::b :as x]
  [::c ::d]
  {::c (! 'c)
   ::d (! 'd)})

(AB<>AB {::a 1 ::b 2})

(defconsequent   AB->CD   ; directional xf
  [::a ::b]
  [::c ::d]
  {::c (+ a b)
   ::d (- a b)})

(AB->CD {::a 1 ::b 2})

(defsequent      AB<>CD   ; reversible xf
  [::a ::b]
  [::c ::d]
  {::c (+ a 2)
   ::d (* b 2)}
  {::a (- c 2)
   ::b (/ d 2)})

(AB<>CD {::a 1 ::b 2})
(AB<>CD {::c 3 ::d 4})
(s/conform ::AB<>CD {::a 1 ::b 2})
(s/unform  ::AB<>CD {::c 3 ::d 4})

(defmergesequent AB<>ABCD ; merge xf
  [::a ::b]
  [::c ::d]
  {::c (* a a)
   ::d (* b b)})

(AB<>ABCD {::a 1 ::b 2})
