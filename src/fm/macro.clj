(ns fm.macro
  (:require
   [fm.form :as form]
   [fm.form.fn :as fn]))

  ;; TODO: multiple definitions
  ;; TODO: variable arity
(defmacro fn
  [& definition]
  (form/fn {::form/ns *ns* ::fn/definition definition}))

(comment

  (macroexpand
   '(fm ^{:fm/doc "bad!"}
        fm1
        ^{:fm/args int?}
        [a]
        a))

  (fm [a] a)

  ;;
  )

#_
(defmacro defm
  [& definition]
  (form/defm
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro consequent
  [& definition]
  (form/consequent
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro nonsequent
  [& definition]
  (form/insequent
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro mergesequent
  [& definition]
  (form/mergesequent
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro isoquent
  [& definition]
  (form/isoquent
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro multiquent
  [& definition]
  (form/multiquent
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro comp
  [& definition]
  (form/comp
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro sequent
  [& definition]
  (form/comp
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro match
  [& definition]
  (form/match
   {::form/ns         *ns*
    ::form/definition definition}))

#_
(defmacro branch
  [& definition]
  (form/branch
   {::form/ns         *ns*
    ::form/definition definition}))
