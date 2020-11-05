(ns fm.core
  (:refer-clojure :exclude [fn defn merge])
  (:require
   [fm.anomaly :as anomaly]
   [fm.api :as api]
   [fm.form :as form]))

(defmacro fn [& definition]
  (form/->form
   {::form/ident      ::form/fn
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro conse [& definition]
  (form/->form
   {::form/ident      ::form/conse
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro nonse [& definition]
  (form/->form
   {::form/ident      ::form/nonse
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro merge [& definition]
  (form/->form
   {::form/ident      ::form/merge
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro iso [& definition]
  (form/->form
   {::form/ident      ::form/iso
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro defn [& definition]
  (form/->def
   {::form/ident      ::form/fn
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro defconse [& definition]
  (form/->def
   {::form/ident      ::form/conse
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro defnonse [& definition]
  (form/->def
   {::form/ident      ::form/nonse
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro defmerge [& definition]
  (form/->def
   {::form/ident      ::form/merge
    ::form/definition definition
    ::form/ns         *ns*}))

(defmacro defiso [& definition]
  (form/->def
   {::form/ident      ::form/iso
    ::form/definition definition
    ::form/ns         *ns*}))
