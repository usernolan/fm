(ns fm.core
  (:refer-clojure :exclude [fn defn merge])
  (:require
   [fm.anomaly :as anomaly]
   [fm.api :as api]
   [fm.form :as form]))

(defmacro fn [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/fn))

(defmacro conse [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/conse))

(defmacro nonse [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/nonse))

(defmacro merge [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/merge))

(defmacro iso [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/iso))

(defmacro defn [& definition]
  (form/->def
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/fn))

(defmacro defconse [& definition]
  (form/->def
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/conse))

(defmacro defnonse [& definition]
  (form/->def
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/nonse))

(defmacro defmerge [& definition]
  (form/->def
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/merge))

(defmacro defiso [& definition]
  (form/->def
   {::form/definition definition
    ::form/ns         *ns*}
   ::form/iso))
