(ns fm.core
  (:require
   [fm.anomaly :as anomaly]
   [fm.api :as api]
   [fm.form :as form]))

(defmacro fn [& definition]
  (form/->fn
   {::form/definition definition
    ::form/ns         *ns*}))
