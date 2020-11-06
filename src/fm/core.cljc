(ns fm.core
  (:refer-clojure :exclude [fn defn merge])
  (:require
   [fm.anomaly :as anomaly]
   [fm.api :as api]
   [fm.form :as form]))


   ;;;
   ;;; NOTE: global dynamic variables; configuration
   ;;;

(def ^:dynamic *trace*
  nil)

(def ^:dynamic *trace-fn*
  `prn)

(def ^:dynamic *anomaly-handler*
  `identity)


   ;;;
   ;;; NOTE: macros
   ;;;

(defmacro fn [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*
    ::form/defaults   {:fm/trace    *trace*
                       :fm/trace-fn *trace-fn*
                       :fm/handler  *anomaly-handler*}}
   ::form/fn))
