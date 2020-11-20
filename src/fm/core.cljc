(ns fm.core
  (:refer-clojure :exclude [fn defn merge])
  (:require
   [fm.anomaly :as anomaly]
   [fm.form :as form]
   [fm.lib :as lib]))


   ;;;
   ;;; NOTE: dynamic variables; configuration
   ;;;

(def ^:dynamic *throw!*
  nil)

(def ^:dynamic *trace*
  nil)

(def ^:dynamic *trace-fn*
  `prn)

(def ^:dynamic *anomaly-handler*
  `identity)


   ;;;
   ;;; NOTE: api
   ;;;

(def zip lib/zip)
(def zipv lib/zipv)
(def zipf lib/zipf)
(def zipvf lib/zipvf)
(def rreduce lib/rreduce)
(def conform-throw lib/conform-throw)

  ;; TODO: add `anomaly`

(def ->form form/->form)
(def ->metadata form/->metadata)


   ;;;
   ;;; NOTE: macros
   ;;;

(defmacro fn [& definition]
  (form/->form
   {::form/definition definition
    ::form/ns         *ns*
    ::form/defaults   {:fm/throw!   *throw!*
                       :fm/trace    *trace*
                       :fm/trace-fn *trace-fn*
                       :fm/handler  *anomaly-handler*}}
   ::form/fn))

(defmacro defn [& definition]
  (form/->def
   {::form/definition definition
    ::form/ns         *ns*
    ::form/defaults   {:fm/throw!   *throw!*
                       :fm/trace    *trace*
                       :fm/trace-fn *trace-fn*
                       :fm/handler  *anomaly-handler*}}
   ::form/fn))
