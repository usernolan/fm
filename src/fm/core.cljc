(ns fm.core
  (:refer-clojure :exclude [fn? fn defn])
  (:require
   [fm.anomaly :as anomaly]
   [fm.form :as form]
   [fm.form.fn :as fn]
   [fm.lib :as lib]))


   ;;;
   ;;; NOTE: dynamic variables; default configuration
   ;;;


(def ^:dynamic *throw!* nil)
(def ^:dynamic *trace* nil)
(def ^:dynamic *trace-fn* `prn)
(def ^:dynamic *anomaly-handler* `identity)


   ;;;
   ;;; NOTE: predicates
   ;;;


(def multifn? lib/multifn?)
(def fn? lib/fn?)
(def singular? lib/singular?)


   ;;;
   ;;; NOTE: compound spec operations
   ;;;


(def conform-explain lib/conform-explain)
(def conform-throw! lib/conform-throw!)


   ;;;
   ;;; NOTE: data transformations
   ;;;


(def zip lib/zip)
(def zipv lib/zipv)
(def zipf lib/zipf)
(def zipvf lib/zipvf)
(def rreduce lib/rreduce)
(def deep-some lib/deep-some)


   ;;;
   ;;; NOTE: hierarchical retrieval
   ;;;


(def geta lib/geta)
(def geta-in lib/geta-in)
(def finda lib/finda)


   ;;;
   ;;; NOTE: default sequent combinators
   ;;;


(def ensure-sequential lib/ensure-sequential)
(def positional-combine lib/positional-combine)
(def nominal-combine lib/nominal-combine)


   ;;;
   ;;; TODO: add `anomaly`
   ;;;


   ;;;
   ;;; NOTE: `fm.form` api
   ;;;


(def ->form form/->form)
(def ->metadata form/->metadata)


   ;;;
   ;;; NOTE: macros
   ;;;

  ;; TODO: `defaults`; NOTE: dynvars


(defmacro fn [& definition]
  (form/->form
   {::fn/definition definition
    ::fn/defaults   {:fm/throw!   *throw!*
                     :fm/trace    *trace*
                     :fm/trace-fn *trace-fn*
                     :fm/handler  *anomaly-handler*}
    ::form/ns       *ns*}
   ::form/fn))

(defmacro defn [& definition]
  (form/->def
   {::fn/definition definition
    ::fn/defaults   {:fm/throw!   *throw!*
                     :fm/trace    *trace*
                     :fm/trace-fn *trace-fn*
                     :fm/handler  *anomaly-handler*}
    ::form/ns       *ns*}
   ::form/fn))
