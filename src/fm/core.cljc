(ns fm.core
  (:refer-clojure :exclude [or cat keys fn? fn defn select-keys])
  (:require
   [fm.anomaly :as anomaly]
   [fm.lib :as lib]
   [fm.sequent :as sequent]
   [fm.spec :as spec]
   #?@(:clj
       [[fm.form :as form]
        [fm.form.fn]]))
  #?(:cljs
     (:require-macros fm.core)))


   ;;;
   ;;; NOTE: predicates
   ;;;


(def multifn? lib/multifn?)
(def fn? lib/fn?)
(def singular? lib/singular?)
(def throwable? lib/throwable?)


   ;;;
   ;;; NOTE: compound spec operations
   ;;;


(def conform-explain lib/conform-explain)
(def conform-throw lib/conform-throw)
(def conform-tag lib/conform-tag)
(def tag lib/tag)
(def which lib/which)


   ;;;
   ;;; NOTE: data transformations
   ;;;


(def zip lib/zip)
(def zipv lib/zipv)
(def zipf lib/zipf)
(def zipvf lib/zipvf)
(def rreduce lib/rreduce)
(def deep-some lib/deep-some)
(def evolve lib/evolve)
(def evolve-xf lib/evolve-xf)


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
   ;;; NOTE: `fm.anomaly` api
   ;;;


(def anomaly? anomaly/anomaly?)
(def deep-anomaly anomaly/deep-anomaly)
(def deep-anomaly? anomaly/deep-anomaly?)
(def anomalous? anomaly/anomalous?)
(def unwrap-anomaly anomaly/unwrap)
(def throw-anomaly anomaly/throw)


   ;;;
   ;;; NOTE: `fm.form` api
   ;;;


#?(:clj (def form form/form))
#?(:clj (def metadata form/metadata))


   ;;;
   ;;; NOTE: `fm.spec` api, macros
   ;;;


(def select-keys spec/select-keys)
#?(:clj (defmacro or [& definition] `(spec/or ~@definition)))
#?(:clj (defmacro cat [& definition] `(spec/cat ~@definition)))
#?(:clj (defmacro alt [& definition] `(spec/alt ~@definition)))
#?(:clj (defmacro keys [& definition] `(spec/keys ~@definition)))
#?(:clj (defmacro keys* [& definition] `(spec/keys* ~@definition)))


   ;;;
   ;;; NOTE: `fm.sequent` api
   ;;;


(def nominal? sequent/nominal?)
(def positional? sequent/positional?)
(def combine sequent/combine)
(def effect sequent/effect)


   ;;;
   ;;; NOTE: `fm.core` api
   ;;; ALT: `fm.analysis` ns
   ;;;


(def ident
  (comp :fm/ident meta))


   ;;;
   ;;; NOTE: default `fm.form` configuration
   ;;;


(def defaults-atom
  (atom
   {:fm/trace    nil
    :fm/trace-fn prn
    :fm/handler  anomaly/throw}))


   ;;;
   ;;; NOTE: macros
   ;;;


#?(:clj (defmacro fn [& definition]
          (form/form
           {:fm.form.fn/defaults   @defaults-atom
            :fm.form.fn/definition definition
            :fm.form/env           &env
            :fm.form/ns            *ns*}
           :fm.form/fn)))

#?(:clj (defmacro defn [& definition]
          (form/form
           {:fm.form.fn/defaults   @defaults-atom
            :fm.form.fn/definition definition
            :fm.form/env           &env
            :fm.form/ns            *ns*}
           :fm.form/defn)))
