(ns fm.specs
  (:require
   [clojure.core.specs.alpha :as core.specs]
   [clojure.spec.alpha :as s]))


   ;;;
   ;;; NOTE: low-level predicates, specs
   ;;;


(s/def ::s/registry-keyword
  qualified-keyword?)


   ;;;
   ;;; NOTE: core predicates, specs
   ;;; NOTE: `specv` dsl
   ;;;


(s/def ::positional-binding-map
  (s/map-of
   ::s/registry-keyword
   ::core.specs/binding-form
   :count 1))

(s/def ::positional-binding-form
  (s/or
   ::s/registry-keyword ::s/registry-keyword
   ::positional-binding-map ::positional-binding-map
   ::core.specs/binding-form ::core.specs/binding-form))

  ;; NOTE: see `::core.specs/param-list`
(s/def ::positional-param-list
  (s/cat
   :params (s/* ::positional-binding-form)
   :var-params (s/? (s/cat
                     :ampersand #{'&}
                     :var-form ::positional-binding-form))
   :as-form (s/? (s/cat
                  :as #{:as}
                  :as-sym ::core.specs/local-name))))

(s/def ::nominal-binding-map
  (s/map-of keyword? ::core.specs/binding-form))

(s/def ::nominal-binding-form
  (s/or
   :keyword keyword? ; NOTE: `:req-un`
   ::nominal-binding-map ::nominal-binding-map))

(s/def ::nominal-param-list
  (s/cat
   :params (s/* ::nominal-binding-form)
   :as-form (s/? (s/cat
                  :as #{:as}
                  :as-sym ::core.specs/local-name))))

(s/def ::specv
  (s/and
   vector?
   (s/or
    :fm.context/nominal (s/tuple ::nominal-param-list)
    :fm.context/positional ::positional-param-list)))
