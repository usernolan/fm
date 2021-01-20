(ns fm.cljs
  (:require
   [clojure.core.specs.alpha :as core.specs]
   [clojure.spec.alpha :as s]))


   ;;;
   ;;; NOTE: `fm` depends on the ability to conform against these specs from
   ;;; `clojure.core`. in clojure, these specs are in the registry by default.
   ;;; in clojurescript, these specs are not included when the registry is
   ;;; redefined for spec consumers. the following is a stopgap until a better
   ;;; way to inherit these specs in clojurescript is identified
   ;;;


;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(s/def ::core.specs/local-name (s/and simple-symbol? #(not= '& %)))

(s/def ::core.specs/binding-form
  (s/or :local-symbol ::core.specs/local-name
        :seq-destructure ::core.specs/seq-binding-form
        :map-destructure ::core.specs/map-binding-form))

(s/def ::core.specs/seq-binding-form
  (s/and vector?
         (s/cat :forms (s/* ::core.specs/binding-form)
                :rest-forms (s/? (s/cat :ampersand #{'&} :form ::core.specs/binding-form))
                :as-form (s/? (s/cat :as #{:as} :as-sym ::core.specs/local-name)))))

(s/def ::core.specs/keys (s/coll-of ident? :kind vector?))
(s/def ::core.specs/syms (s/coll-of symbol? :kind vector?))
(s/def ::core.specs/strs (s/coll-of simple-symbol? :kind vector?))
(s/def ::core.specs/or (s/map-of simple-symbol? any?))
(s/def ::core.specs/as ::core.specs/local-name)

(s/def ::core.specs/map-special-binding
  (s/keys :opt-un [::core.specs/as ::core.specs/or ::core.specs/keys
                   ::core.specs/syms ::core.specs/strs]))

(s/def ::core.specs/map-binding (s/tuple ::core.specs/binding-form any?))

(s/def ::core.specs/ns-keys
  (s/tuple
    (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
    (s/coll-of simple-symbol? :kind vector?)))

(s/def ::core.specs/map-bindings
  (s/every (s/or :map-binding ::core.specs/map-binding
                 :qualified-keys-or-syms ::core.specs/ns-keys
                 :special-binding (s/tuple #{:as :or :keys :syms :strs} any?)) :kind map?))

(s/def ::core.specs/map-binding-form
  (s/merge
   ::core.specs/map-bindings
   ::core.specs/map-special-binding))
