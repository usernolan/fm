(ns fm.nonsequent.meta
  (:require
   [clojure.alpha.spec :as s]
   [fm.meta :as meta]
   [fm.sequent.form.lib :as form.lib]))

(defmulti  nonse-xf (fn [[k _]] k))
(defmethod nonse-xf :fm/args
  [[k v]]
  [k {::meta/sym  (gensym 'args-spec)
      ::meta/form (form.lib/args-form->select-form v)}])

(defmethod nonse-xf :fm/ret
  [_]
  nil)

(defmethod nonse-xf :fm/nonse
  [[k v]]
  [k {::meta/sym  (gensym 'nonse-spec)
      ::meta/form `(s/select ~v [~'*])}])

(defmethod nonse-xf :fm/rel
  [x]
  (meta/fn-xf x))

(defmethod nonse-xf :fm/ignore
  [x]
  (meta/fn-xf x))

(defmethod nonse-xf :default
  [[k v]]
  [k {::meta/form v}])
