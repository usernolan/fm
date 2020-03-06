(ns fm.sequent.meta
  (:require
   [fm.meta :as meta]
   [fm.sequent.form.lib :as seq.form.lib]))

(defmulti  var-xf (fn [[k _]] k))
(defmethod var-xf :fm/arglists
  [[_ v]]
  [:arglists `(list '~v)]) ; TODO: form.lib/seq-form->arglists

(defmethod var-xf :default
  [x]
  (meta/var-xf x))

(defmulti  seq-xf (fn [[k _]] k))
(defmethod seq-xf :fm/args
  [[k v]]
  [k (seq.form.lib/seq-data->keys-form v)])

(defmethod seq-xf :fm/ret
  [[k v]]
  [k (seq.form.lib/seq-data->keys-form v)])

(defmethod seq-xf :fm.sequent/left
  [[k v]]
  [k (seq.form.lib/seq-form->sorted-kws v)])

(defmethod seq-xf :fm.sequent/right
  [[k v]]
  [k (seq.form.lib/seq-form->sorted-kws v)])

(defmethod seq-xf :default
  [x]
  (meta/fn-xf x))

(defmulti  nonse-xf (fn [[k _]] k))
(defmethod nonse-xf :fm/nonse
  [[k v]]
  [k (seq.form.lib/seq-data->keys-form v)])

(defmethod nonse-xf :fm.sequent/nonse
  [[k v]]
  [k (seq.form.lib/seq-form->sorted-kws v)])

(defmethod nonse-xf :default
  [x]
  (seq-xf x))
