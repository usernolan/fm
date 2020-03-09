(ns fm.sequent.meta
  (:require
   [fm.meta :as meta]
   [fm.sequent.form.lib :as seq.form.lib]))

(defmulti  var-xf (fn [[k _]] k))
(defmethod var-xf :fm/arglists
  [[_ v]]
  [:arglists
   (if (vector? v)
     `(list ['~(seq.form.lib/seq-form->syms v)])
     `(list ['~(seq.form.lib/seq-form->syms (::seq.form.lib/left-form  v))]
            ['~(seq.form.lib/seq-form->syms (::seq.form.lib/right-form v))]))])

(defmethod var-xf :default
  [x]
  (meta/var-xf x))

(defmulti  seq-xf (fn [[k _]] k))
(defmethod seq-xf :fm/args
  [[k v]]
  [k (seq.form.lib/seq-data->keys-form v)])

(defmethod seq-xf :fm/ret
  [[k v]]
  [k (seq.form.lib/seq-data->keys-form (assoc v ::seq.form.lib/right? true))])

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

(defmulti  merge-xf (fn [[k _]] k))
(defmethod merge-xf :fm/ret
  [[k {::seq.form.lib/keys [ns left-form right-form]}]]
  (let [left  (seq.form.lib/seq-form->sorted-kws left-form)
        right (seq.form.lib/seq-form->sorted-kws right-form)
        form  (vec (sort (into left right)))
        data  #::seq.form.lib{:ns ns :form form}]
    [k (seq.form.lib/seq-data->keys-form data)]))

(defmethod merge-xf :fm.sequent/right
  [[k {::seq.form.lib/keys [left-form right-form]}]]
  (let [left  (seq.form.lib/seq-form->sorted-kws left-form)
        right (seq.form.lib/seq-form->sorted-kws right-form)]
    [k (vec (sort (into left right)))]))

(defmethod merge-xf :default
  [x]
  (seq-xf x))
