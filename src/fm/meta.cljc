(ns fm.meta
  (:require
   [clojure.alpha.spec :as s]
   [fm.form.lib :as form.lib]
   [fm.lib :as lib]))

(defmulti  var-xf (fn [[k _]] k))
(defmethod var-xf :fm/doc
  [[_ v]]
  [:doc v])

(defmethod var-xf :fm/arglists
  [[_ v]]
  [:arglists `(list '~v)])

(defmethod var-xf :default
  [_]
  nil)

(defmulti  fn-xf (fn [[k _]] k))
(defmethod fn-xf :fm/sym
  [[k v]]
  [k `'~v])

(defmethod fn-xf :fm/args
  [[k v]]
  [k (form.lib/tuple-spec-form* (if (vector? v) v (vector v)))])

(defmethod fn-xf :fm/ret
  [[k v]]
  [k (form.lib/spec-form v)])

(defmethod fn-xf :fm/rel
  [[k v]]
  [k (form.lib/spec-form v)])

(defmethod fn-xf :fm/handler
  [[k v]]
  [k (form.lib/handler-form v)])

(defmethod fn-xf :fm/trace
  [[k v]]
  [k (form.lib/trace-form (if (true? v) `prn v))])

(defmethod fn-xf :fm/conform
  [[k v]]
  [k (form.lib/pred-form v)])

(defmethod fn-xf :default
  [x]
  x)
