(ns fm.meta
  (:require
   [fm.form.lib :as form.lib]))


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
  [k {::sym  (gensym "sym__")
      ::form `'~v}])

(defmethod fn-xf :fm/args
  [[k v]]
  [k {::sym  (gensym "args-spec__")
      ::form (form.lib/tuple-spec-form* (if (vector? v) v [v]))}])

(defmethod fn-xf :fm/ret
  [[k v]]
  [k {::sym  (gensym "ret-spec__")
      ::form (form.lib/spec-form v)}])

(defmethod fn-xf :fm/rel
  [[k v]]
  [k {::sym  (gensym "rel-spec__")
      ::form (form.lib/spec-form v)}])

(defmethod fn-xf :fm/handler
  [[k v]]
  [k {::sym  (gensym "handler__")
      ::form (form.lib/handler-form v)}])

(defmethod fn-xf :fm/trace
  [[k v]]
  [k {::sym  (gensym "trace__")
      ::form (form.lib/trace-form (if (true? v) `prn v))}])

(defmethod fn-xf :default
  [[k v]]
  [k {::sym  (gensym)
      ::form v}])
