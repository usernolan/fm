(ns fm.meta
  (:require
   [clojure.alpha.spec :as s]
   [fm.form.lib :as form.lib]
   [fm.lib :as lib]))

(s/def ::sym form.lib/binding-sym?)

(s/def ::binding
  (s/select [::sym] [*]))

(def binding?
  (partial s/valid? ::binding))

(def binding-filter
  (comp binding? second))

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
  [k {::sym  (gensym 'sym)
      ::form `'~v}])

(defmethod fn-xf :fm/args
  [[k v]]
  [k {::sym  (gensym 'args-spec)
      ::form (form.lib/tuple-spec-form* (if (vector? v) v (vector v)))}])

(defmethod fn-xf :fm/ret
  [[k v]]
  [k {::sym  (gensym 'ret-spec)
      ::form (form.lib/spec-form v)}])

(defmethod fn-xf :fm/rel
  [[k v]]
  [k {::sym  (gensym 'rel-spec)
      ::form (form.lib/spec-form v)}])

(defmethod fn-xf :fm/handler
  [[k v]]
  [k {::sym  (gensym 'handler)
      ::form (form.lib/handler-form v)}])

(defmethod fn-xf :fm/trace
  [[k v]]
  [k {::sym  (gensym 'trace)
      ::form (form.lib/trace-form (if (true? v) `prn v))}])

(defmethod fn-xf :fm/conform
  [[k v]]
  [k {::form (lib/ensure-pred v)}])

(defmethod fn-xf :default
  [[k v]]
  [k {::sym  (gensym)
      ::form v}])
