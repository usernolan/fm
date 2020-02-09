(ns fm.macros
  (:require
   [fm.utils :as utils]))

(defmacro fm
  [args-form & body]
  (let [ns-sym (symbol (str *ns* "/" (gensym "fm__")))]
    (utils/fm-form {:fm/sym       ns-sym
                    :fm/args-form args-form
                    :fm/body      body})))

(defmulti  var-meta-xf (fn [[k _]] k))
(defmethod var-meta-xf :fm/doc
  [[_ v]]
  [:doc v])

(defmethod var-meta-xf :fm/arglists
  [[_ v]]
  [:arglists `(list '~v)])

(defmethod var-meta-xf :default
  [_]
  nil)

(defmacro defm
  [sym args-form & body]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  {}
                  (map var-meta-xf)
                  (merge
                   (meta args-form)
                   {:fm/arglists args-form}))
        fm-sym   (with-meta sym var-meta)
        fm-form  (utils/fm-form {:fm/sym       ns-sym
                                 :fm/args-form args-form
                                 :fm/body      body})]
    `(def ~fm-sym ~fm-form)))
