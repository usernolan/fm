(ns fm.macro
  (:require
   [fm.form :as form]
   [fm.meta :as meta]))

(defmacro fm
  [args-form & body]
  (let [ns-sym (symbol (str *ns* "/" (gensym "fm__")))]
    (form/fm {:fm/sym       ns-sym
              :fm/args-form args-form
              :fm/body      body})))

(defmacro defm
  [sym args-form & body]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  {}
                  (map meta/var-xf)
                  (merge
                   (meta args-form)
                   {:fm/arglists args-form}))
        fm-sym   (with-meta sym var-meta)
        fm-form  (form/fm {:fm/sym       ns-sym
                           :fm/args-form args-form
                           :fm/body      body})]
    `(def ~fm-sym ~fm-form)))
