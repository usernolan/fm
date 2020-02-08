(ns fm.macros
  (:require
   [fm.utils :as utils]))

(defmacro fm
  [args-form & body]
  (let [ns-sym (symbol (str *ns* "/" (gensym "fm__")))]
    (utils/fm-form {:fm/sym       ns-sym
                    :fm/args-form args-form
                    :fm/body      body})))

(defmacro defm
  [sym args-form & body]
  (let [ns-sym (symbol (str *ns* "/" sym))
        form   (utils/fm-form {:fm/sym       ns-sym
                               :fm/args-form args-form
                               :fm/body      body})]
    `(def ~sym ~form)))
