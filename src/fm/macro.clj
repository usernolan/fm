(ns fm.macro
  (:require
   [fm.form :as form]
   [fm.meta :as meta]))

(defmacro fm
  [& signature]
  (let [named?    (symbol? (first signature))
        ns-sym    (->>
                   (if named?
                     (first signature)
                     'fm)
                   (gensym)
                   (str *ns* "/")
                   (symbol))
        args-form (if named?
                    (second signature)
                    (first signature))
        body      (if named?
                    (drop 2 signature)
                    (drop 1 signature))]
    (form/fm
     {:fm/sym       ns-sym
      :fm/args-form args-form
      :fm/body      body})))

(defmacro defm
  [sym args-form & body]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  (hash-map)
                  (map meta/var-xf)
                  (merge
                   (meta args-form)
                   {:fm/arglists args-form}))
        fm-sym   (with-meta sym var-meta)
        fm-form  (form/fm
                  {:fm/sym       ns-sym
                   :fm/args-form args-form
                   :fm/body      body})]
    `(def ~fm-sym ~fm-form)))
