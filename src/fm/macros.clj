(ns fm.macros
  (:require
   [fm.utils :as utils]))

(defmacro fm
  "The `fm` macro."
  [args ret & body]
  (let [qname (symbol (str *ns* "/" (gensym "fm__")))]
    (utils/fm-form {:fm/fname qname
                    :fm/args args
                    :fm/ret ret
                    :fm/body body})))

(defmacro defm
  "The `defm` macro."
  [fname args ret & body]
  (let [qname (symbol (str *ns* "/" fname))
        form (utils/fm-form {:fm/fname qname
                             :fm/args args
                             :fm/ret ret
                             :fm/body body})]
    `(def ~fname ~form)))
