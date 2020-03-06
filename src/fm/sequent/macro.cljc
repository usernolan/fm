(ns fm.sequent.macro
  (:require
   [fm.consequent.form :as conseq.form]
   [fm.form :as form]
   [fm.macro :refer [fm defm]]
   [fm.mergesequent.form :as mseq.form]
   [fm.nonsequent.form :as nonseq.form]
   [fm.sequent.form :as seq.form]
   [fm.sequent.meta :as seq.meta]))

(defmacro nonsequent
  [& signature]
  (let [named?     (symbol? (first signature))
        ns-sym     (->>
                    (if named?
                      (first signature)
                      'nonsequent)
                    (gensym)
                    (str *ns* "/")
                    (symbol))
        left-form  (if named?
                     (second signature)
                     (first signature))
        right-form (if named?
                     (nth signature 2)
                     (second signature))
        body       (if named?
                     (drop 3 signature)
                     (drop 2 signature))]
    (nonseq.form/nonsequent
     {::form/sym        ns-sym
      ::form/left-form  left-form
      ::form/right-form right-form
      ::form/body       body})))

(defmacro defnonsequent
  [sym left-form right-form & body]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  (hash-map)
                  (map seq.meta/var-xf)
                  (merge
                   (meta left-form)
                   {:fm/arglists left-form}))
        seq-sym  (with-meta sym var-meta)
        seq-form (nonseq.form/nonsequent
                  {::form/sym        ns-sym
                   ::form/left-form  left-form
                   ::form/right-form right-form
                   ::form/body       body})]
    `(def ~seq-sym ~seq-form)))

(defmacro sequent
  [& signature]
  (let [named?       (symbol? (first signature))
        ns-sym       (->>
                      (if named?
                        (first signature)
                        'nonsequent)
                      (gensym)
                      (str *ns* "/")
                      (symbol))
        left-form    (if named?
                       (second signature)
                       (first signature))
        right-form   (if named?
                       (nth signature 2)
                       (second signature))
        forward-form (if named?
                       (nth signature 3)
                       (nth signature 2))
        reverse-form (if named?
                       (nth signature 4)
                       (nth signature 3))]
    (seq.form/sequent
     {::form/sym          ns-sym
      ::form/left-form    left-form
      ::form/right-form   right-form
      ::form/forward-form forward-form
      ::form/reverse-form reverse-form})))

(defmacro defsequent
  [sym left-form right-form forward-form reverse-form]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  (hash-map)
                  (map seq.meta/var-xf)
                  (merge
                   (meta left-form)
                   {:fm/arglists left-form})) ; TODO: right form
        seq-sym  (with-meta sym var-meta)
        seq-form (seq.form/sequent
                  {::form/sym          ns-sym
                   ::form/left-form    left-form
                   ::form/right-form   right-form
                   ::form/forward-form forward-form
                   ::form/reverse-form reverse-form})]
    `(def ~seq-sym ~seq-form)))
