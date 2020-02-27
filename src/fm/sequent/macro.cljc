(ns fm.sequent.macro
  (:require
   [fm.consequent.form :as conseq.form]
   [fm.macro :refer [fm defm]]
   [fm.mergesequent.form :as mseq.form]
   [fm.nonsequent.form :as nonseq.form]
   [fm.sequent.form :as seq.form]))

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
        args-form  (if named?
                     (second signature)
                     (first signature))
        nonse-form (if named?
                     (nth signature 2)
                     (second signature))
        body       (if named?
                     (drop 3 signature)
                     (drop 2 signature))]
    (nonseq.form/nonsequent
     {:fm/macro-sym  `fm
      :fm/sym        ns-sym
      :fm/args-form  args-form
      :fm/nonse-form nonse-form
      :fm/body       body})))

(defmacro defnonsequent
  [sym args-form nonse-form & body]
  (let [ns-sym (symbol (str *ns* "/" sym))]
    (nonseq.form/nonsequent
     {:fm/macro-sym  `defm
      :fm/sym        ns-sym
      :fm/args-form  args-form
      :fm/nonse-form nonse-form
      :fm/body       body})))
