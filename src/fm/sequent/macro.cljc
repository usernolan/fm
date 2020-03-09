(ns fm.sequent.macro
  (:require
   [fm.consequent.form :as conse.form]
   [fm.form :as form]
   [fm.macro :refer [fm defm]]
   [fm.mergesequent.form :as merge.form]
   [fm.nonsequent.form :as nonse.form]
   [fm.sequent.form :as seq.form]
   [fm.sequent.form.lib :as seq.form.lib]
   [fm.sequent.meta :as seq.meta]))

(defn conseq-signature->map
  [signature]
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
    {::form/sym        ns-sym
     ::form/left-form  left-form
     ::form/right-form right-form
     ::form/body       body}))

(defn seq-signature->map
  [signature]
  (let [{::form/keys [body] :as m}
        (conseq-signature->map signature)]
    (merge
     (dissoc m ::form/body)
     {::form/forward-form (first body)
      ::form/reverse-form (second body)})))

(defmacro nonsequent
  [& signature]
  (->>
   (conseq-signature->map signature)
   (nonse.form/nonsequent)))

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
        seq-form (nonse.form/nonsequent
                  {::form/sym        ns-sym
                   ::form/left-form  left-form
                   ::form/right-form right-form
                   ::form/body       body})]
    `(def ~seq-sym ~seq-form)))

(defmacro consequent
  [& signature]
  (->>
   (conseq-signature->map signature)
   (conse.form/consequent)))

(defmacro defconsequent
  [sym left-form right-form & body]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  (hash-map)
                  (map seq.meta/var-xf)
                  (merge
                   (meta left-form)
                   {:fm/arglists left-form}))
        seq-sym  (with-meta sym var-meta)
        seq-form (conse.form/consequent
                  {::form/sym        ns-sym
                   ::form/left-form  left-form
                   ::form/right-form right-form
                   ::form/body       body})]
    `(def ~seq-sym ~seq-form)))

(defmacro mergesequent
  [& signature]
  (->>
   (conseq-signature->map signature)
   (merge.form/mergesequent)))

(defmacro defmergesequent
  [sym left-form right-form & body]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  (hash-map)
                  (map seq.meta/var-xf)
                  (merge
                   (meta left-form)
                   {:fm/arglists left-form}))
        seq-sym  (with-meta sym var-meta)
        seq-form (merge.form/mergesequent
                  {::form/sym        ns-sym
                   ::form/left-form  left-form
                   ::form/right-form right-form
                   ::form/body       body})]
    `(def ~seq-sym ~seq-form)))

(defmacro sequent
  [& signature]
  (->>
   (seq-signature->map signature)
   (seq.form/sequent)))

(defmacro defsequent
  [sym left-form right-form forward-form reverse-form]
  (let [ns-sym   (symbol (str *ns* "/" sym))
        var-meta (into
                  (hash-map)
                  (map seq.meta/var-xf)
                  (merge
                   (meta left-form)
                   {:fm/arglists
                    {::seq.form.lib/left-form  left-form
                     ::seq.form.lib/right-form right-form}}))
        seq-sym  (with-meta sym var-meta)
        seq-form (seq.form/sequent
                  {::form/sym          ns-sym
                   ::form/left-form    left-form
                   ::form/right-form   right-form
                   ::form/forward-form forward-form
                   ::form/reverse-form reverse-form})]
    `(def ~seq-sym ~seq-form)))
