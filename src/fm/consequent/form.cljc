(ns fm.consequent.form
  (:require
   [fm.form :as form]
   [fm.macro :refer [defm]]
   [fm.sequent.form :as seq.form]
   [fm.sequent.form.lib :as seq.form.lib]
   [fm.sequent.meta :as seq.meta]))

(defm consequent
  [{::form/keys [sym left-form right-form]
    :as         form-args}]

  (let [left-data  #::seq.form.lib{:ns (namespace sym) :form left-form}
        right-data #::seq.form.lib{:ns (namespace sym) :form right-form}
        metadata   (into
                    (hash-map)
                    (map seq.meta/seq-xf)
                    (merge
                     (meta left-form)
                     {:fm/sym           sym
                      :fm/args          left-data
                      :fm/ret           right-data
                      :fm/sequent       :fm.sequent/consequent
                      :fm.sequent/left  left-form
                      :fm.sequent/right right-form}))
        bindings   (into
                    (hash-map)
                    (map seq.form.lib/binding-xf)
                    (merge
                     metadata
                     {::seq.form.lib/left-or  left-data
                      ::seq.form.lib/right-or right-data}))
        args-sym   (gensym 'arg)
        args-form  (vector args-sym)
        left-syms  (seq.form.lib/seq-form->syms left-form)
        right-syms (seq.form.lib/seq-form->syms right-form)
        form-args  (merge
                    form-args
                    {::form/bindings             bindings
                     ::form/metadata             metadata
                     ::form/args-form            args-form
                     ::form/args-syms            args-form
                     ::form/args                 args-sym
                     ::form/left-syms            left-syms
                     ::form/right-syms           right-syms
                     ::form/args-binding-form-fn seq.form/args-binding-form
                     ::form/ret-binding-form-fn  seq.form/quent-form
                     ::form/quent-form-fn        seq.form/quent-form
                     ::form/rel-form-fn          seq.form/rel-form})]

    (form/binding-form form-args)))
