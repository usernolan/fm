(ns fm.nonsequent.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.form :as form]
   [fm.form.lib :as form.lib]
   [fm.macro :refer [defm]]
   [fm.meta :as meta]
   [fm.sequent.form :as seq.form]
   [fm.sequent.form.lib :as seq.form.lib]
   [fm.sequent.meta :as seq.meta]))

(defm rel-form
  [{::form/keys [sym bindings args left-syms nonse-syms]}]

  (let [nonse-sym         (get-in bindings [::nonse-sym             ::form.lib/sym])
        conf-nonse-sym    (get-in bindings [::conformed-nonse-sym   ::form.lib/sym])
        rel-spec-sym      (get-in bindings [:fm/rel                 ::form.lib/sym])
        conform?          (get-in bindings [:fm/conform             ::form.lib/form] #{})
        conf-right-nonse? (some conform? [:fm/right :fm.sequent/nonse])
        rel-data          #:fm.rel{:left  (:as left-syms)
                                   :nonse (:as nonse-syms)}]

    `(let [~nonse-syms
           (seq.form.lib/or-conform-data->map
            #::seq.form.lib{:conform?  ~conf-right-nonse?
                            :conformed ~conf-nonse-sym
                            :data      ~nonse-sym})]

       (if (s/valid? ~rel-spec-sym ~rel-data)
         ~(:as left-syms)

         #::anomaly{:spec ::anomaly/rel
                    :sym  '~sym
                    :args ~args
                    :data (s/explain-data ~rel-spec-sym ~rel-data)}))))

(defm quent-form
  [{::form/keys [sym left-form body bindings args left-syms rel-form-fn]
    :as         form-args
    :or         {rel-form-fn rel-form}}]

  (let [nonse-sym       (gensym 'nonse)
        conf-nonse-sym  (gensym 'conformed-nonse)
        conf-args-sym   (get-in bindings [::seq.form/conformed-args-sym ::form.lib/sym])
        nonse-or-sym    (get-in bindings [::seq.form.lib/nonse-or       ::form.lib/sym])
        trace-sym       (get-in bindings [:fm/trace                     ::form.lib/sym])
        conform?        (get-in bindings [:fm/conform                   ::form.lib/form] #{})
        conf-args-left? (some conform? [:fm/args :fm.sequent/left])
        trace?          (contains? bindings :fm/trace)
        rel?            (contains? bindings :fm/rel)
        quent-bindings  {::conformed-nonse-sym {::form.lib/sym conf-nonse-sym}
                         ::nonse-sym           {::form.lib/sym nonse-sym}}
        form-args       (update form-args ::form/bindings merge quent-bindings)]

    `(let [~left-syms
           (seq.form.lib/or-conform-data->map
            #::seq.form.lib{:conform?  ~conf-args-left?
                            :conformed ~conf-args-sym
                            :data      ~args})]

       ~@(when trace?
           [`(~trace-sym #:fm.trace{:sym '~sym :left ~(:as left-syms)})])

       (let [~nonse-sym (do ~@body)]

         ~@(when trace?
             [`(~trace-sym #:fm.trace{:sym '~sym :nonse ~nonse-sym})])

         (if (s/valid? :fm/anomaly ~nonse-sym)
           ~nonse-sym

           (let [~conf-nonse-sym (s/conform ~nonse-or-sym ~nonse-sym)]

             ~@(when trace?
                 [`(~trace-sym
                    #:fm.trace{:sym '~sym :conformed-nonse ~conf-nonse-sym})])

             (if (s/invalid? ~conf-nonse-sym)
               #::anomaly{:spec ::anomaly/nonse
                          :sym  '~sym
                          :args ~args
                          :data (s/explain-data ~nonse-or-sym ~nonse-sym)}

               ~(if rel?
                  (rel-form-fn form-args)
                  (:as left-syms)))))))))

(defm nonsequent
  [{::form/keys [sym left-form right-form]
    :as         form-args}]

  (let [left-data  #::seq.form.lib{:ns (namespace sym) :form left-form}
        right-data #::seq.form.lib{:ns (namespace sym) :form right-form}
        metadata   (into
                    (hash-map)
                    (map seq.meta/nonse-xf)
                    (merge
                     (meta left-form)
                     {:fm/sym           sym
                      :fm/args          left-data
                      :fm/ret           left-data
                      :fm/sequent       :fm.sequent/nonsequent
                      :fm/nonse         right-data
                      :fm.sequent/left  left-form
                      :fm.sequent/right left-form
                      :fm.sequent/nonse right-form}))
        bindings   (into
                    (hash-map)
                    (map seq.form.lib/binding-xf)
                    (merge
                     metadata
                     {::seq.form.lib/left-or  left-data
                      ::seq.form.lib/nonse-or right-data}))
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
                     ::form/nonse-syms           right-syms
                     ::form/args-binding-form-fn seq.form/args-binding-form
                     ::form/ret-binding-form-fn  quent-form
                     ::form/quent-form-fn        quent-form
                     ::form/rel-form-fn          rel-form})]

    (form/binding-form form-args)))
