(ns fm.sequent.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.form :as form]
   [fm.form.lib :as form.lib]
   [fm.macro :refer [defm]]
   [fm.meta :as meta]
   [fm.sequent.form.lib :as seq.form.lib]
   [fm.sequent.meta :as seq.meta]))

(defm rel-form
  [{::form/keys [sym bindings args left-syms right-syms reverse?]}]

  (let [right-seq-sym (get-in bindings [:fm.sequent/right ::form.lib/sym])
        rel-spec-sym  (get-in bindings [:fm/rel           ::form.lib/sym])
        sequent       (get-in bindings [:fm/sequent       ::form.lib/form])
        merge?        (contains? #{:fm.sequent/mergesequent} sequent)
        rel-data      #:fm.rel{:left  ~(:as left-syms)
                               :right ~(:as right-syms)}]

    `(if (s/valid? ~rel-spec-sym ~rel-data)
       ~(cond
          reverse? (:as left-syms)
          merge?   `(into
                     ~(:as left-syms)
                     (select-keys
                      ~(:as right-syms)
                      ~right-seq-sym))
          :else    (:as right-syms))

       #::anomaly{:spec ::anomaly/rel
                  :sym  '~sym
                  :args ~args
                  :data (s/explain-data ~rel-spec-sym ~rel-data)})))

(defm quent-form
  [{::form/keys [sym bindings body forward-form reverse-form args
                 left-syms right-syms reverse? rel-form-fn]
    :as         form-args
    :or         {reverse?     false
                 forward-form `(do ~@body)
                 rel-form-fn  rel-form}}]

  (let [ret-sym           (gensym 'ret)
        conf-ret-sym      (gensym 'conformed-ret)
        conf-args-sym     (get-in bindings [::conformed-args-sym         ::form.lib/sym])
        left-or-sym       (get-in bindings [::seq.form.lib/left-or       ::form.lib/sym])
        right-or-sym      (get-in bindings [::seq.form.lib/right-or      ::form.lib/sym])
        left-coll-or-sym  (get-in bindings [::seq.form.lib/left-coll-or  ::form.lib/sym])
        right-coll-or-sym (get-in bindings [::seq.form.lib/right-coll-or ::form.lib/sym])
        right-seq-sym     (get-in bindings [:fm.sequent/right            ::form.lib/sym])
        trace-sym         (get-in bindings [:fm/trace                    ::form.lib/sym])
        sequent           (get-in bindings [:fm/sequent                  ::form.lib/form])
        conform?          (get-in bindings [:fm/conform                  ::form.lib/form] #{})
        conf-args-left?   (some conform? [:fm/args :fm.sequent/left])
        conf-args-right?  (some conform? [:fm/args :fm.sequent/right])
        conf-ret-left?    (some conform? [:fm/ret :fm.sequent/left])
        conf-ret-right?   (some conform? [:fm/ret :fm.sequent/right])
        trace?            (contains? bindings :fm/trace)
        rel?              (contains? bindings :fm/rel)
        merge?            (contains? #{:fm.sequent/mergesequent} sequent)
        quent-bindings    {::conformed-ret-sym {::form.lib/sym conf-ret-sym}
                           ::ret-sym           {::form.lib/sym ret-sym}}
        form-args         (update form-args ::form/bindings merge quent-bindings)]

    `(let [~(if reverse? right-syms left-syms)
           (seq.form.lib/or-conform-data->map
            #::seq.form.lib{:conform?  ~(if reverse? conf-args-right? conf-args-left?)
                            :conformed ~conf-args-sym
                            :data      ~args
                            :or-spec   ~(if reverse? right-coll-or-sym left-coll-or-sym)})]

       ~@(when trace?

           (let [trace-data (merge #:fm.trace{:sym `'~sym}
                                   (if reverse?
                                     #:fm.trace{:right (:as right-syms)}
                                     #:fm.trace{:left (:as left-syms)}))]

           [`(~trace-sym ~trace-data)]))

       (let [~ret-sym ~(if reverse? reverse-form forward-form)]

         ~@(when trace?
             [`(~trace-sym #:fm.trace{:sym '~sym :ret ~ret-sym})])

         (if (s/valid? :fm/anomaly ~ret-sym)
           ~ret-sym

           (let [~conf-ret-sym (s/conform ~(if reverse? left-or-sym right-or-sym) ~ret-sym)]

             ~@(when trace?
                 [`(~trace-sym
                    #:fm.trace{:sym '~sym :conformed-ret ~conf-ret-sym})])

             (if (s/invalid? ~conf-ret-sym)
               #::anomaly{:spec ::anomaly/ret
                          :sym  '~sym
                          :args ~args
                          :data (s/explain-data ~(if reverse? left-or-sym right-or-sym) ~ret-sym)}

               (let [~(if reverse? (:as left-syms) (:as right-syms))
                     (seq.form.lib/or-conform-data->map
                      #::seq.form.lib{:conform?  ~(if reverse? conf-ret-left? conf-ret-right?)
                                      :conformed ~conf-ret-sym
                                      :data      ~ret-sym
                                      :or-spec   ~(if reverse? left-coll-or-sym right-coll-or-sym)})]

                 ~@(when trace?

                     (let [trace-data (merge #:fm.trace{:sym `'~sym}
                                             (if reverse?
                                               #:fm.trace{:left (:as left-syms)}
                                               #:fm.trace{:right (:as right-syms)}))]

                       [`(~trace-sym ~trace-data)]))

                 ~(cond
                    rel?     (rel-form-fn form-args)
                    reverse? (:as left-syms)
                    merge?   `(into
                               ~(:as left-syms)
                               (select-keys
                                ~(:as right-syms)
                                ~right-seq-sym))
                    :else    (:as right-syms))))))))))

(defm args-binding-form
  [{::form/keys [sym bindings args quent-form-fn]
    :as         form-args
    :or         {quent-form-fn quent-form}}]

  (let [conf-args-sym (gensym 'conformed-args)
        reverse?-sym  (gensym 'reverse?)
        left-or-sym   (get-in bindings [::seq.form.lib/left-or ::form.lib/sym])
        trace-sym     (get-in bindings [:fm/trace              ::form.lib/sym])
        sequent       (get-in bindings [:fm/sequent            ::form.lib/form])
        trace?        (contains? bindings :fm/trace)
        reversible?   (contains? #{:fm.sequent/sequent} sequent)
        args-bindings {::conformed-args-sym {::form.lib/sym conf-args-sym}
                       ::reverse?-sym       {::form.lib/sym reverse?-sym}}
        form-args     (update form-args ::form/bindings merge args-bindings)]

    `(let [~conf-args-sym (s/conform ~left-or-sym ~args)]

       ~@(when trace?
           [`(~trace-sym
              #:fm.trace{:sym '~sym :conformed-args ~conf-args-sym})])

       (if (s/invalid? ~conf-args-sym)

         #::anomaly{:spec ::anomaly/args
                    :sym  '~sym
                    :args ~args
                    :data (s/explain-data ~left-or-sym ~args)}

         ~(if reversible?

            `(let [~reverse?-sym (= (first ~conf-args-sym) :fm.sequent/right)]

               (if ~reverse?-sym
                 ~(quent-form-fn (assoc form-args ::form/reverse? true))
                 ~(quent-form-fn form-args)))

            (quent-form-fn form-args))))))

(defm sequent
  [{::form/keys [sym left-form right-form]
    :as         form-args}]

  (let [seq-data   {::seq.form.lib/ns         (namespace sym)
                    ::seq.form.lib/left-form  left-form
                    ::seq.form.lib/right-form right-form}
        metadata   (into
                    (hash-map)
                    (map seq.meta/seq-xf)
                    (merge
                     (meta left-form)
                     {:fm/sym           sym
                      :fm/args          seq-data
                      :fm/ret           seq-data
                      :fm/sequent       :fm.sequent/sequent
                      :fm.sequent/left  left-form
                      :fm.sequent/right right-form}))
        bindings   (into
                    (hash-map)
                    (map seq.form.lib/binding-xf)
                    (merge
                     metadata
                     {::seq.form.lib/left-or       seq-data
                      ::seq.form.lib/right-or      seq-data
                      ::seq.form.lib/left-coll-or  seq-data
                      ::seq.form.lib/right-coll-or seq-data}))
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
                     ::form/args-binding-form-fn args-binding-form
                     ::form/ret-binding-form-fn  quent-form
                     ::form/quent-form-fn        quent-form
                     ::form/rel-form-fn          rel-form})]

    (form/binding-form form-args)))
