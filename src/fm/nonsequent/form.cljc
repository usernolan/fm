(ns fm.nonsequent.form
  (:require
   [clojure.alpha.spec :as s]
   [fm.anomaly :as anomaly]
   [fm.macro :refer [defm]]
   [fm.meta :as meta]
   [fm.sequent.form.lib :as form.lib]
   [fm.nonsequent.meta :as nonseq.meta]))

(defm body-form
  [{:fm/keys [sym metadata args-syms nonse-form body]}]
  (let [nonse-sym      (gensym 'nonse)
        nonse-spec-sym (get-in metadata [:fm/nonse  ::meta/sym])
        rel-spec-sym   (get-in metadata [:fm/rel    ::meta/sym])
        ignore?        (get-in metadata [:fm/ignore ::meta/form] #{})]

    `(let [~nonse-sym (do ~@body)]
       (cond
         (s/valid? :fm/anomaly ~nonse-sym)
         ~nonse-sym

         ~@(when (and
                  (contains? metadata :fm/nonse)
                  (not (ignore? :fm/nonse)))

             [`(not (s/valid? ~nonse-spec-sym ~nonse-sym))
              `#::anomaly{:spec ::anomaly/nonse
                          :sym  '~sym
                          :args ~args-syms
                          :data (s/explain-data ~nonse-spec-sym ~nonse-sym)}])

         ~@(when (and
                  (contains? metadata :fm/rel)
                  (not (ignore? :fm/rel)))

             [`(not (s/valid? ~rel-spec-sym {:args ~args-syms :ret ~nonse-sym}))
              `#::anomaly{:spec ::anomaly/rel
                          :sym  '~sym
                          :args ~args-syms
                          :data (s/explain-data ~rel-spec-sym {:args ~args-syms :ret ~nonse-sym})}])

         :else
         ~(first args-syms)))))

(defm nonsequent
  [{:fm/keys [macro-sym sym args-form nonse-form]
    :as      form-args}]

  (let [ignore?      (:fm/ignore (meta args-form) (hash-set))
        metadata     (into
                      (hash-map)
                      (map nonseq.meta/nonse-xf)
                      (merge
                       (meta args-form)
                       {:fm/sym    sym
                        :fm/ignore ignore?}
                       (when (seq args-form)  {:fm/args  args-form})
                       (when (seq nonse-form) {:fm/nonse nonse-form})))
        bindings-map (select-keys metadata [:fm/nonse :fm/rel])
        bindings     (interleave
                      (map ::meta/sym  (vals bindings-map))
                      (map ::meta/form (vals bindings-map)))
        path         (vector :fm/ignore ::meta/form)
        fm-metadata  (update-in metadata path conj :fm/rel)
        fm-args-form (with-meta
                       (form.lib/args-form->fm-args-form args-form)
                       (not-empty
                        (zipmap
                         (keys fm-metadata)
                         (map ::meta/form (vals fm-metadata)))))
        args-syms    (if (seq fm-args-form)
                       (vector (:as (first fm-args-form)))
                       (vector))
        fm-body-form (body-form
                      (merge
                       form-args
                       {:fm/metadata  metadata
                        :fm/args-syms args-syms}))]

    `(let [~@bindings]
       (~macro-sym
        ~@(when sym [(symbol (name sym))])
        ~fm-args-form
        ~fm-body-form))))

(comment

  (require '[fm.sequent.macro :refer [nonsequent defnonsequent]])
  (require '[clojure.alpha.spec :as s])

  (s/def ::a int?)
  (s/def ::b int?)

  (nonsequent [] [])

  (nonsequent [::a] [::b])
  ((nonsequent [::a] [::b] {::b 2}) {::a 1})
  ((nonsequent [::a] [::b] {::b 'b}) {::a 1})
  ((nonsequent [::a] [::b] {::b 2}) {::a 'a})

  (nonsequent [::a ::b] [])
  ((nonsequent [::a ::b] []) {::a 1 ::b 2})
  ((nonsequent [::a ::b] []) {::a 1 ::b 'b})

  (nonsequent [] [::a ::b])
  ((nonsequent [] [::a ::b] {::a 1 ::b 2}))
  ((nonsequent [] [::a ::b] {::a 1 ::b 'b}))

  (->>
   {::a 1 ::b 2}
   ((nonsequent
     [::a ::b]
     []
     (prn "!"))))

  )
