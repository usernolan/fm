(ns fm.sequent.form.lib
  (:require
   [clojure.alpha.spec :as s]
   [fm.macro :refer [defm]]))

(def nil-ns?
  (comp nil? namespace))

(s/def ::ns-kw       (s/and keyword? namespace))
(s/def ::binding-sym (s/and symbol? nil-ns?))

(def binding-sym?
  (partial s/valid? ::binding-sym))

(s/def ::ns-kw-or-binding-sym
  (s/or
   ::ns-kw       ::ns-kw
   ::binding-sym ::binding-sym))

(s/def ::binding-map
  (s/and
   map?
   (fn [m]
     (or
      (s/valid? (s/every-kv ::ns-kw ::binding-sym) m)
      (s/valid? (s/every-kv ::binding-sym ::ns-kw) m)))))

(defm properly-bound?
  ^{:fm/args sequential?
    :fm/ret  boolean?}
  [s]
  (let [as  (filter (comp #{:as} second) s)
        sym (filter (comp binding-sym? second) s)]
    (or
     (and
      (empty? as)
      (empty? sym))
     (and
      (= 1 (count as))
      (= 1 (count sym))))))

(s/def ::args-form
  (s/and
   (s/*
    (s/alt
     :as            #{:as}
     ::binding-syms ::binding-sym
     ::ns-kws       ::ns-kw
     ::binding-maps ::binding-map))
   properly-bound?))

(s/def ::binding-syms
  (s/tuple ::binding-sym))

(s/def ::ns-kws
  (s/* ::ns-kw))

(s/def ::binding-maps
  (s/* ::binding-map))

(s/def ::args-form-schema
  (s/schema
   [::ns-kws
    ::binding-maps
    ::binding-map
    ::binding-syms]))

(defm args-form->map
  ^{:fm/args ::args-form
    :fm/ret  ::args-form-schema}
  [args-form]
  (let [xf (fn [[k v]] [k (mapv second v)])]
    (->>
     (s/conform ::args-form args-form)
     (group-by first)
     (into {} (map xf)))))

(defm binding-map-xf
  ^{:fm/args ::binding-map
    :fm/ret  ::binding-map}
  [m]
  (if (symbol? (ffirst m))
    m
    (zipmap
     (vals m)
     (keys m))))

(s/def ::args-select-form any?)

(defm args-form->select-form
  ^{:fm/args ::args-form
    :fm/ret  ::args-select-form}
  [args-form]
  (let [{::keys [ns-kws binding-maps]}
        (args-form->map args-form)
        ks (->>
            binding-maps
            (into {} (map binding-map-xf))
            (vals)
            (into ns-kws))]
    `(s/select ~ks [~'*])))

(s/def ::ns-kw-binding-tuple
  (s/tuple symbol? ::ns-kw))

(defm ns-kw-binding-xf
  ^{:fm/args ::ns-kw
    :fm/ret  ::ns-kw-binding-tuple}
  [ns-kw]
  [(symbol (name ns-kw)) ns-kw])

(s/def ::fm-args-form
  (s/or
   :_1 (s/tuple
        (s/every-kv
         (s/or
          ::binding-sym ::binding-sym
          :as           #{:as})
         (s/or
          ::ns-kw       ::ns-kw
          ::binding-sym ::binding-sym)))
   :_0 #{[]}))

(defm args-form->fm-args-form
  ^{:fm/args ::args-form
    :fm/ret  ::fm-args-form}
  [args-form]
  (if (seq args-form)
    (let [{::keys [ns-kws binding-maps binding-syms]}
          (args-form->map args-form)]
      [(merge
        (into {} (map binding-map-xf) binding-maps)
        (into {} (map ns-kw-binding-xf) ns-kws)
        {:as (if binding-syms (first binding-syms) (gensym))})])
    []))

(comment

  (def args-form1 [::a ::b {::c 'c} {'d ::d} :as 'x])

  (s/valid? ::binding-map {'a :k/v})
  (s/valid? ::binding-map {:k/v 'a})
  (s/valid? ::binding-map {:k 'a})
  (s/valid? ::binding-map {:k/v 'a/b})

  (s/conform ::args-form args-form1)
  (s/conform ::args-form [::a ::b {::c 'c}])
  (s/conform ::args-form [:as 'x])
  (s/conform ::args-form [])

  (s/def ::a int?)
  (s/def ::b int?)
  (s/def ::c int?)
  (s/def ::d int?)

  (def args-form-map1 (args-form->map args-form1))
  (s/explain ::args-form-schema args-form-map1)

  (def fm-args-form1 (fm-args-form args-form-map1))

  (->>
   [:as 'x]
   (args-form->map)
   (fm-args-form))

  (args-select-form args-form-map1)
  (fm-args-form args-form-map1))
