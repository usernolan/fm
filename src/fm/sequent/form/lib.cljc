(ns fm.sequent.form.lib
  (:require
   [clojure.alpha.spec :as s]
   [clojure.set :as set]
   [fm.macro :refer [defm]]
   [fm.form.lib :as form.lib]))

(s/def ::seq-form
  (s/*
   (s/alt
    ::ass          #{:as}
    ::binding-syms ::form.lib/binding-sym
    ::ns-kws       ::form.lib/ns-kw
    ::kws          keyword?
    ::maps         map?)))

(def seq-form?
  (partial s/valid? ::seq-form))

(s/def ::ass
  (s/* #{:as}))

(s/def ::binding-syms
  (s/tuple ::form.lib/binding-sym))

(s/def ::ns-kws
  (s/coll-of ::form.lib/ns-kw :into #{}))

(s/def ::kws
  (s/coll-of keyword? :into #{}))

(s/def ::maps
  (s/coll-of map? :into #{}))

(s/def ::seq-form-schema
  (s/schema
   [::ass
    ::binding-syms
    ::ns-kws
    ::kws
    ::maps]))

(defm seq-form->map
  ^{:fm/args    ::seq-form
    :fm/ret     ::seq-form-schema
    :fm/conform true}
  [seq-form]
  (->>
   (group-by first seq-form)
   (into
    (hash-map)
    (map (fn [[k v]] [k (mapv second v)])))))

(def map-fmt-xf
  (partial
   into
   (hash-map)
   (map
    (fn [[k v]]
      (if (keyword? k)
        [v k]
        [k v])))))

(defm seq-form->sorted-kws
  ^{:fm/args ::seq-form
    :fm/ret  ::kws}

  [seq-form]

  (let [{::keys [ass binding-syms ns-kws kws maps]}
        (seq-form->map seq-form)

        kws'   (set (vals (into {} (map map-fmt-xf) maps)))
        as-kw? (and
                (not (contains? kws' :as))
                (or
                 (and
                  (nil? binding-syms)
                  (> (count ass) 0))
                 (> (count ass) 1)))
        kws'   (if as-kw? (conj kws' :as) kws')]

    (vec (sort (set/union ns-kws kws kws')))))

(s/def ::ns          string?)
(s/def ::form        seq-form?)
(s/def ::left-form   seq-form?)
(s/def ::right-form  seq-form?)
(s/def ::conseq-data (s/keys :req [::ns ::form]))
(s/def ::seq-data    (s/keys :req [::ns ::left-form ::right-form]))

(s/def ::req    (s/and ::form.lib/ns-kw-vec seq))
(s/def ::req-un (s/and ::form.lib/ns-kw-vec seq))
(s/def ::keys
  (s/keys
   :opt
   [::req
    ::req-un]))

(defm conseq-data->keys
  ^{:fm/args ::conseq-data
    :fm/ret  ::keys}

  [{::keys [ns form]}]

  (let [{req    true
         req-un false} (group-by
                        (comp boolean namespace)
                        (seq-form->sorted-kws form))
        ns-xf          (comp (partial keyword ns) name)
        req-un         (into [] (map ns-xf) req-un)
        req            (vec req)]

    (merge
     (when (seq req)    {::req req})
     (when (seq req-un) {::req-un req-un}))))

(defm keys->keys-form
  ^{:fm/args ::keys}

  [{::keys [req
            req-un]}]

  `(s/keys
    ~@(when (seq req)    [:req req])
    ~@(when (seq req-un) [:req-un req-un])))

(def conseq-data->keys-form
  (comp
   keys->keys-form
   conseq-data->keys))

(defn seq-data->keys-form
  ^{:fm/args (s/or
              ::seq    ::seq-data
              ::conseq ::conseq-data)}

  [{::keys [ns form left-form right-form right?]}]

  (if form

    (conseq-data->keys-form {::ns ns ::form form})

    (let [left-keys  (conseq-data->keys-form {::ns ns ::form left-form})
          right-keys (conseq-data->keys-form {::ns ns ::form right-form})]

      (if right?
        `(s/or
          ~@(when (seq right-form) [:fm.sequent/right right-keys])
          ~@(when (seq left-form)  [:fm.sequent/left left-keys]))

        `(s/or
          ~@(when (seq left-form)  [:fm.sequent/left left-keys])
          ~@(when (seq right-form) [:fm.sequent/right right-keys]))))))

(defm keys->coll-or-form
  ^{:fm/args ::keys}

  [{::keys [req
            req-un]}]

  `(s/or
    ~@(when (seq req) (interleave req req))
    ~@(when (seq req-un)
        `(::req-un
          (s/keys
           :req-un
           ~req-un))))) ; TODO: allow dust?

(defm keys->coll-form
  ^{:fm/args ::keys}

  [keys]

  `(s/coll-of
    ~(keys->coll-or-form keys)
    :into []))

(defm keys->or-form
  ^{:fm/args ::keys}

  [{::keys [req req-un] :as keys}]

  (let [ks    (into req req-un)
        only? (= (count ks) 1)
        keys? (seq ks)
        coll? (seq ks)]

    `(s/or
      ~@(when (and only? (seq req))    [(first req) (first req)])
      ~@(when keys?                    [::keys (keys->keys-form keys)])
      ~@(when coll?                    [::coll (keys->coll-form keys)])
      ~@(when (and only? (seq req-un)) [(keyword (name (first req-un))) `any?]))))

(def conseq-data->or-form
  (comp
   keys->or-form
   conseq-data->keys))

(defm seq-data->or-form
  ^{:fm/args (s/or
              ::seq    ::seq-data
              ::conseq ::conseq-data)}

  [{::keys [ns form left-form right-form right?]}]

  (if form
    (conseq-data->or-form {::ns ns ::form form})

    (let [left-or  (conseq-data->or-form {::ns ns ::form left-form})
          right-or (conseq-data->or-form {::ns ns ::form right-form})]

      (if right?
        `(s/or
          ~@(when (seq right-form) [:fm.sequent/right right-or])
          ~@(when (seq left-form)  [:fm.sequent/left  left-or]))

        `(s/or
          ~@(when (seq left-form)  [:fm.sequent/left  left-or])
          ~@(when (seq right-form) [:fm.sequent/right right-or]))))))

(def conseq-data->coll-or-form
  (comp
   keys->coll-or-form
   conseq-data->keys))

(defn seq-data->coll-or-form
  ^{:fm/args (s/or
              ::seq    ::seq-data
              ::conseq ::conseq-data)}

  [{::keys [ns form left-form right-form right?]}]

  (if form
    (conseq-data->coll-or-form {::ns ns ::form form})

    (let [left-coll-or  (conseq-data->coll-or-form {::ns ns ::form left-form})
          right-coll-or (conseq-data->coll-or-form {::ns ns ::form right-form})]

      (if right?
        `(s/or
          ~@(when (seq right-form) [:fm.sequent/right right-coll-or])
          ~@(when (seq left-form)  [:fm.sequent/left  left-coll-or]))

        `(s/or
          ~@(when (seq left-form)  [:fm.sequent/left  left-coll-or])
          ~@(when (seq right-form) [:fm.sequent/right right-coll-or]))))))

(defmulti  binding-xf (fn [[k _]] k))
(defmethod binding-xf ::left-or
  [[k v]]
  [k {::form.lib/sym  (gensym (name k))
      ::form.lib/form (seq-data->or-form v)}])

(defmethod binding-xf ::right-or
  [[k v]]
  [k {::form.lib/sym  (gensym (name k))
      ::form.lib/form (seq-data->or-form (assoc v ::right? true))}])

(defmethod binding-xf ::nonse-or
  [[k v]]
  [k {::form.lib/sym  (gensym (name k))
      ::form.lib/form (seq-data->or-form v)}])

(defmethod binding-xf ::left-coll-or
  [[k v]]
  [k {::form.lib/sym  (gensym (name k))
      ::form.lib/form (seq-data->coll-or-form v)}])

(defmethod binding-xf ::right-coll-or
  [[k v]]
  [k {::form.lib/sym  (gensym (name k))
      ::form.lib/form (seq-data->coll-or-form (assoc v ::right? true))}])

(defmethod binding-xf ::nonse-coll-or
  [[k v]]
  [k {::form.lib/sym  (gensym (name k))
      ::form.lib/form (seq-data->coll-or-form v)}])

(defmethod binding-xf :default
  [x]
  (form.lib/binding-xf x))

(s/def ::syms
  (s/or
   :_1 (s/every-kv
        (s/or
         ::binding-sym ::form.lib/binding-sym
         ::as          #{:as})
        (s/or
         ::kw          keyword?
         ::binding-sym ::form.lib/binding-sym))
   :_0 #{{}}))

(defm seq-form->syms
  ^{:fm/args ::seq-form
    :fm/ret  ::syms}

  [seq-form]

  (if (seq seq-form)
    (let [{::keys [ass binding-syms ns-kws kws maps]}
          (seq-form->map seq-form)

          map-fmt (into {} (map map-fmt-xf) maps)
          as-kw?  (and
                   (not (contains? (set (vals map-fmt)) :as))
                   (or
                    (and
                     (nil? binding-syms)
                     (> (count ass) 0))
                    (> (count ass) 1)))
          kws     (if as-kw? (conj kws :as) kws)]

      (merge
       (when (seq map-fmt) map-fmt)
       (when (seq ns-kws)  (zipmap (map (comp symbol name) ns-kws) ns-kws))
       (when (seq kws)     (zipmap (map symbol kws) kws))
       {:as (if (seq binding-syms)
              (first binding-syms)
              (gensym))}))

    {}))

(defn req-un-xf
  [[k v]]
  (if (= k ::req-un)
    v
    [k v]))

(def directional?
  (comp
   #{:fm.sequent/left
     :fm.sequent/right}
   first))

(defn match-xf
  [or-spec x]
  (let [c (s/conform or-spec x)]
    (cond
      (s/invalid? c)   c
      (directional? c) (first (second c))
      :else            (first c))))

(defn or-conform-data->map
  [{::keys [conform? conformed data or-spec]
    :as    args}]
  (let [[tag conformed-data] conformed]
    (case tag
      :fm.sequent/left  (or-conform-data->map (update args ::conformed second))
      :fm.sequent/right (or-conform-data->map (update args ::conformed second))
      ::keys            (if conform? conformed-data data)
      ::coll            (if conform?
                          (into {} (map req-un-xf) conformed-data)
                          (into
                           (hash-map)
                           (map req-un-xf)
                           (zipmap
                            (map (partial match-xf or-spec) data)
                            (map identity data))))
      {tag (if conform? conformed-data data)})))
