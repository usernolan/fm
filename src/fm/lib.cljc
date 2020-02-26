(ns fm.lib)

(defn reduce*
  ([recur? f init xs]
   (reduce* recur? f f init xs))
  ([recur? cf f init xs]
   (reduce
    (fn [acc x]
      (if (recur? acc x)
        (cf (reduce* recur? cf f acc x) x)
        (f acc x)))
    init
    xs)))