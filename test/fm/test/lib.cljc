(comment

  (rreduce
   (fn -rec? [acc x]
     (prn "rec?" acc x)
     (vector? x))
   (fn -initf [acc x]
     (prn "initf" acc x)
     (->
      acc
      (update-in (into [:data] (butlast (:path acc))) conj [])
      (update :path conj 0)))
   (fn -rf [acc x]
     (prn "rf" acc x)
     (if (symbol? x)
       (let [i (dec (count (:path acc)))]
         (->
          acc
          (update-in (into [:data] (butlast (:path acc))) conj (str x))
          (update-in [:path i] inc)))
       acc))
   (fn -cf [acc r]
     (prn "cf" acc r)
     (->
      acc
      (assoc :data (:data r))
      (update-in [:path (dec (count (:path acc)))] inc)))
   {:path [0] :data []} 
   '[a [b [c d] [e] [[[f]]]] g [h]])

  (zip
   vector?
   '[a [b [c]]]
   '[int? ::m1])

  (zip
   vector?
   '[a [b [c]]]
   '[int? [:m1 [int?]]])

  (zip
   vector?
   '[a [b [c] :as bs] & ds]
   '[int? [::m1 [int?]] & int?])

  (zip
   vector?
   '[a [b]]
   '[int? [::m1 [int?]] & int?])

  (zipv
   vector?
   '[a [b [c]]]
   '[int? ::m1])

  (zipv
   vector?
   '[a [b [c]]]
   '[int? [:m1 [int?]]])

  (zipv
   vector?
   '[a [b [c] :as bs] & ds]
   '[int? [::m1 [int?]] & int?])

  (zipv
   vector?
   '[a [b]]
   '[int? [::m1 [int?]] & int?])

  (zipf
   vector?
   (fn [a b] b)
   '[a [b [c]]]
   '[int? ::m1])

  (zipf
   vector?
   (fn [a b] b)
   '[a [b [c]]]
   '[int? [::m1 [int?]]])

  (zipf
   vector?
   (fn [a b] b)
   '[a [b [c] :as bs] & ds]
   '[int? [::m1 [int?]] & int?])

  (zipf
   vector?
   (fn [a b] b)
   '[a [b]]
   '[int? [::m1 [int?]] & int?])

  (zipvf
   vector?
   (fn [a b] b)
   '[a [b [c]]]
   '[int? ::m1])

  (zipvf
   vector?
   (fn [a b] b)
   '[a [b [c]]]
   '[int? [::m1 [int?]]])

  (zipvf
   vector?
   (fn [a b] b)
   '[a [b [c] :as bs] & ds]
   '[int? [::m1 [int?]] & int?])

  (zipvf
   vector?
   (fn [a b] b)
   '[a [b]]
   '[int? [::m1 [int?]] & int?])

  (zipvf
   vector?
   (fn [a b] b)
   '[a [b]]
   (repeat `any?))

  (zipvf
   vector?
   (fn [a] (if (= a '&) '& `any?))
   '[a [b [c]] [d] & [e]])

  ;;;
  )
