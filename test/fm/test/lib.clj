(comment

  (lib/rreduce
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

  )
