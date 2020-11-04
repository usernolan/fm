(comment

  (fm/defn fm1
    ^{:fm/doc  "variadic increment-sum"
      :fm/args [int? int? & int?]
      :fm/ret  int?
      :fm/rel  (fn [{args :args ret :ret}]
                 (> ret (apply + args)))

      :fm.anomaly/handler prn}
    ([a] (inc a))
    ([a b] (inc (+ a b)))
    ([a b & cs] (inc (apply + a b cs))))

  ;;;
  )
