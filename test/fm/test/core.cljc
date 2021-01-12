(comment ; WIP: sketch project

  (project { ,,, } [ ,,, ])

  (defn project [ctx path]
    ((resolve
      (first path))
     ctx
     (vec
      (rest path))))

  (project
   {:fm.form/definition ,,,}
   [:fm.form/->form ,,,])

  ;;;
  )
