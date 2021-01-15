# fm

```clojure
(require '[clojure.math.combinatorics :as combinatorics])

(def fm
  (rand-nth
   (combinatorics/cartesian-product
    #{:function :failure :formal :frequency :fix :free :functor :form}
    #{:metadata :mode :method :modulation :merge :monad :map :markup
      :medium :multi :meta :manifold :monoid :modulus :morphism :meron}
    ;; sometimes #{:language}
    )))

  ;; NOTE: I don't know what any of these words mean
```

# 

`fm` provides syntax for specifying runtime invariants via
[`clojure.spec`](https://github.com/clojure/spec.alpha/)
