# fm

```clojure
(s/def ::a int?)
(s/def ::b int?)

(fm/defn add [::a ::b]
  (+ a b))
```

# 

`fm` provides syntax for specifying runtime invariants via
[`clojure.spec`](https://github.com/clojure/spec.alpha/)
