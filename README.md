# `fm`
A small macro that produces a regular `fn`

```
(defm inc_
  ^{:fm/args number?
    :fm/ret  number?}
  [n]
  (inc n))
```

[Usage](src/fm/utils.cljc#L265)
