# naphthalimide

Idiomatic Clojure support for OpenTracing.

## Usage

```
(require '[naphthalimide.alpha :as trace])
```

An anonymous, traced function:

```
(let [f (trace/fn important
          [a b]
          (+ a b))]
  (f 1 2))
```
All function arguments will be passed as tags.

A defined, traced function:

```
(trace/defn my-traced-function
  "Documentation..."
  ([a] a)
  ([a b] (+ a b))
```

A traced section of code:

```
(fn [a b]
  (trace/span my-inline-span
              [c (str a b)
               d (str b a)]
    [c d]))
```

The span will be tagged with `c` and `d`.


## License

Copyright © 2018 Theodore Cushman

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
