# naphthalimide

Idiomatic Clojure support for OpenTracing.

## Usage

### Dependency information

```clojure
[biiwide/naphthalimide "0.0.5"]
```

### Sample Usage

```clojure
(require '[naphthalimide.beta :as trace])
```

An anonymous, traced function:

```clojure
(let [f (trace/fn important
          [^::trace/tag a b]
          (+ a b))]
  (f 1 2))
```

Function arguments annotaed with :naphthalimide.beta/tag will be
added to the span as tags.

A defined, traced function:

```clojure
(trace/defn my-traced-function
  "Documentation..."
  ([^::trace/tag a] a)
  ([^::trace/tag a ^::trace/tag b] (+ a b)))
```

A traced block of code:

```clojure
(fn [a b]
  (trace/let-span my-inline-span
                  [c (str a b)
		   d (str b a)]
    (list c d)))
```

The span `my-inline-span` will be created and tagged with `c` and `d`.


## License

Copyright © 2022 Theodore Cushman

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
