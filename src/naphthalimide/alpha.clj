(ns naphthalimide.alpha
  (:refer-clojure :exclude [defn- defn fn fn*])
  (:require [clojure.core :as clj]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer]
            [potemkin :refer [import-vars]]))


(import-vars
  [naphthalimide.alpha.tracer
   global-tracer register-global-tracer! with-tracer])


(clj/defn- destruct-syms
  [x]
  (cond (symbol? x) [x]
        (coll? x)   (mapcat destruct-syms x)))


(clj/defmacro span
  "Denotes a block of code as a span.
The tag-binding may include any values to be used for tags.
The tag names will be available for use within the body."
  [span-name tag-bindings & body]
  (let [[tag-bindings body] (if (or (not (vector? tag-bindings))
                                    (empty? body))
                              [[] (cons tag-bindings body)]
                              [tag-bindings body])
        tag-names (destruct-syms
                    (keys (apply hash-map tag-bindings)))
        form-meta (zipmap (map (comp (partial str "source.") name)
                               (keys (meta &form)))
                          (vals (meta &form)))
        ns-meta   {"source.ns"   (name (ns-name *ns*))
                   "source.file" *file*}]
    `(let [~@tag-bindings
           tags# ~(merge form-meta ns-meta
                         (zipmap (map name tag-names)
                                 tag-names))
           span# (span/start ~(name span-name)
                             (span/tags tags#))]
       (try (let [result# (do ~@body)]
              (span/finish! span#)
              result#)
         (catch Throwable t#
           (span/fail-with! span# t#)
           (throw t#))))))


(clj/defn- parse-fn
  [fn-form]
  (if (seq (filter vector? fn-form))
    (let [not-vec? (complement vector?)]
      {:prelude (take-while not-vec? fn-form)
       :arities (list (drop-while not-vec? fn-form))})
    (let [not-list? (complement list?)]
      {:prelude (take-while not-list? fn-form)
       :arities (drop-while not-list? fn-form)})))


(clj/defmacro fn
  [fn-name & fn-form]
  (assert (symbol? fn-name)
          "Traced Functions must have a name")
  (let [{:keys [prelude arities]}
        (parse-fn (cons fn-name fn-form))]
    `(clj/fn ~@prelude
       ~@(for [[argv & body] arities]
           `(~argv
              (span ~fn-name ~(vec (mapcat (partial repeat 2) argv))
                    ~@body))))))


(clj/defmacro defn
  [name & defn-form]
  (let [{:keys [prelude arities]}
        (parse-fn (cons name defn-form))]
    `(def ~@prelude
       (fn ~name ~@arities))))

    