(ns naphthalimide.alpha
  (:refer-clojure :exclude [defn- defn fn fn*])
  (:require [clojure.core :as clj]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer :as tracer]
            [potemkin :refer [import-vars]]))


(import-vars
  [naphthalimide.alpha.tracer
   global-tracer register-global-tracer! with-tracer])


(clj/defn- destruct-syms
  [binding]
  (remove #{'&}
          ((clj/fn syms [x]
             (cond (symbol? x) [x]
                   (coll? x)   (mapcat syms x)))
            binding)))



(clj/defn- meta-from
  [expr meta-source]
  (vary-meta expr merge
             (meta meta-source)))


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
    `(let [~@(remove #{'&} tag-bindings)
           tags# ~(merge form-meta ns-meta
                         (zipmap (map name tag-names)
                                 tag-names))]
       (span/within-scope (span/start ~(name span-name)
                                      (span/child-of (tracer/active-span))
                                      (span/tags tags#))
         ~@body))))


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
    (meta-from
      `(clj/fn ~@prelude
         ~@(for [arity arities]
             (let [[argv & body] arity]
               (meta-from `(~argv
                             ~(meta-from `(span ~fn-name ~(vec (mapcat (partial repeat 2)
                                                                       (destruct-syms argv)))
                                                ~@body)
                                         arity))
                          arity))))
      &form)))


(clj/defmacro defn
  [name & defn-form]
  (let [{:keys [prelude arities]}
        (parse-fn (cons name defn-form))]
    (meta-from `(def ~@prelude
                  ~(meta-from `(fn ~name ~@arities) &form))
               &form)))



(comment
  ;; Register the Jaeger Tracer
  (register-global-tracer!
    (-> (com.uber.jaeger.Configuration. "testing")
        (.withSampler (-> (com.uber.jaeger.Configuration$SamplerConfiguration.)
                          (.withType "const")
                          (.withParam 1)))
        (.withReporter (-> (com.uber.jaeger.Configuration$ReporterConfiguration.)
                           (.withLogSpans true)
                           (.withFlushInterval (int 1000))
                           (.withMaxQueueSize (int 10000))
                           (.withSender (-> (com.uber.jaeger.Configuration$SenderConfiguration.)
                                            (.withAgentHost "127.0.0.1")
                                            (.withAgentPort (int 5775))))))
        (.getTracer)))

  ;; Sample Recursive functions with randomized
  ;; execution time.
  (let [f1 (fn f1 [a b]
             (Thread/sleep (rand-int 50))
             (+ a b))
         f2 (fn f2 ([] 0)
                   ([a] a)
                   ([a b] (f1 a b))
                   ([a b & more]
                     (reduce f2 a (cons b more))))]
     (f2 1 2 3 4 5 6 7))

  )
