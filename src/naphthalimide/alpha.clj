(ns naphthalimide.alpha
  (:refer-clojure :exclude [defn- defn fn fn*])
  (:require [clojure.core :as clj]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer :as tracer]
            [naphthalimide.internal :refer
             [parse-fn definline*]]
            [potemkin :refer [import-vars]]))


(import-vars
  (naphthalimide.alpha
    (tracer global-tracer
            register-global-tracer!
            with-tracer)))


(def active-span span/active)


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


(clj/defn- namespaced-name
  [sym]
  (if (and (instance? clojure.lang.Named sym)
           (some? (namespace sym)))
    (str sym)
    (recur (symbol (str (ns-name *ns*))
                   (name sym)))))


(clj/defmacro span
  "Denotes a block of code as a span.
The tag-binding may include any values to be used for tags.
The tag names will be available for use within the body."
  [span-name tag-map & body]
  (let [[tag-map body] (if (or (not (map? tag-map))
                               (empty? body))
                         [[] (cons tag-map body)]
                         [tag-map body])]
    `(span/within-scope (span/start ~(namespaced-name span-name)
                                    (span/with-tags ~tag-map))
       ~@body)))


(clj/defmacro let-span
  "Denotes a block of code as a span.
The tag-binding may include any values to be used for tags.
The tag names will be available for use within the body."
  [span-name tag-bindings & body]
  (let [[tag-bindings body] (if (or (not (vector? tag-bindings))
                                    (empty? body))
                              [[] (cons tag-bindings body)]
                              [tag-bindings body])
        tag-names (destruct-syms
                    (keys (apply hash-map tag-bindings)))]
    `(let [~@(remove #{'&} tag-bindings)]
       (span ~span-name
             ~(zipmap (map name tag-names)
                      tag-names)
             ~@body))))


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
                             ~(meta-from `(let-span ~fn-name ~(vec (mapcat (partial repeat 2)
                                                                           (destruct-syms argv)))
                                                ~@body)
                                         arity))
                          arity))))
      &form)))


(clj/defmacro defn
  [name & defn-form]
  (let [{:keys [prelude arities]}
        (parse-fn (cons name defn-form) )]
    (binding [*print-meta* true]
      (meta-from `(def ~@prelude
                    (fn ~name ~@(map #(vary-meta % (constantly nil))
                                     arities)))
                 &form))))


(definline* log!
  "Adds a log entry to the active span."
  ([map-or-message]
    `(if-some [span# (span/active)]
              (span/log! span# ~map-or-message)))
  ([epoch-micros map-or-message]
    `(if-some [span# (span/active)]
              (span/log! span# ~epoch-micros ~map-or-message))))


(definline* set-tag!
  "Sets a tag on the active span."
  [key value]
  `(when-some [span# (span/active)]
     (span/set-tag! span# ~key ~value)))


(definline* set-baggage-item!
  "Sets a baggage item on the active span."
  [key value]
  `(when-some [span# (span/active)]
     (span/set-baggage-item! span# ~key ~value)))
  

(comment
  ;; Register a Jaeger Tracer
  (register-global-tracer!
    (-> (com.uber.jaeger.Configuration. "testing")
        (.withSampler (-> (com.uber.jaeger.Configuration$SamplerConfiguration.)
                          (.withType "const")
                          (.withParam 1)))
        (.withReporter (-> (com.uber.jaeger.Configuration$ReporterConfiguration.)
                           (.withLogSpans true)
                           (.withFlushInterval (int 2000))
                           (.withMaxQueueSize (int 10000))
                           (.withSender (-> (com.uber.jaeger.Configuration$SenderConfiguration.)
                                            (.withAgentHost "127.0.0.1")
                                            (.withAgentPort (int 5775))))))
        (.getTracer)))

  ;; Register a Jaeger Tracer configured from environment
  (register-global-tracer!
    (com.uber.jaeger.Configuration/fromEnv))
  
  ;; Sample Recursive functions with randomized
  ;; execution time.
  (let [f1 (trace/fn f1 [a b]
             (Thread/sleep (rand-int 50))
             (+ a b))
         f2 (trace/fn f2 ([] 0)
                   ([a] a)
                   ([a b] (f1 a b))
                   ([a b & more]
                     (reduce f2 a (cons b more))))]
     (f2 1 2 3 4 5 6 7))
  )
