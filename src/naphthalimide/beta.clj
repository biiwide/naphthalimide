(ns naphthalimide.beta
  (:refer-clojure :exclude [defn- defn fn fn*])
  (:require [clojure.core :as clj]
            naphthalimide.alpha
            [naphthalimide.alpha.span :as span]
            naphthalimide.alpha.tracer
            [naphthalimide.internal :refer
             [definline* destruct-syms meta-from parse-fn]]
            [potemkin :refer [import-vars]]))


(import-vars
  (naphthalimide.alpha let-span span)
  (naphthalimide.alpha.tracer global-tracer
                              register-global-tracer!
                              with-tracer))


(def active-span span/active)


(clj/defmacro fn
  [fn-name & fn-form]
  (assert (symbol? fn-name)
          "Traced Functions must have a name")
  (let [{:keys [prelude arities]}
        (parse-fn (cons fn-name fn-form))]
    (meta-from
      `(clj/fn ~@prelude
         ~@(for [arity arities]
             (let [[argv & body] arity
                   tags (mapcat (clj/fn [arg]
                                  (when-some [t (::tag (meta arg))]
                                    [(if (symbol? t) t arg) arg]))
                               (destruct-syms argv))]
               (meta-from `(~argv
                             ~(macroexpand-1
                                (meta-from
                                  (if (empty? tags)
                                    `(span ~fn-name ~@body)
                                    `(let-span ~fn-name
                                       ~(vec tags)
                                       ~@body))
                                  arity)))
                          arity))))
      &form)))


(clj/defmacro defn
  [name & defn-form]
  (let [{:keys [prelude arities]}
        (parse-fn (cons name defn-form) )]
    (binding [*print-meta* true]
      (meta-from `(def ~@prelude
                    ~(macroexpand-1
                       (meta-from `(fn ~name ~@arities) &form)))
                 &form))))


(definline* log!
  "Adds a log entry to either the provided span or the active span."
  ([map-or-message]
   (macroexpand
     `(when-some [span# (span/active)]
        (log! span# ~map-or-message))))
  ([span map-or-message]
   `(span/log! ~span ~map-or-message)))


(definline* set-tag!
  "Sets a tag on either the provided span or the active span."
  ([key value]
   `(when-some [span# (span/active)]
      (set-tag! span# ~key ~value)))
  ([span key value]
   `(span/set-tag! ~span ~key ~value)))


(definline* set-baggage-item!
  "Sets a baggage item on the active span."
  ([key value]
   `(when-some [span# (span/active)]
      (set-baggage-item! span# ~key ~value)))
  ([span key value]
   `(span/set-baggage-item! ~span ~key ~value)))


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
