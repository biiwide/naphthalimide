(ns naphthalimide.beta-test
  (:require [clojure.test :refer :all]
            [naphthalimide.beta :as trace]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer.mock :as mock]))



(defn demo-span-fn
  [a b c]
  (trace/let-span outer-span
              [{:keys [x y]} a
               b b]
    (trace/let-span inner-span
                [b b c c]
      (str a b c))))


(trace/defn traced-fn-1
  [a b]
  (+ a b))


(trace/defn ^Number traced-fn-2
  "docstring"
  ([] 0)
  ([a] a)
  ([a b]
    (traced-fn-1 a b))
  ([a b & more]
    (reduce traced-fn-1 a (cons b more))))


(trace/defn traced-fn-3
  [[a b] {:keys [c d]}]
  (list a b c d))


(deftest test-nested-traced-fns
  (let [fn1 (trace/fn fn1 [a b] (+ a b))
        fn2 (trace/fn fn2
              ([a b] (fn1 a b))
              ([a b & more] (reduce fn1 a (cons b more))))]
    (are [expected xform expr]
      (let [tracer (mock/tracer)]
        (trace/with-tracer tracer
          (try expr (catch Exception e nil))
          (is (= expected
                 (map xform (mock/finished-spans tracer))))))

      [] identity (traced-fn-1 1 2)
      )))


(defn jaeger-tracer
  [app-name & {:keys [agent-host agent-port
                      flush-interval max-queue-size]
                 :or {agent-host     "127.0.0.1"
                      agent-port     5775
                      flush-interval 1000
                      max-queue-size 10000}}]
  (-> (com.uber.jaeger.Configuration. app-name)
      (.withSampler (-> (com.uber.jaeger.Configuration$SamplerConfiguration.)
                        (.withType "const")
                        (.withParam 1)))
      (.withReporter (-> (com.uber.jaeger.Configuration$ReporterConfiguration.)
                         (.withLogSpans true)
                         (.withFlushInterval (int flush-interval))
                         (.withMaxQueueSize (int max-queue-size))
                         (.withSender (-> (com.uber.jaeger.Configuration$SenderConfiguration.)
                                          (.withAgentHost agent-host)
                                          (.withAgentPort (int agent-port))))))
      (.getTracer)))


(comment
  (let [tracer1 (jaeger-tracer "client")
        tracer2 (jaeger-tracer "server")]
    (trace/with-tracer tracer1
      (trace/span "client-stuff"
                  (traced-fn-2 2 4 6 8)
                  (let [ctxt (trace/active-span)]
                    )))))
