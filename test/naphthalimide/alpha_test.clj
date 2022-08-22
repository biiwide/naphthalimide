(ns naphthalimide.alpha-test
  (:require [clojure.test :refer :all]
            matcher-combinators.test
            [naphthalimide.alpha :as trace]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer.mock :as mock]
            ))


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


(deftest test-span
  (are [form expected]
    (trace/with-tracer (mock/tracer)
      (try form (catch Exception _ nil))
      (is (match? expected (mock/finished-spans (trace/global-tracer)))))

    (trace/span "a+b" {:a 1 :b 2} 3)
    [{:operation (str `a+b)
      :context {:trace-id integer?
                :span-id integer?}
      :start-micros integer?
      :finish-micros integer?
      :tags {:a 1
             :b 2
             :source.ns "naphthalimide.alpha-test"
             :source.file "naphthalimide/alpha_test.clj"}
      :trace-id integer?}]

    (trace/span "outer" {:a {:uno "one"}}
      (trace/span "inner" {:b {:dos "two"}}
        "something"))
    [{:operation (str `inner)
      :tags {:b "{:dos \"two\"}"}}
     {:operation (str `outer)
      :tags {:a "{:uno \"one\"}"}}]

    (trace/span "throws" {:x :y}
      (throw (IllegalArgumentException. "Boom!")))
    [{:operation "naphthalimide.alpha-test/throws",
      :context map?
      :tags {:x ":y"
             :error true}
      :log [{:timestamp-micros integer?
             :fields {"error.kind" "Exception",
                      "error.object" #(instance? IllegalArgumentException %)}}]}]
    ))


(deftest test-nested-traced-fns
  (let [fn1 (trace/fn fn1 [a b] (+ a b))
        fn2 (trace/fn fn2
              ([a b] (fn1 a b))
              ([a b & more] (reduce fn1 a (cons b more))))]
    #_(are [expected xform expr]
          (let [tracer (mock/tracer)]
            (try expr (catch Exception e nil))
            (= expected
               (map xform (mock/finished-spans tracer))))
         
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
