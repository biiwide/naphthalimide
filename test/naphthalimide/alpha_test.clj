(ns naphthalimide.alpha-test
  (:require [clojure.test :refer [are deftest is]]
            [matcher-combinators.matchers :as m]
            matcher-combinators.test
            [naphthalimide.alpha :as trace]
            [naphthalimide.alpha.tracer :as tracer]
            [naphthalimide.alpha.tracer.mock :as mock]
            ))


(defn demo-span-fn
  [a b c]
  (trace/let-span outer-span
    [{:keys [x y]} a
     b b]
    (trace/let-span inner-span
      [b b c c]
      (str a b c x y))))


(deftest test-span
  (are [form expected]
    (trace/with-tracer (mock/tracer)
      (try form (catch Exception _ nil))
      (is (match? expected (mock/finished-spans (trace/global-tracer)))))

    (is (= 3 (trace/span "a+b" {:a 1 :b 2} 3)))
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

    (is (= "something"
           (trace/span "outer" {:a {:uno "one"}}
             (trace/span "inner" {:b {:dos "two"}}
               "something"))))
    [{:operation (str `inner)
      :tags {:b "{:dos \"two\"}"}}
     {:operation (str `outer)
      :tags {:a "{:uno \"one\"}"}}]

    (is (thrown? IllegalArgumentException
          (trace/span "throws" {:x :y}
            (throw (IllegalArgumentException. "Boom!")))))
    [{:operation "naphthalimide.alpha-test/throws",
      :context map?
      :tags {:x ":y"
             :error true}
      :log [{:timestamp-micros integer?
             :fields {"error.kind" "Exception",
                      "error.object" #(instance? IllegalArgumentException %)}}]}]
    ))


(deftest test-let-span
  (are [form expected]
    (trace/with-tracer (mock/tracer)
      (try form (catch Exception _ nil))
      (is (match? expected (mock/finished-spans (trace/global-tracer)))))

    (is (= 3 (trace/let-span "+ab" [a 1 b 2] (+ a b))))
    [{:operation (str `+ab)
      :context {:trace-id integer?
                :span-id integer?}
      :start-micros integer?
      :finish-micros integer?
      :tags {:a 1
             :b 2
             :source.ns "naphthalimide.alpha-test"
             :source.file "naphthalimide/alpha_test.clj"}
      :trace-id integer?}]

    (is (= 4 (trace/let-span "ab++" [a 1 b (+ a 2)] (+ a b))))
    [{:operation (str `ab++)
      :tags {:a 1
             :b 3}}]
    (is (= 10 (trace/let-span "a" [a 2]
                (trace/let-span "b" [b (+ a 3)]
                  (* a b)))))
    [{:operation (str `b)
      :tags {:b 5}}
     {:operation (str `a)
      :tags {:a 2}}]
    ))


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


(deftest test-traced-fns
  (are [invocation expected-spans]
    (trace/with-tracer (mock/tracer)
      (try invocation (catch Exception _ nil))
      (is (match? expected-spans
                  (mock/finished-spans (tracer/global-tracer)))
          (format "Test Case: %s" (pr-str (quote invocation)))))

    (is (= 3 (traced-fn-1 1 2)))
    [{:operation (str `traced-fn-1)
      :tags {:a 1
             :b 2
             :source.ns "naphthalimide.alpha-test"
             :source.file "naphthalimide/alpha_test.clj"
             :source.line pos-int?
             :source.column pos-int?}}]

    (is (thrown? ClassCastException
          (traced-fn-1 "x" "y")))
    [{:operation (str `traced-fn-1)
      :tags {:a "x"
             :b "y"
             :source.ns "naphthalimide.alpha-test"
             :source.file "naphthalimide/alpha_test.clj"
             :error true}
      :log [{:fields {"error.kind" "Exception"
                      "error.object" #(instance? ClassCastException %)}}]}]

    (is (= (traced-fn-2) 0))
    [{:operation (str `traced-fn-2)
      :tags (m/equals {:source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (is (= :just-a (traced-fn-2 :just-a)))
    [{:operation (str `traced-fn-2)
      :tags (m/equals {:a ":just-a"
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (is (= 17 (traced-fn-2 8 9)))
    [{:operation (str `traced-fn-1)
      :tags {:a 8
             :b 9}}
     {:operation (str `traced-fn-2)
      :tags (m/equals {:a 8
                       :b 9
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (is (= 10 (traced-fn-2 1 2 3 4)))
    [{:operation (str `traced-fn-1)
      :tags {:a 1 :b 2}}
     {:operation (str `traced-fn-1)
      :tags {:a 3 :b 3}}
     {:operation (str `traced-fn-1)
      :tags {:a 6 :b 4}}
     {:operation (str `traced-fn-2)
      :tags (m/equals {:a 1
                       :b 2
                       :more "(3 4)"
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (is (= [1 2 3 4]
           (traced-fn-3 [1 2] {:c 3 :d 4})))
    [{:operation (str `traced-fn-3)
      :tags (m/equals {:a 1 :b 2 :c 3 :d 4
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]
    ))


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
