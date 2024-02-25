(ns naphthalimide.beta-test
  (:require [clojure.test :refer [are deftest is]]
            [matcher-combinators.matchers :as m]
            matcher-combinators.test
            [naphthalimide.alpha.tracer :as tracer]
            [naphthalimide.alpha.tracer.mock :as mock]
            [naphthalimide.beta :as trace])
  (:import (io.opentracing Span)))


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
             :source.ns "naphthalimide.beta-test"
             :source.file "naphthalimide/beta_test.clj"}
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
    [{:operation "naphthalimide.beta-test/throws",
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
             :source.ns "naphthalimide.beta-test"
             :source.file "naphthalimide/beta_test.clj"}
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


(trace/defn traced-fn-0
  [a b]
  (+ a b))


(trace/defn traced-fn-1
  [a ^::trace/tag b]
  (+ a b))


(trace/defn ^Number traced-fn-2
  "docstring"
  ([] 0)
  ([a] a)
  ([^::trace/tag a ^::trace/tag b]
    (traced-fn-1 a b))
  ([^{::trace/tag aa} a1
    ^{::trace/tag bb} b1
    & ^{:abc 123
        ::trace/tag others
        :some "Thing"} more]
    (reduce traced-fn-1 a1 (cons b1 more))))


(trace/defn traced-fn-3
  [[a ^::trace/tag b] {:keys [c ^::trace/tag d]}]
  (list a b c d))


(deftest test-traced-fns
  (are [invocation expected-spans]
    (trace/with-tracer (mock/tracer)
      (try invocation (catch Exception _ nil))
      (is (match? expected-spans
                  (mock/finished-spans (tracer/global-tracer)))
          (format "Test Case: %s" (pr-str (quote invocation)))))

    (traced-fn-0 1 2)
    [{:operation (str `traced-fn-0)
      :tags (m/equals {:source.ns "naphthalimide.beta-test"
                       :source.file "naphthalimide/beta_test.clj"
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (traced-fn-1 1 2)
    [{:operation (str `traced-fn-1)
      :tags (m/equals {:b 2
                       :source.ns "naphthalimide.beta-test"
                       :source.file "naphthalimide/beta_test.clj"
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (traced-fn-1 "x" "y")
    [{:operation (str `traced-fn-1)
      :tags {:b "y"
             :source.ns "naphthalimide.beta-test"
             :source.file "naphthalimide/beta_test.clj"
             :error true}
      :log [{:fields {"error.kind" "Exception"
                      "error.object" #(instance? ClassCastException %)}}]}]

    (traced-fn-2)
    [{:operation (str `traced-fn-2)
      :tags (m/equals {:source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (traced-fn-2 :just-a)
    [{:operation (str `traced-fn-2)
      :tags (m/equals {:source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (traced-fn-2 8 9)
    [{:operation (str `traced-fn-1)
      :tags {:b 9}}
     {:operation (str `traced-fn-2)
      :tags (m/equals {:a 8
                       :b 9
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (traced-fn-2 1 2 3 4)
    [{:operation (str `traced-fn-1)
      :tags {:b 2}}
     {:operation (str `traced-fn-1)
      :tags {:b 3}}
     {:operation (str `traced-fn-1)
      :tags {:b 4}}
     {:operation (str `traced-fn-2)
      :tags (m/equals {:aa 1
                       :bb 2
                       :others "(3 4)"
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]

    (traced-fn-3 [1 2] {:c 3 :d 4})
    [{:operation (str `traced-fn-3)
      :tags (m/equals {:b 2
                       :d 4
                       :source.ns string?
                       :source.file string?
                       :source.line pos-int?
                       :source.column pos-int?})}]
    ))


(defn span? [x]
  (instance? Span x))


(deftest test-log!
  (are [form expected]
    (trace/with-tracer (mock/tracer)
      (is (span? form))
      (is (match? expected
                  (mock/finished-spans (tracer/global-tracer)))))

    (trace/span "log1" (trace/log! "abc"))
    [{:operation (str `log1)
      :log [{:timestamp-micros integer?
             :fields {"event" "abc"}}]}]

    (trace/span "log2"
      (trace/log! "aaa")
      (trace/log! "bbb")
      (trace/log! "ccc"))
    [{:operation (str `log2)
      :log [{:fields {"event" "aaa"}}
            {:fields {"event" "bbb"}}
            {:fields {"event" "ccc"}}]}]

    (trace/span "log3"
      (trace/log! {:a "map"
                   :of :stuff
                   :xyz 123
                   :list [4 5 6]}))
    [{:operation (str `log3)
      :log [{:fields {"a"   "map"
                      "of"  :stuff
                      "xyz" 123
                      "list" [4 5 6]}}]}]

    (trace/span "log4" (trace/log! (trace/active-span)
                                   "log message 4"))
    [{:operation (str `log4)
      :log [{:fields {"event" "log message 4"}}]}]
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
