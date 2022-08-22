(ns naphthalimide.alpha.span-test
  (:require [clojure.test :refer :all]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer :as tracer]
            [naphthalimide.alpha.tracer.mock :as mock]
            ))


(deftest test-throwable?
  (are [expected? input]
       (expected? (#'span/throwable? input))
       
       true?  (Throwable. "Boom!")
       true?  (RuntimeException.)
       true?  (AssertionError.)
       false? "Error"
       false? nil
       false? -1
       ))


(deftest test-to-string
  (is (= "{:a \"B\"}"
         (span/to-string {:a "B"})))

  (is (= "{:a 1, :b 2, :c 3, ...}"
         (span/to-string {:a 1 :b 2 :c 3 :d 4 :e 5})))

  (is (= "[:a :b]"
         (span/to-string [:a :b])))

  (is (= "[:a :b :c ...]"
         (span/to-string [:a :b :c :d :e :f])))

  (is (= "(2 3)"
         (span/to-string (map inc '(1 2)))))

  (is (= "(0 1 2 ...)"
         (span/to-string (range))))
  
  (is (= "[(0 1 2 ...) (0 1 2 ...) (0 1 2 ...) ...]"
         (span/to-string [(range) (range) (range) (range) (range)])))

  (is (= "{:a (0 1 2 ...), :b (0 1 2 ...), :c (0 1 2 ...), ...}"
         (span/to-string (zipmap [:a :b :c :d :e :f]
                                 (repeatedly range)))))

  (binding [span/*sequence-length* 5]
    (is (= "(0 1 2 3 4 ...)"
           (span/to-string (range))))

    (is (= "{:a 1, :b 2, :c 3, :d 4, :e 5}"
           (span/to-string {:a 1 :b 2 :c 3 :d 4 :e 5}))))
  
  (binding [span/*sequence-length* 2]
    (is (= "{:a (0 1 ...), :b (0 1 ...), ...}"
           (span/to-string (zipmap [:a :b :c :d :e :f]
                                   (repeatedly range)))))))


(deftest test-set-tag!
  (tracer/with-tracer (mock/tracer)
    (are [k input-v expected-v]
      (let [span (span/start "me")]
        (span/set-tag! span k input-v)
        (is (= expected-v (get-in (mock/to-map span)
                                  [:tags (keyword k)]))))

      :key     123  123
      "bool"   true true
      'null    nil  nil
      "double" 3.45 3.45
      "ratio"  1/3  1/3
      :vector  [1 2 3 4] "[1 2 3 ...]"
      )))
