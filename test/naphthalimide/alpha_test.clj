(ns naphthalimide.alpha-test
  (:require [clojure.test :refer :all]
            [naphthalimide.alpha :as trace]
            [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer.mock :as mock]
            ))



(defn demo-span-fn
  [a b c]
  (trace/span outer-span
              [{:keys [x y]} a
               b b]
    (trace/span inner-span
                [b b c c]
      (str a b c))))


(trace/defn traced-fn-1
  ([a b]
    (+ a b)))


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
    #_(are [expected xform expr]
          (let [tracer (mock/tracer)]
            (try expr (catch Exception e nil))
            (= expected
               (map xform (mock/finished-spans tracer))))
         
          )))
         
         
         
         
