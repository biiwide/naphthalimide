(ns naphthalimide.alpha-test
  (:require [clojure.test :refer :all]
            [naphthalimide.alpha :as trace]))



(defn demo-span-fn
  [a b c]
  (trace/span outer-span
              [{:keys [x y]} a
               b b]
    (trace/span inner-span
                [b b c c]
      (str a b c))))


(trace/defn traced-fn-1
  [a b]
  (+ a b))

(trace/defn ^Number traced-fn-2
  "docstring"
  ([] 1)
  ([a] a)
  ([a b] (* a b)))

(trace/defn traced-fn-3
  [[a b] {:keys [c d]}]
  (list a b c d))
