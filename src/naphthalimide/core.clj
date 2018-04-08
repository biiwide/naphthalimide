(ns naphthalimide.core
  (:refer-clojure :exclude [fn fn*])
  (:require [clojure.core :as clj]
            [naphthalimide.span :as span]
            [naphthalimide.tracer :refer [global-tracer]]))


(defn fn*
  [f f-name arg-names]
  (clj/fn [& args]
    (let [args-tags (-> (zipmap arg-names args)
                        (dissoc nil))
          span (span/build f-name
                 (span/tag-map args-tags))]
      (try (apply f args)
        (finally (span/finish span))))))


(defmacro fn
  [fn-name argv & body]
  (assert (symbol? fn-name)
          "Traced Functions must have a name")
  (assert (vector? argv)
          "Arguments must be a vector")
  `(fn*
     (clj/fn ~fn-name ~argv ~@body)
     ~(name (symbol (name (ns-name *ns*))
                    (name fn-name)))
     ~(mapv name argv)))
            