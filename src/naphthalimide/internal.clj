(ns naphthalimide.internal)


(defn meta-from
  [expr meta-source]
  (vary-meta expr (partial merge (meta meta-source))))


(defn parse-fn
  [fn-form]
  (if (some vector? fn-form)
    (let [not-vec? (complement vector?)
          arity (drop-while not-vec? fn-form)]
      {:prelude (take-while not-vec? fn-form)
       :arities (list (with-meta arity (some meta arity)))})
    (let [not-seq? (complement seq?)]
      {:prelude (take-while not-seq? fn-form)
       :arities (drop-while not-seq? fn-form)})))


(defmacro definline*
  "Multi-arity form of clojure.core/definline"
  [name & decl]
  (let [{:keys [prelude arities]} (parse-fn decl)
        macro (eval (cons `fn arities))]
    `(do
       (defn ~name ~@prelude
         ~@(for [[argv] arities]
             (list argv (apply macro argv))))
       (alter-meta! (var ~name) assoc :inline (fn ~name ~@arities))
       (var ~name))))


(defmacro implicit
  [base-fn bindings]
  (let [base-var      (resolve base-fn)
        binding-map   (apply hash-map bindings)
        binding-syms  (into #{} (keys binding-map))
        arglists      (:arglists (meta base-var))]

    `(-> (fn  ~@(for [args arglists]
                  (let [explicit-args (remove binding-syms args)]
                    (list (vec explicit-args)
                          (cons base-fn
                                (for [arg args]
                                  (binding-map arg arg)))))))
         (vary-meta assoc :arglists (for [arglist arglists]
                                      (vec (remove binding-syms arglist)))))))


(defmacro defimplicit
  [name & declaration]
  (let [[prelude [implicit-form]] (split-with (complement sequential?)
                                              declaration)]
    `(let [f# ~(cons `implicit implicit-form)]
       (def ~name ~@prelude f#)
       (alter-meta! (var ~name) assoc :arglists
                    (:arglists (meta f#)))
       (var ~name))))


(defn destruct-syms
  [binding]
  (remove #{'&}
          ((fn syms [x]
             (cond (symbol? x) [x]
                   (coll? x)   (mapcat syms x)))
            binding)))



(defn namespaced-name
  [sym]
  (if (and (instance? clojure.lang.Named sym)
           (some? (namespace sym)))
    (str sym)
    (recur (symbol (str (ns-name *ns*))
                   (name sym)))))
