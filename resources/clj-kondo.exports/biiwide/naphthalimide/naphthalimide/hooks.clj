(ns naphthalimide.hooks
  (:require [clj-kondo.hooks-api :as api]
            [clojure.pprint :refer [pprint]]))

(defonce span-names (atom {}))

(defn register-span! [qualified-span-name]
  (let [n (get (swap! span-names update qualified-span-name (fnil inc 0))
               qualified-span-name)]
    (= 1 n)))

(defn qualify-span-name [an-ns a-sym]
  (symbol (str an-ns) (name a-sym)))

(defn analyze-span
  [{:keys [node ns] :as input}]
  (let [[span-name & body] (rest (:children node))]
    (when-not (register-span! (qualify-span-name ns (api/sexpr span-name)))
      (api/reg-finding! (assoc (meta span-name)
                               :message "Duplicate span-name declared in span."
                               :type :naphthalimide/duplicate-spans)))
    {:node (with-meta (api/list-node
                        (cons (api/token-node 'do)
                              body))
                      (meta node))}))

(defn analyze-let-span
  [{:keys [node ns]}]
  (let [[span-name bindings & body] (rest (:children node))]
    (when-not (register-span! (qualify-span-name ns (api/sexpr span-name)))
      (api/reg-finding! (assoc (meta span-name)
                               :message "Duplicate span-name declared in let-span."
                               :type :naphthalimide/duplicate-spans)))
    {:node (with-meta (api/list-node
                        (list* (api/token-node 'clojure.core/let)
                               bindings
                               body))
                      (meta node))}))

(defn analyze-fn
  [{:keys [node ns]}]
  (let [[fn-name & more] (rest (:children node))]
    (when-not (register-span! (qualify-span-name ns (api/sexpr fn-name)))
      (api/reg-finding! (assoc (meta fn-name)
                               :message "Duplicate span-name declared for lambda"
                               :type :naphthalimide/duplicate-spans)))
    {:node (with-meta (api/list-node
                        (list* (api/token-node 'clojure.core/fn)
                               fn-name
                               more))
                      (meta node))}))

(defn analyze-defn
  [{:keys [node ns]}]
  (let [[fn-name & more] (rest (:children node))]
    (when-not (register-span! (qualify-span-name ns (api/sexpr fn-name)))
      (api/reg-finding! (assoc (meta fn-name)
                               :message "Duplicate span-name declared for defn"
                               :type :naphthalimide/duplicate-spans)))
    {:node (with-meta (api/list-node
                        (list* (api/token-node 'clojure.core/defn)
                               fn-name
                               more))
                      (meta node))}))

(defn symbol-node? [x]
  (some-> x api/sexpr symbol?))

(defn normalize-arities
  [fn-body]
  (if (api/vector-node? (first fn-body))
    [(api/list-node fn-body)]
    fn-body))

(def node-class?
  (memoize (fn [clazz] (some #{"interface clj_kondo.impl.rewrite_clj.node.protocols.Node"}
                             (map str (ancestors clazz))))))

(defn node? [x]
  (node-class? (class x)))

(defn walk
  [inner outer form]
  (cond
    (node? form)
    (outer (update form :children #(mapv inner %)))
    :else (outer form)))

(defn meta-dissoc
  [node k?]
  (if-some [md (:meta node)]
    (assoc node :meta (vec (mapcat (fn [meta-node]
                                     (cond (api/token-node? meta-node)
                                           (when (not (k? meta-node))
                                             [meta-node])
                                           (api/map-node? meta-node)
                                           (update meta-node :children
                                                   (partial mapcat #(when (not (k? (first %))) %)))
                                           :else
                                           [meta-node]))
                                   md)))
    node))

(defn prewalk
  [f form]
  (walk (partial prewalk f) identity (f form)))

(defn alias-keyword
  [node qualified-kw]
  (cons qualified-kw nil))

(defn beta-tag? [x]
  (and (qualified-keyword? x)
       (= "tag" (name x))))

(defn analyze-beta-fn
  [{:keys [node ns] :as input}]
  (let [[fn-name & more] (rest (:children node))
        arities (normalize-arities more)
        tag-keys (alias-keyword input :naphthalimide.beta/tag)]
    (when-not (register-span! (qualify-span-name ns (api/sexpr fn-name)))
      (api/reg-finding! (assoc (meta fn-name)
                               :message "Duplicate span-name declared for lambda"
                               :type :naphthalimide/duplicate-spans)))
    (assoc input :node
           (with-meta (api/list-node
                        (list* (api/token-node 'clojure.core/fn)
                               fn-name
                               (mapv (fn [a]
                                       (let [[argv & body] (:children a)]
                                         (api/list-node
                                           (cons (prewalk #(meta-dissoc % (comp beta-tag? api/sexpr))
                                                          argv)
                                                 body))))
                                     arities)))
                      (meta node)))))

(defn analyze-beta-defn
  [{:keys [node ns] :as input}]
  (let [[fn-name & more] (rest (:children node))
        more (if (api/string-node? (first more))
               (rest more)
               more)]
    (assoc input :node
           (with-meta (api/list-node
                        [(api/token-node 'clojure.core/def)
                         fn-name
                         (with-meta (api/list-node
                                      (list* (api/token-node 'naphthalimide.beta/fn)
                                             fn-name
                                             more))
                                    (meta node))])
                      (meta node)))))
