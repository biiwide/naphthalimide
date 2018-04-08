(ns naphthalimide.span
  (:refer-clojure :exclude [boolean?])
  (:require [naphthalimide.tracer :refer [global-tracer]])
  (:import  [io.opentracing References Span SpanContext
                            Tracer Tracer$SpanBuilder]
            ))

(defn- boolean? [x]
  (or (true? x)
      (false? x)))


(defn tag [tag-key tag-val]
  (let [tk (name tag-key)]
    (fn [^Tracer$SpanBuilder builder]
      (cond (nil? tag-val)     (.withTag builder tk ^String tag-val)
            (string? tag-val)  (.withTag builder tk ^String tag-val)
            (integer? tag-val) (.withTag builder tk ^Integer (int tag-val))
            (boolean? tag-val) (.withTag builder tk ^Boolean tag-val)
            :else              (.withTag builder tk ^String (pr-str tag-val))))))


(defn tag-map [tags-map]
  (reduce-kv (fn [modifier tk tv]
               (comp modifier (tag tk tv)))
             identity tags-map))


(defn context-of
  [span-or-context]
  (cond (instance? Span span-or-context)        (.context span-or-context)
        (instance? SpanContext span-or-context) span-or-context
        (nil? span-or-context)                  nil))


(defn referencing [relationship span-or-context]
  (let [rel-name (name relationship)
        ctxt     (context-of span-or-context)]
    (fn [^Tracer$SpanBuilder builder]
      (.addReference builder rel-name ctxt))))


(defn child-of [parent]
  (referencing References/CHILD_OF
               parent))


(defn follows-from [predecessor]
  (referencing References/FOLLOWS_FROM
               predecessor))


(defn build [span-name & modifiers]
  (.start
    (reduce (fn [sb modifier] (modifier sb))
            (.buildSpan (global-tracer) (name span-name))
            modifiers)))

(definline finish
  [^Span span]
  `(.finish ~span))
