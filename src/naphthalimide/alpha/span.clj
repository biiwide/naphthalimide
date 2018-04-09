(ns naphthalimide.alpha.span
  (:refer-clojure :exclude [boolean?])
  (:require [naphthalimide.alpha.tracer :refer [global-tracer]])
  (:import  [io.opentracing References Span SpanContext
                            Tracer Tracer$SpanBuilder]
            ))


(definline ^:private boolean?
  [x]
  (or (true? x)
      (false? x)))


(definline ^:private throwable?
  [x]
  `(instance? Throwable ~x))


(defn set-tag!
  "Sets a tag on an existing span."
  [^Span span tag-key tag-val]
  (let [tk (name tag-key)]
    (cond (string? tag-val)  (.setTag span tk ^String tag-val)
          (number? tag-val)  (.setTag span tk ^Number tag-val)
          (boolean? tag-val) (.setTag span tk ^Boolean tag-val)
          (nil? tag-val)     (.setTag span tk ^String tag-val)
          :else              (.setTag span tk ^String (pr-str tag-val))
          )))


(defn- ^java.util.Map ->event-map
  [x]
  (cond (map? x)       (zipmap (map name (keys x))
                               (vals x))
        (string? x)    {"event" x}
        (throwable? x) (let [^Throwable t x]
                         (recur {:error.kind   "Exception"
                                 :error.object t
                                 :event        "error"
                                 :message      (.getMessage t)}))
        ))


(defn log!
  ([^Span span event]
    (.log span (->event-map event)))
  ([^Span span micros event]
    (.log span ^long micros (->event-map event))))

  
(defn tag
  "Add a tag when a span is started.
Example:
  (span/start foo (tag :k \"v\"))"
  [tag-key tag-val]
  (let [tk (name tag-key)]
    (fn [^Tracer$SpanBuilder builder]
      (cond (string? tag-val)  (.withTag builder tk ^String tag-val)
            (number? tag-val)  (.withTag builder tk ^Number tag-val)
            (boolean? tag-val) (.withTag builder tk ^Boolean tag-val)
            (nil? tag-val)     (.withTag builder tk ^String tag-val)
            :else              (.withTag builder tk ^String (pr-str tag-val))))))


(defn- apply-all
  [x fs]
  (reduce (fn [x' f] (f x')) x fs))


(defn tags [& tags-or-map]
  (let [tags-map (if (empty? (rest tags-or-map))
                   (first tags-or-map)
                   (apply hash-map tags-or-map))
        tags     (mapv (fn [[tk tv]] (tag tk tv)) tags-map)]
    (fn [sb]
      (apply-all sb tags))))


(defn- context-of
  [span-or-context]
  (cond (instance? Span span-or-context)
        (.context ^Span span-or-context)

        (instance? SpanContext span-or-context)
        span-or-context

        (nil? span-or-context)
        nil))


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


(defn with-timestamp
  [micros]
  (fn [^Tracer$SpanBuilder sb]
    (.withStartTimestamp sb micros)))


(defn ^Span start
  [span-name & modifiers]
  (.start ^Tracer$SpanBuilder
          (apply-all (.buildSpan (global-tracer)
                                 (name span-name))
                     modifiers)))


(definline finish!
  "Finish a Span.
Can only be performed once.
Subsequent attempts will be ignored."
  [^Span span]
  `(.finish ^Span ~span))


(defn fail-with!
  "Finish a Span and mark it as an error."
  [^Span span ^Throwable exception]
  (log! span exception)
  (set-tag! span :error true)
  (finish! span))
