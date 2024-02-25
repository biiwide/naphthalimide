(ns naphthalimide.alpha.span
  (:require [naphthalimide.alpha.tracer
             :refer [activate global-tracer]]
            [naphthalimide.internal
             :refer [definline*]])
  (:import  (io.opentracing References Scope ScopeManager
                            Span SpanContext
                            Tracer Tracer$SpanBuilder)
            (java.util Map)))


(definline ^:private throwable?
  [x]
  `(instance? Throwable ~x))


(def ^:dynamic *sequence-length* 3)


(defn ^String to-string
  [x]
  (binding [*print-length* *sequence-length*]
    (pr-str x)))


(defprotocol HasActiveSpan
  (^io.opentracing.Span -active-span [source]))


(extend-protocol HasActiveSpan
  Span
  (-active-span [span] span)
  Scope
  (-active-span [_scope]
    (throw (IllegalArgumentException. "OpenTracing no longer support returning the active Span from a Scope.")))
  ScopeManager
  (-active-span [^ScopeManager scope-mgr]
    (-active-span (.activeSpan scope-mgr)))
  Tracer
  (-active-span [^Tracer tracer]
    (.activeSpan tracer))
  nil
  (-active-span [_] nil))


(defprotocol HasContext
  (^io.opentracing.SpanContext context [x] "Return a span context"))
  

(extend-protocol HasContext
  Span
  (context [^Span span] (.context span))
  SpanContext
  (context [span-ctxt] span-ctxt)
  nil
  (context [_] nil))


(definline* ^Span active
  ([]
    `(-active-span (global-tracer)))
  ([source]
    `(-active-span ~source)))


(defprotocol TagValue
  (-span-set-tag [value key span])
  (-builder-with-tag [value key span-builder]))


(extend-protocol TagValue
  Boolean
  (-span-set-tag [val key span]
    (.setTag ^Span span ^String key ^Boolean val))
  (-builder-with-tag [val key builder]
    (.withTag ^Tracer$SpanBuilder builder ^String key ^Boolean val))

  Number
  (-span-set-tag [value key span]
    (.setTag ^Span span ^String key ^Number value))
  (-builder-with-tag [val key builder]
    (.withTag ^Tracer$SpanBuilder builder ^String key ^Number val))

  String
  (-span-set-tag [value key span]
    (.setTag ^Span span ^String key ^String value))
  (-builder-with-tag [val key builder]
    (.withTag ^Tracer$SpanBuilder builder ^String key ^String val))

  Object
  (-span-set-tag [value key span]
    (-span-set-tag (to-string value) key span))
  (-builder-with-tag [val key builder]
    (-builder-with-tag (to-string val) key builder))

  nil
  (-span-set-tag [value key span]
    (.setTag ^Span span ^String key ^String value))
  (-builder-with-tag [val key builder]
    (.withTag ^Tracer$SpanBuilder builder ^String key ^String val)))


(defn set-tag!
  "Sets a tag on an existing span."
  [^Span span tag-key tag-val]
  (-span-set-tag tag-val (name tag-key) span)
  span)


(defn ^java.util.Map event
  "Construct an event suitable for log!"
  [x]
  (cond (map? x)       (zipmap (map name (keys x))
                               (vals x))
        (string? x)    {"event" x}
        (throwable? x) (let [^Throwable t x]
                         (recur {:error.kind   "Exception"
                                 :error.object t
                                 :event        "error"
                                 :message      (.getMessage t)}))))


(defn ^Span log!
  "Log data to a span"
  ([^Span span map-or-message]
   (.log span ^Map (event map-or-message)))
  ([^Span span ^long epoch-micros map-or-message]
   (.log span epoch-micros ^Map (event map-or-message))))


(defn set-baggage-item!
  [^Span span k v]
  (.setBaggageItem span (name k) (str v)))


(defn with-tag
  "Add a tag when a span is started.
Example:
  (span/start foo (tag :k \"v\"))"
  [tag-key tag-val]
  (partial -builder-with-tag tag-val (name tag-key)))


(defn- apply-all
  [x fs]
  (reduce (fn [x' f] (f x')) x fs))


(defn with-tags [& tags-or-map]
  (let [tags-map (if (empty? (rest tags-or-map))
                   (first tags-or-map)
                   (apply hash-map tags-or-map))
        tags     (mapv (fn [[tk tv]] (with-tag tk tv)) tags-map)]
    (fn [sb]
      (apply-all sb tags))))


(defn with-reference [relationship span-or-context]
  (if-some [ctxt (context span-or-context)]
    (fn [^Tracer$SpanBuilder builder]
      (.addReference builder (name relationship) ctxt))
    identity))


(defn with-parent [parent]
  (with-reference References/CHILD_OF
                  parent))


(defn with-predecessor [predecessor]
  (with-reference References/FOLLOWS_FROM
                  predecessor))


(defn with-timestamp
  [micros]
  (fn [^Tracer$SpanBuilder sb]
    (.withStartTimestamp sb micros)))


(defn ^Span fresh
  ([span-name modifiers]
    (fresh (global-tracer) span-name modifiers))
  ([^Tracer tracer span-name modifiers]
    (.start ^Tracer$SpanBuilder
            (apply-all (.buildSpan tracer
                                   (name span-name))
                       (cons (with-parent (active tracer))
                             modifiers)))))


(defmacro ^io.opentracing.Span start
  [span-name & modifiers]
  `(fresh (global-tracer)
          ~span-name
          ~(vec modifiers)))


(defn ^io.opentracing.Span finish!
  [^Span span]
  (doto span
    (.finish)))


(defn ^io.opentracing.Span fail-with!
  "Finish a Span and mark it as an error."
  [^Span span ^Throwable exception]
  (doto span
    (log! exception)
    (set-tag! :error true)))


(defmacro within-scope
  [span & body]
  (let [form-meta (zipmap (map (comp (partial str "source.") name)
                               (keys (meta &form)))
                          (vals (meta &form)))
        ns-meta   {"source.ns"   (name (ns-name *ns*))
                   "source.file" *file*}
        source-meta (merge form-meta ns-meta)]
    `(let [^Span span# (reduce-kv set-tag! ~span ~source-meta)]
       (try
         (with-open [^Scope scope# (activate span#)]
           ~@body)
         (catch Throwable t#
           (fail-with! span# t#)
           (throw t#))
         (finally
           (.finish span#))))))
