(ns naphthalimide.alpha.tracer.mock
  (:import (io.opentracing.mock MockSpan
                                MockSpan$MockContext
                                MockSpan$LogEntry
                                MockSpan$Reference
                                MockTracer)))


(defn ^MockTracer tracer
  []
  (MockTracer.))


(defn- context->
  [^MockSpan$MockContext ctxt]
  (cond-> {:trace-id (.traceId ctxt)
           :span-id  (.spanId ctxt)}
    (not (empty? (.baggageItems ctxt)))
    (assoc :baggage  (into {} (.baggageItems ctxt)))))
  

(defn- log-entry->
  [^MockSpan$LogEntry entry]
  {:timestamp-micros (.timestampMicros entry)
   :fields           (into {} (.fields entry))})


(defn- reference->
  [^MockSpan$Reference ref]
  {:reference-type (.getReferenceType ref)
   :context        (context-> (.getContext ref))})


(defn- mock-span->
  [^MockSpan span]
  (cond-> {:operation     (.operationName span)
           :context       (context-> (.context span))
           :start-micros  (.startMicros span)
           :finish-micros (.finishMicros span)
           :tags          (let [tags (.tags span)]
                            (zipmap (map keyword (keys tags))
                                    (vals tags)))
           :trace-id      (.traceId (.context span))}

    (not (zero? (.parentId span)))
    (assoc :parent-id (.parentId span))

    (not (empty? (.logEntries span)))
    (assoc :log       (map log-entry->
                           (.logEntries span)))

    (not (empty? (.references span)))
    (assoc :references    (map reference->
                               (.references span)))

    (not (empty? (.generatedErrors span)))
    (assoc :generated-errors (.generatedErrors span))
    ))


(defn finished-spans
  [^MockTracer tracer]
  (mapv mock-span->
        (.finishedSpans tracer)))


