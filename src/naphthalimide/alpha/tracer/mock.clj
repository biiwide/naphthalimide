(ns naphthalimide.alpha.tracer.mock
  (:import (io.opentracing.mock MockSpan
                                MockSpan$MockContext
                                MockSpan$LogEntry
                                MockSpan$Reference
                                MockTracer)))


(defn ^MockTracer tracer
  []
  (MockTracer.))


(defprotocol ToMap
  (to-map [x]))


(extend-type MockSpan$MockContext
  ToMap
  (to-map [^MockSpan$MockContext ctxt]
    (cond-> {:trace-id (.traceId ctxt)
             :span-id  (.spanId ctxt)}
      (seq (.baggageItems ctxt))
      (assoc :baggage  (into {} (.baggageItems ctxt))))))
  

(extend-type MockSpan$LogEntry
  ToMap
  (to-map [^MockSpan$LogEntry entry]
    {:timestamp-micros (.timestampMicros entry)
     :fields           (into {} (.fields entry))}))


(extend-type MockSpan$Reference
  ToMap
  (to-map [^MockSpan$Reference ref]
    {:reference-type (.getReferenceType ref)
     :context        (to-map (.getContext ref))}))


(extend-type MockSpan
  ToMap
  (to-map [^MockSpan span]
    (cond-> {:operation     (.operationName span)
             :context       (to-map (.context span))
             :start-micros  (.startMicros span)
             :finish-micros (.finishMicros span)
             :tags          (let [tags (.tags span)]
                              (zipmap (map keyword (keys tags))
                                      (vals tags)))
             :trace-id      (.traceId (.context span))}

      (not (zero? (.parentId span)))
      (assoc :parent-id (.parentId span))

      (seq (.logEntries span))
      (assoc :log (map to-map (.logEntries span)))

      (seq (.references span))
      (assoc :references (map to-map (.references span)))

      (seq (.generatedErrors span))
      (assoc :generated-errors (seq (.generatedErrors span)))
      )))


(defn finished-spans
  [^MockTracer tracer]
  (mapv to-map
        (.finishedSpans tracer)))

