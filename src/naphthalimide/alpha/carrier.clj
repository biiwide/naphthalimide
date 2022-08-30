(ns naphthalimide.alpha.carrier
  (:require [naphthalimide.alpha.span :as span]
            [naphthalimide.alpha.tracer :as tracer])
  (:import  (io.opentracing Span SpanContext Tracer)
            (io.opentracing.propagation Format Format$Builtin)
            (java.nio ByteBuffer)
            ))


(defonce FORMATS
  (atom {}))


(defn register-carrier-format!
  ([carrier-format-name format new-carrier]
    (register-carrier-format!
      carrier-format-name format new-carrier identity))
  ([carrier-format-name format new-carrier carrier-final]
    (assert (instance? Format format)
            (format "Format '%s' must implement io.opentracing.propagation.Format"
                    (str format)))
    (swap! FORMATS assoc carrier-format-name
           {:format format
            :init   new-carrier
            :final  carrier-final})
    carrier-format-name))


(defn carrier-format
  [carrier-format-name]
  (@FORMATS carrier-format-name))

(defn ^io.opentracing.propagation.TextMap fresh-text-map
  ([]
    (fresh-text-map {}))
  ([init]
    (let [state (atom
                  (zipmap
                    (map name (keys init))
                    (map str (vals init))))]
      (reify clojure.lang.IDeref
             (deref [_] @state)
             io.opentracing.propagation.TextMap
             (iterator [_]
               (.iterator ^clojure.lang.IPersistentMap (deref state)))
             (put [_ k v]
               (swap! state assoc k v))))))

(defn fresh-byte-buffer
  ([] (ByteBuffer/allocate 0))
  ([bytes] (ByteBuffer/wrap bytes)))


(register-carrier-format! :binary
                          Format$Builtin/BINARY
                          fresh-byte-buffer)

(register-carrier-format! :text-map
                          Format$Builtin/TEXT_MAP
                          fresh-text-map
                          deref)


(defn injectable-context
  ([carrier-format-name]
    (injectable-context carrier-format-name
                        (tracer/global-tracer)))
  ([carrier-format-name tracer]
    (injectable-context carrier-format-name
                        tracer
                        (span/active tracer)))
  ([carrier-format-name ^Tracer tracer ^Span span]
    (when-some [{:keys [format init final]} (carrier-format carrier-format-name)]
      (let [carrier (init)]
        (.inject tracer (span/context span) format carrier)
        (final carrier)))))


(defn ^SpanContext extract-context
  ([carrier-format-name carrier]
    (extract-context carrier-format-name carrier (tracer/global-tracer)))
  ([carrier-format-name carrier ^Tracer tracer]
    (when-some [{:keys [format init]} (carrier-format carrier-format-name)]
      (.extract tracer format (init carrier)))))

