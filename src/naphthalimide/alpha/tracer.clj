(ns naphthalimide.alpha.tracer
  (:import  [io.opentracing Tracer]
            [io.opentracing.util GlobalTracer]
            ))

(def ^:dynamic *tracer* nil)


(definline register-global-tracer!
  "Registers a tracer as the default global tracer."
  [tracer]
  `(GlobalTracer/register ~tracer))


(defn ^io.opentracing.Tracer global-tracer
  "Returns the active global tracer.
* Can be overridden by using with-tracer."
  []
  (or *tracer* (GlobalTracer/get)))


(defmacro with-tracer
  "Temporarily overrides the global-tracer."
  [tracer & body]
  `(do (assert (instance? Tracer ~tracer)
               ~(format "%s must be an instance of io.opentracing Tracer!" (pr-str tracer)))
       (binding [*tracer* tracer]
         ~@body)))
