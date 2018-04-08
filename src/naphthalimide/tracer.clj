(ns naphthalimide.tracer
  (:import  [io.opentracing.util GlobalTracer]
            ))


(definline register-global-tracer!
  [tracer]
  `(GlobalTracer/register ~tracer))


(definline global-tracer []
  `(GlobalTracer/get))

