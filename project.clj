(defproject biiwide/naphthalimide "0.0.1"

  :description "Idomatic OpenTracing"

  :url "https://github.com/biiwide/naphthalimide"

  :license {:name "Eclipse Public License 2.0"
            :url "https://www.eclipse.org/legal/epl-2.0"}

  :dependencies [[io.opentracing/opentracing-api "0.31.0"]
                 [io.opentracing/opentracing-util "0.31.0"]
                 [org.clojure/clojure "1.8.0"]
                 [potemkin "0.4.5"]
                 ]

  :global-vars {*warn-on-reflection* true}
  
  :profiles {:dev {:dependencies [[io.opentracing/opentracing-mock "0.31.0"]
                                  [com.uber.jaeger/jaeger-core "0.26.0"]]
                   }}
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]  
  )
