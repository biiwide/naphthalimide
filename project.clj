(defproject biiwide/naphthalimide "0.0.6-SNAPSHOT"

  :description "Idomatic OpenTracing"

  :url "https://github.com/biiwide/naphthalimide"

  :license {:name "Eclipse Public License 2.0"
            :url "https://www.eclipse.org/legal/epl-2.0"}

  :plugins [[lein-ancient "1.0.0-RC3"]
            [lein-cloverage "1.2.4"]
            [lein-file-replace "0.1.0"]]

  :dependencies [[io.opentracing/opentracing-api "0.33.0"]
                 [io.opentracing/opentracing-util "0.33.0"]
                 [org.clojure/clojure "1.10.3" :scope "provided"]
                 [potemkin "0.4.7"]]

  :global-vars {*warn-on-reflection* true}
  
  :aliases {"kondo"
            ["with-profile" "clj-kondo" "run" "-m" "clj-kondo.main" "--lint" "src:test" "--copy-configs"]}

  :profiles {:dev {:dependencies [[com.uber.jaeger/jaeger-core "0.27.0"]
                                  [io.opentracing/opentracing-mock "0.33.0"]
                                  [nubank/matcher-combinators "3.9.1"]
                                  [cloverage "1.2.4"]]}
             :clj-kondo {:dependencies ^:replace [[clj-kondo "2024.02.12"]]}}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["file-replace" "README.md"
                   "\\[biiwide/naphthalimide \"" "\"]"
                   "version"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  )
