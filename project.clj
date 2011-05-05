(defproject webmine "0.1.5-SNAPSHOT"
  :description "Web data mining library.
               Provides support for mining websites and newsfeeds."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [xerces/xercesImpl "2.9.1"]
                 [clj-sys/tagsoup "1.3-SNAPSHOT"]
                 [clj-serializer "0.1.1"]
                 [html-parse "0.0.1-SNAPSHOT"]
                 [xalan "2.7.1"]
                 [rome "0.9"]
                 [infer "1.0-SNAPSHOT"]
                 [fetcher "0.0.5-SNAPSHOT"]
                 [nlputil-clj "1.0-SNAPSHOT"]
                 [work "1.1.2-SNAPSHOT"]
                 [clj-time "0.2.0-SNAPSHOT"]
                 [clj-sys/plumbing "0.1.5-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  :jvm-opts ["-server"  "-mx1800m" "-Djava.awt.headless=true" "-Dfile.encoding=UTF8"]

  :repositories  {"apache" "https://repository.apache.org/content/repositories/releases/"
                  "snapshots" "http://mvn.getwoven.com/repos/woven-public-snapshots"
                  "releases" "http://mvn.getwoven.com/repos/woven-public-releases"})
