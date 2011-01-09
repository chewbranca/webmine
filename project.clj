(defproject webmine "0.1.3-SNAPSHOT"
  :description "Web data mining library.
               Provides support for mining websites and newsfeeds."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [xerces/xercesImpl "2.9.1"]
                 [clj-sys/tagsoup "1.3-SNAPSHOT"]
                 [clj-serializer "0.1.1"]
                 [xalan "2.7.1"]
                 [rome "0.9"]
                 [infer "1.0-SNAPSHOT"]
                 [clj-http "0.1.0-SNAPSHOT"]
                 [nlputil-clj "1.0-SNAPSHOT"]
                 [work "0.1.4-SNAPSHOT"]
                 [clj-time "0.2.0-SNAPSHOT"]
                 [clj-sys/plumbing "0.1.3-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
		     [robert/hooke "1.1.0"]
                     [lein-clojars "0.5.0"]]
  :jvm-opts ["-server"  "-mx1800m" "-Djava.awt.headless=true" "-Dfile.encoding=UTF8"]
  :test-selectors {:default (fn [v] (not (:performance v)))
                   :performance :performance
                   :all (constantly true)}
  :repositories  {"apache" "https://repository.apache.org/content/repositories/releases/"})