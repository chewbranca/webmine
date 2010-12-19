(ns webmine.readability-test
  (:use clojure.test webmine.readability webmine.parser))

(deftest format-plain-text-content-test
  (is (= (-> "Hi.<p>New paragraph" dom format-plain-text-content)
         "Hi.\n\nNew paragraph")))