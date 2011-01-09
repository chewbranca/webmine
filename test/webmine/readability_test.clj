(ns webmine.readability-test  
  (:use clojure.test webmine.readability webmine.parser)
  (:use [clojure.contrib.profile :only [profile]]))

(deftest format-plain-text-content-test
  (is (= (-> "Hi.<p>New paragraph" dom format-plain-text-content)
         "Hi.\n\nNew paragraph")))

(def ^:private test-urls
     [ "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/"
       "http://www.huffingtonpost.com/arianna-huffington/post_1098_b_770178.html"
       "http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"
       "http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html"
       "http://www.freekareem.org/2010/11/16/kareem-amer-is-free/"
       "http://lifehacker.com/5671690/this-weeks-top-downloads?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+lifehacker/full+(Lifehacker)"
       "http://gardening.about.com/od/growingtips/tp/Tomato_Tips.htm"
       "http://channel9.msdn.com/posts/DC2010T0100-Keynote-Rx-curing-your-asynchronous-programming-blues"])
      

(deftest  ^{:performance true}  performance-test
  (let [test-html (doall (map slurp test-urls))]
    (time (doall (map extract-content (take 500 (cycle test-html)))))))      