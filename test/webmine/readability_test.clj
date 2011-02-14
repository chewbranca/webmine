(ns webmine.readability-test  
  (:require [clj-http.client :as http])
  (:use clojure.test webmine.readability webmine.parser))

(deftest format-plain-text-content-test
  (is (= (-> "Hi.<p>New paragraph" dom format-plain-text-content)
         "Hi.\n\nNew paragraph")))

(def ^:private test-urls
     [   "http://techcrunch.com/2010/10/22/stripon/" ; gets an extra bit at the end wrong, but basically good 

"http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/"
       "http://www.huffingtonpost.com/arianna-huffington/post_1098_b_770178.html"
       "http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"
       "http://www.freekareem.org/2010/11/16/kareem-amer-is-free/"
  ; parser.clj doesn't clear the scripts here
  "http://io9.com/5671733/air-force-academy-now-welcomes-spell+casters"
  ;; Mostly Work 
  ; Get content but lots of other stuff in content DIV
  "http://lifehacker.com/5671690/this-weeks-top-downloads?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+lifehacker/full+(Lifehacker)"

  ;; DOESNT WORK - no <p> tags !
  "http://gardening.about.com/od/growingtips/tp/Tomato_Tips.htm"

"http://channel9.msdn.com/posts/DC2010T0100-Keynote-Rx-curing-your-asynchronous-programming-blues"
])
      
(deftest  ^{:performance true}  performance-test
  (let [test-html (doall (map slurp test-urls))]
    (time (doall (map extract-content (take 500 (cycle test-html)))))))      

(deftest words
  (is (= 2 (count
	    (re-seq word-re "foo bar")))))

(defn body-words [url]
  (count (re-seq word-re
		 (clean-text
		  (readability-div
		   (webmine.parser/dom
		    (:body (http/get url))))))))

(deftest best-dev-wordcount
  (is (= 1126
	 (body-words "http://www.latimes.com/news/nationworld/world/la-fg-muslim-brotherhood-20110131,0,5283199.story")))
  (is (= 419
	 (body-words "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/")))
  (is (= 1106
	 (body-words "http://www.huffingtonpost.com/arianna-huffington/post_1098_b_770178.html")))
  (is (= 959
	 (body-words "http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"))))