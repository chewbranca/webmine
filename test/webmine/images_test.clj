(ns webmine.images-test
 (:use clojure.test
       webmine.images
       [clojure.contrib.profile :only [profile]]))

(deftest extract-hw-test
  (let [s "height: 500px; width: 376px;"
	t "width: 376px; height: 500px;"]
    (is (= {:height 500 :width 376}
	   (hw-from-str s)))
    (is (= {:height 500 :width 376}
	   (hw-from-str t)))))

(deftest at-leat-test
  (is (= [{:size {:height 150 :width 160}}]
	 (at-least 10000
	    [{:size {:height 150 :width 160}}
	    {:size {:height 50 :width 60}}]))))

(deftest find-best-img
  (is (= {:url "https://docs.google.com/File?id=dhhw653p_848g39gd9gx_b",
	  :size {:width 640, :height 480}
	  :content-size 86182}
	  (best-img-at
"http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html")))
  (is (= nil (best-img-at "http://alexisohanian.com/fantastic-advice-on-why-its-better-to-be-hard" 10000))))

(def ^:private test-urls
     [ "http://www.huffingtonpost.com/arianna-huffington/post_1098_b_770178.html"
       "http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"
       "http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html"
       "http://www.freekareem.org/2010/11/16/kareem-amer-is-free/"
       "http://lifehacker.com/5671690/this-weeks-top-downloads?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+lifehacker/full+(Lifehacker)"
       "http://gardening.about.com/od/growingtips/tp/Tomato_Tips.htm"
       "http://channel9.msdn.com/posts/DC2010T0100-Keynote-Rx-curing-your-asynchronous-programming-blues"])

(deftest ^{:performance true}
  best-img-performance-test
  (profile (time (doall (map best-img-at test-urls)))))


;;image with no size tags, also has later image in core body that is slightly larger, we should get the top image.
(deftest img-without-size-tag
 (is (= "http://tctechcrunch.files.wordpress.com/2010/10/screen-shot-2010-10-22-at-9-55-42-am.png"
	(:url (best-img-at "http://techcrunch.com/2010/10/22/stripon/" 1000)))))

(deftest none-in-body
 (is (= nil
	(:url (best-img-at "http://daringfireball.net/2010/10/apple_no_longer_bundling_flash_with_mac_os_x" 1000)))))

(deftest trick-outer-div-w-promo
 (is (= "http://gigaom2.files.wordpress.com/2010/10/devicefidelity-nfc-microsd-card.jpeg?w=185&h=140"
	(:url (best-img-at "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/" 1000)))))

(deftest fall-back-to-nothing
 (is (= nil
	(best-img-at "http://www.newyorker.com/humor/2011/01/24/110124sh_shouts_allen" 1000))))