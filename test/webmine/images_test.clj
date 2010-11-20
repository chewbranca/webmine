(ns webmine.images-test
 (:use clojure.test
       webmine.images))

(deftest extract-hw-test
  (let [s "height: 500px; width: 376px;"
	t "width: 376px; height: 500px;"]
    (is (= {:height 500 :width 376}
	   (hw-from-str s)))
    (is (= {:height 500 :width 376}
	   (hw-from-str t)))))

(deftest find-best-img
  (is (= {:url "https://docs.google.com/File?id=dhhw653p_848g39gd9gx_b",
	  :size {:width 640, :height 480}}
	  (best-img-at
"http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"))))

;;gets me:
;;http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html

;;image with no size tags, also has later image in core body that is slightly larger, we should get the top image.
;;http://techcrunch.com/2010/10/22/stripon/

;;rolling back to all images when there are none in the body.  image is also relative path to host.
;;http://daringfireball.net/2010/10/apple_no_longer_bundling_flash_with_mac_os_x

;;trick outer div with bigger image for promotion.
;;http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/
;;http://gigaom.com/2010/10/23/latest-smartphones-reviewed-t-mobile-g2-nokia-n8/

