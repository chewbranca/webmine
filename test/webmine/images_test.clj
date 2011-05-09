(ns webmine.images-test
 (:use clojure.test
       webmine.images
       html-parse.parser
       fetcher.core
       [clojure.contrib.profile :only [profile]]))

(deftest extract-hw-test
  (let [s "height: 500px; width: 376px;"
	t "width: 376px; height: 500px;"]
    (is (= {:height 500 :width 376}
	   (hw-from-str s)))
    (is (= {:height 500 :width 376}
	   (hw-from-str t)))))

(deftest expand-relative-paths
  (let [url "http://vator.tv/news/2011-04-11-what-you-need-to-know-04-11-11"
	d (dom (:body (fetch :get url)))
	_ (expand-relative-imgs url d)
	srcs (filter #(.startsWith % "/")
		     (map (comp :src attr-map) (elements d "img")))]
    (is (= []  srcs))))