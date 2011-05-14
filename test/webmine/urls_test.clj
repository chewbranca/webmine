(ns webmine.urls-test
  (:use clojure.test
        webmine.urls)
  (:import (java.net URL MalformedURLException)))

(deftest test-url
  (is (instance? URL (url "http://google.com")))
  (is (nil? (url "/foo/bar")))
  (is (nil? (url "foo://bar"))))

(deftest test-host
  (is (= "google.com") (host (url "http://google.com/foo")))
  (is (= "google.com") (host (url "http://Google.com/foo"))))

(deftest test-url-seq
  (is (= [(url "http://google.com") (url "http://yahoo.com")]
         (url-seq "Yep, http://google.com is better than http://yahoo.com"))))