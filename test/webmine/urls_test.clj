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

(deftest test-path
  (are [p u] (= p (path (url u)))
    ""     "http://gooogle.com"
    "/"    "http://google.com/"
    "/foo" "http://google.com/foo"))

(deftest test-host-by-ip
  (is (host-by-ip (url "http://google.com"))))

(deftest test-http?
  (is (http? (url "http://google.com")))
  (is (not (http? (url "https://google.com")))))

(deftest test-urls
  (is (= [(url "http://google.com") (url "http://yahoo.com")]
         (urls ["foobar" "http://google.com" "bizbat" "http://yahoo.com"]))))

(deftest test-url-seq
  (is (= [(url "http://google.com") (url "http://yahoo.com")]
         (url-seq "Yep, http://google.com is better than http://yahoo.com"))))

(deftest test-unique-hosts
  (is (= ["http://www.theamericanscholar.org" "http://www.well.com"]
	 (sort (map str 
		    (unique-hosts [(url "http://bit.ly/bkuH97")

				   (url "http://bit.ly/1Dpk5")])))))
  (is (= ["http://blog.revolutionanalytics.com"
          "http://www.iaventures.com"
          "http://www.readwriteweb.com"]
           (sort
            (map str 
                 (unique-hosts
                  [(url "http://www.iaventurepartners.com")
                   (url "http://blog.revolution-computing.com")
                   (url "http://blog.revolution-computing.com")
                   (url "http://www.readwriteweb.com")
                   (url "http://www.readwriteweb.com")
                   (url "http://toolegit.cn")]))))))

(deftest test-expand-relative-url
  (are [a r] (= a (expand-relative-url "http://foo.com/bar/baz" r))
       "http://foo.com/moo" "/moo"
       "http://foo.com/bar/moo" "moo")
  (are [a r] (= a (expand-relative-url "https://foo.com/bar/baz" r))
       "https://foo.com/moo" "/moo"
       "https://foo.com/bar/moo" "moo"))

(deftest multi-redirect
  (is (= "http://techcrunch.com/2010/01/05/techcrunch-giveaway-a-google-nexus-one-techcrunch/"
         (expand "http://bit.ly/4XzVxm")))
  (is (= "http://io9.com/#!5616394/all-the-books-youll-be-lusting-for-this-fall-season"
         (expand "http://bit.ly/9hkePJ")))
  (is (= "http://www.iaventures.com"
         (expand "http://www.iaventurepartners.com"))))