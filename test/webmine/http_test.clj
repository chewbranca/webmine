(ns webmine.http-test
  (:use [webmine.http :only [strip-punc]]
	clojure.test))

(deftest strip-bad-punc-test
  (is (= "utf-8"
	 (strip-punc "utf-8;"))))