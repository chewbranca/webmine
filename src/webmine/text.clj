(ns webmine.text
  (:use [clojure.contrib.singleton :only [per-thread-singleton]])
  (:import [opennlp.tools.lang.english SentenceDetector])
  (:require [clojure.contrib.duck-streams :as ds]))

(defn- resource-to-temp-file   
  [resource-name file-ext]
  (let [temp-file (doto (java.io.File/createTempFile "webmine" file-ext) .deleteOnExit)]
    (ds/copy 
        (ClassLoader/getSystemResourceAsStream resource-name) 
	temp-file)
    temp-file))

(def open-nlp-sent-split
     (per-thread-singleton
      #(-> "opennlp_models/eng-sent-seg.bin.gz"
	   (resource-to-temp-file ".bin.gz")
	   (.getAbsolutePath)
	   (SentenceDetector.))))

(defn sent-intervals 
  "return vec of [start,stop] indices"
  [txt]
  (partition 2 1 
	     (concat [0] (.sentPosDetect (open-nlp-sent-split) txt) [(.length txt)])))

(defn sent-split
  [txt]
  (map
   (fn [[start stop]]
     (.substring txt start stop))
   (sent-intervals txt)))
