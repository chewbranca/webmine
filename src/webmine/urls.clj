(ns webmine.urls
  (:require [work.core :as work])
  (:use [plumbing.error :only [maybe-comp]])
  (:import (java.net URL InetAddress
                     MalformedURLException UnknownHostException
                     HttpURLConnection Proxy)))

(defn url
  "Creates a URL. Returns nil if the url specifies and unkown protocol."
  [link] 
  (if (instance? java.net.URL link) link
      (try (java.net.URL. link)
           (catch MalformedURLException _ nil))))

(defn host
  "Return the lowercased hostname for the given URL, if applicable."
   [#^URL u]
  (if-let [h (.getHost u)]
    (.toLowerCase h)))

;; TODO: Remove http:// conversion, what about https?
(defn host-url [u]
 ((maybe-comp
   url
   #(str "http://" %)
   host
   url)
   u))

; jacked from nutch: Regex patern to get URLs within a plain text.
; http://www.truerwords.net/articles/ut/urlactivation.html
; URLs from plain text using Regular Expressions.
; http://wiki.java.net/bin/view/Javapedia/RegularExpressions
; http://regex.info/java.html @author Stephan Strittmatter - http://www.sybit.de
(def url-pattern
  #"([A-Za-z][A-Za-z0-9+.-]{1,120}:[A-Za-z0-9/](([A-Za-z0-9$_.+!*,;/?:@&~=-])|%[A-Fa-f0-9]{2}){1,333}(#([a-zA-Z0-9][a-zA-Z0-9$_.+!*,;/?:@&~=%-]{0,1000}))?)")

(defn url-seq [^String t]
  (->>
   (re-seq url-pattern t)
   (map (comp url #(.trim ^String %) first))
   (remove nil?)))