(ns webmine.http
  "HTTP client for webmine."
  (:refer-clojure :exclude [get])
  (:require [clj-http.core :as clj-http]
            clj-http.client)
  (:use [plumbing.core :only [-?>]]
        [clojure.string :only [split trim]]
        [webmine.parser :only [charset dom]])
  (:import (org.apache.commons.io IOUtils)))

(defn wrap-html-body
  [client]
  (fn [req]
    (let [{:keys [headers body] :as resp} (client req)]
      (if (.startsWith (headers "content-type") "text/html")
        (let [b (IOUtils/toByteArray body)
              charset (or (-?> (headers "content-type")
                               (split #"=")
                               second
                               trim)
                          (charset (dom (String. b "UTF-8")))
                          "UTF-8")]
          (assoc resp :body (String. b charset)))
        resp))))

(defn wrap-request
  [request]
  (-> request
      (clj-http.client/wrap-client (clj-http/pooled-http-client))
      clj-http.client/wrap-redirects
      clj-http.client/wrap-exceptions
      clj-http.client/wrap-decompression
      clj-http.client/wrap-query-params
      clj-http.client/wrap-basic-auth
      clj-http.client/wrap-accept
      clj-http.client/wrap-accept-encoding
      clj-http.client/wrap-content-type
      clj-http.client/wrap-method
      clj-http.client/wrap-url
      wrap-html-body))

(def request (wrap-request clj-http/request))

(defn get
  "Like #'request, but sets the :method and :url as appropriate."
  [url & [req]]
  (request (merge req {:method :get
                       :url url})))

;; From l.fetcher
(defn body-str [u]
  (try (:body (get (str u)))
       (catch java.lang.Exception _ nil)))

(defn header-str [u]
  (try (:body (get (str u)))
       (catch java.lang.Exception _ nil)))