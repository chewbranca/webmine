(ns webmine.feeds
  (:use clojure.xml
        clojure.set
        clojure.contrib.java-utils
        webmine.core
        webmine.readability
        webmine.parser
        webmine.urls
        [clojure.java.io :only [input-stream]])
  (:require [work.core :as work]
            [clojure.zip :as zip]
            [webmine.images :as imgs]
            [clojure.contrib.logging :as log]
            [clojure.contrib.zip-filter :as zip-filter]
            [clojure.contrib.zip-filter.xml :as xml-zip]
            [clj-http.client :as http]
            [clj-time.format :as time-fmt]
            [clj-time.coerce :as time-coerce])  
  (:import [com.sun.syndication.feed.synd
            SyndFeedImpl SyndEntryImpl SyndContentImpl]
	   [org.joda.time.format DateTimeFormat]
           [com.sun.syndication.io
            SyndFeedInput SyndFeedOutput XmlReader]
           java.util.Date
           java.util.ArrayList
           java.io.InputStream
           [java.text
            SimpleDateFormat ParsePosition]))

(def rfc822-rss-formats
     (map #(SimpleDateFormat. %)
	  ["E, dd MMM yy HH:mm:ss Z"
	   "E, dd MMM yyyy HH:mm:ss Z"
	   "E, dd MMM yyyy HH:mm:ss ZZ"
	   "E, dd MMM yyyy HH:mm:ss z"
	   "E dd MMM yyyy HH:mm:ss z"
	   "MMM, dd yyyy HH:mm:ss z"
	   "dd MMM yyyy HH:mm:ss z"
	   "E MMM dd HH:mm:ss z yyyy"
	   "E, dd MMM yyyy HH:mm ZZ"]))

(defn compact-date-time
  "take date time string and parse using RSS/Atom
   formats and falling back on clj-time.formats if necessary"
  [#^String s]
  (first
   (concat
    (for [#^SimpleDateFormat sdf rfc822-rss-formats
	  :let [d (try
		    (.parse sdf (.trim s) (ParsePosition. 0))
		    (catch Exception _ nil))]
	  :when d]
      (-> d time-coerce/from-date time-coerce/to-string))
    (for [fmt (vals time-fmt/formatters)
	  :let [d (try
		    (time-fmt/parse fmt s)
		    (catch Exception _ nil))]
	  :when d] (-> d time-coerce/to-string)))))

;;TODO better way?
(defn to-char [s] (.charAt s 0))

(defn period? [c] (= (to-char ".") c))

(defn count-sentences [s]
  (count (filter period? s)))

(defn- sentences-from-ptags
  ([ps k]
     (sentences-from-ptags ps k ""))
  ([ps k text]
     (let [t (str text (text-from-dom (first ps)))]
       (if (or (>= (count-sentences t)
		   k)
	       (empty? (rest ps)))
	 t
	 (recur (rest ps) k t)))))

;;TODO: just extract text first and expand by periods, or continue with p-tag based approach?
(defn first-k-sentences [body k]
  (let [d (dom body)
	ps (elements d "p")]
    (sentences-from-ptags ps k)))

(defn mk-des [entry]
  (let [min-sentences 3
	d (:des entry)
	c (:content entry)]
    (if (and d
	     (not (= d c))
	     (>= (count-sentences (text-from-dom (dom d)))
		 min-sentences))
      entry
      (assoc entry :des (first-k-sentences c min-sentences)))))

(defrecord Feed [title des link entries])

(defrecord FeedEntry [title link content des date author])

(defn- item-node-to-entry [item]
  (let [item-root (zip/xml-zip item)
        get-text (fn [k] (xml-zip/xml1-> item-root k xml-zip/text))
        entry (FeedEntry.
               ;; title
               (get-text :title)
               ;; link
               (get-text :link)
               ;; content
               (apply max-key count
                      (map get-text [:content :description :content:encoded]))
               ;; des
               (first (filter identity
                              (map get-text [:description :content :content:encoded])))
               ;; date
               (try (first (for [k [:pubDate :date :updatedDate :dc:date]
                                 :let [s (get-text k)]
                                 :when s] (compact-date-time s)))
                    (catch Exception e (log/error e)))
               ;; author
               (get-text :author))]
    (try (mk-des entry)
         (catch Exception _
           entry))))

(defn parse-feed [source]
  "returns record Feed representing a snapshot of a feed. Supports keys
  :title Name of feed
  :des Description of feed
  :link link to feed
  :entries seq of Entry records, see doc below for entries"
  (try
    (when-let [root (-> source parse zip/xml-zip)]
      (Feed.
         ; title
         (xml-zip/xml1-> root :channel :title xml-zip/text)
	 ; desc (cription)
         (xml-zip/xml1-> root :channel :description xml-zip/text)
	 ; link
	 (xml-zip/xml1-> root :channel :link xml-zip/text)
	 ; entries
	 (doall
	   (for [n (xml-zip/xml-> root :channel :item zip/node)
		 :let [entry (into {}
				 (filter second
					 (item-node-to-entry n)))]]
	   entry))))
    (catch Exception _  (do (println (format "ERROR: Couldn't parse %s, returning nil" source))
			     nil))))


(defn with-images [es]
  (map #(imgs/with-best-img % :link :content) 
       es))

(defn ensure-entries
  "Takes a seq of entires.  if any are missing :content, fetch the entry and extract the best div using readability."
  [es]
  (map
   (fn [e]
     (if (:content e) e
	 (assoc e :content
	 (best-body (:body (http/get (:link e)))))))
   es))

(defn entries [source]
  "
  takes a string or inputstream
  return seq of entries from rss feed source.
  Each entry is a map with string values
  :title entry title
  :des  descritpion
  :date String of date (uses to-string canonical
   rep in clj-time.coerce)
  :author author string
  :content Content of entry (or :description if not content)
  :link Link to content. "
  (try
    (let [res (->  source
		   parse-feed
		   :entries
		   with-images
		   ensure-entries)]
      ;;turn records into maps
      (map (partial into {}) res))
    (catch Exception e
      (log/error (format "Error processing source %s" source))
      (.printStackTrace e)
      nil)))

(defn fetch-entries [u]
  (-> u url input-stream entries))

(defn extract-entries [body]
  (-> body (.getBytes "UTF-8")
	       input-stream
	       entries))

;;TODO: duplicaiton from some stuff in images.  Should probably be pulling into readability.
(defn best-body [content]
  (let [d (dom content)
	;;first try to get the text out of the core body div.
	core-text (text-from-dom (readability-div d))
	;;if that we have core images, use those, if not, get all the images in the dom
	best (if (and core-text
			     (> (count core-text)
					  0))
		      core-text
		      (text-from-dom d))]
    best))

(defn feed? [item]
  (and item
       (let [feed (parse-feed item)]
	 (-> feed :entries empty? not))))

(defn links-from-entry [e]
 (-> e :content url-seq))

(defn map-to-entry [entry]
  (let [content-keys (difference
		      (into #{} (keys entry))
		      #{:title :link :author :date :des})
	content (map (fn [k]
		       (let [v (entry k)]
			 [k (.setValue (SyndContentImpl.) v)]))
		     content-keys)]
  (doto (SyndEntryImpl.)
    (.setTitle (:title entry))
    (.setLink (:link entry))
    (.setAuthor (:author entry))
    (.setPublishedDate (:date entry))
    (.setDescription
     (doto (SyndContentImpl.)
       (.setType "text/html")
       (.setValue  (:des entry)))))))

(defn feed-home [source]
 (if-let [synd-feed (parse-feed source)]
    (:link synd-feed)))

(defn external?
  "is the url from the same host as the home url?"
  [home other]
  (and home other
       (not (= (.getHost other)
	       (.getHost home)))))

(def internal? (complement external?))

(defn external
  "removes urls to the same host"
  [home urls]
  (filter #(external? (url home) %)
	  urls))

(defn external-feed? [home other]
  (and (external? home other)
       (feed? other)))

(defn internal-feed? [home other]
  (and (internal? home other)
       (feed? other)))

;;TODO: refactor to a sinlge api - url, string url, etc.
(defn find-outlinks
"string (page body) -> url (blog homepage) -> outlinks"
[s h]
  (seq (into #{}
	     (work/filter-work
	      #(external? (url h) %)
	      (url-seq s)
	      20))))

(defn comment? [u]
(.contains (str u) "comments"))

;;TODO: invert to make mroe efficient with and.
;;also could invert conditional to be only .xml, /, or nothing
(defn rss-suffix? [u]
  (let [u (str u)]
    (and (or (.contains u "xml")
	     (.contains u "rss")
	     (.contains u "atom")
	     #_(.matches u "^.*/[^/.]*$"))
	 (not (.endsWith u "xmlrpc.php")))))
;; (let [su (str u)
;; 	l (.length su)
;; 	drop (= "/" (.charAt su (- l 1)))
;; 	u* (if (not drop) su (subs su 0 (- l 1)))]
;; 	(not (or (.endsWith u* ".com")
;; 	    (.endsWith u* ".html")
;; 	    (.endsWith u* ".php")
;; 	    (.endsWith u* ".png")
;; 	    (.endsWith u* ".ico")
;; 	    (.endsWith u* ".txt")
;; 	    (.endsWith u* ".txt"))))

(defn- fix-link
  "fix absolute links"
  [base #^String link]
  (if (and link (.startsWith link "/"))
    (str base link)
    link))

(defn host-rss-feeds
  "checks head of page for rss feed links. Does two checks, any links
   that are marked as rss/xml/atom in the link type or if
   any link has rss xml or  "
  [page]
  (let [d (-> page body-str dom)]    
    (into #{}	  
    (filter identity #_(comp feed? url)
	    (concat
	     ;; sometimes rss feeds are marked as
	     ;; <link type="application/rss+xml">
	     (when-let [all-links (elements d "link")]
	       (for [l all-links
		     :when l
		     :let [attr (attr-map l)
			   #^String type (:type attr)
			   #^String link (->> attr :href (fix-link (str page)))]
		     :when (and link
			        type
				(or (.contains type "rss") (.contains type "atom"))) ]
		 link))
	     ;;most sites go with the standard that the rss or atom feed is in the head
	     (when-let [head-links (-> d head links-from-dom)]
	       (->> head-links
		    (map (partial fix-link (str page)))
		    (filter #(and (not (comment? %)) (rss-suffix? %))))))))))

(def canonical-feed (comp min-length host-rss-feeds))

(defn canonical-feeds
"
Avoid subscribing to multiple feeds on the same blog.
Initial heuristic is to take url with min length.
May not be a good idea for blogs that have many useful feeds, for example, for a news site like huffington post."
[urls]
(seq (into #{} (work/map-work canonical-feed urls 20))))

;;TODO: parallelize
(defn blogroll [opml]
  (for [x (xml-seq (parse opml))
        :when (= :outline (:tag x))
	:let [a (:attrs x)
	      u (or (:xmlUrl a) (:htmlUrl a))]
	:when (url u)]
    u))

(defn find-feed-outlinks
  "given the url of a blog's homepage, find the outlinks to feeds from the homepage."
  [b u]
  (let [outs (into #{}
		   (find-outlinks b u))
	feeds (filter
	       identity
	       (canonical-feeds outs))
	;;we need to filter same host feeds again, as they can get filtered from outlinsk but then be found again when extracting canonical feeds.
	ex-feeds (into #{} (filter #(external? (url u) (url %))
				   feeds))]
    (seq ex-feeds)))

(defn home-feed-outlinks
  [u]
  (find-feed-outlinks (body-str u) u))

(defn entry-feed-outlinks
  "given the url of a blog's feed, find the outlinks to feeds from all the entries currently in this blog's feed."
  [u]
  (let [home (feed-home (url u))
	uber-b (apply str (fetch-entries u))]
    (find-feed-outlinks uber-b u)))

(defn feed-outlinks
  "given the url of a blog's homepage or rss feed, find the outlinks to feeds from both the homepage, and all the entries currently in this blog's feed."
  [u]
  (let [h (feed-home (url u))
	[home fd] (if h [h u]
		      [u (canonical-feed u)])]
    {:homepage [home (home-feed-outlinks home)]
     :entries [fd (entry-feed-outlinks fd)]}))

(defn merge-outlinks [outlinks-map]
  (into #{} (concat (second (:entries outlinks-map))
		    (second (:homepage outlinks-map)))))

(comment
  (fetch-entries "http://www.rollingstone.com/siteServices/rss/allNews")
  (compact-date-time "Sun, 31 Oct 2010 03:03:00 EDT")
  (fetch-entries (java.net.URL. "http://www.rollingstone.com/siteServices/rss/allNews"))
  (canonical-feed "http://www.rollingstone.com/")
  (canonical-feed "http://techcrunch.com/2010/11/02/andreessen-horowitz-650m-fund/")
  time-coerce/from-date
  ; This one requires fix-link, otherwise doesn't work
  (canonical-feed "http://npr.org")
  (fetch-entries "http://www.nytimes.com/services/xml/rss/nyt/HomePage.xml")
  (canonical-feed "http://io9.com/")
  (canonical-feed "http://www.huffingtonpost.com/"))