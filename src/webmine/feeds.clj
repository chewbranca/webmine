(ns webmine.feeds
  (:use clojure.xml
        clojure.set
        clojure.contrib.java-utils
        webmine.core
        webmine.readability
        webmine.parser
        webmine.urls
        mochi.nlp.process.sent-splitter
	[plumbing.core]
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
	  :let [d (try-silent
		    (.parse sdf (.trim s) (ParsePosition. 0)))]
	  :when d]
      (-> d time-coerce/from-date time-coerce/to-string))
    (for [fmt (vals time-fmt/formatters)
	  :let [d (try-silent (time-fmt/parse fmt s))]
	  :when d] (-> d time-coerce/to-string)))))

(defn to-char [s] (.charAt s 0))

(defn period? [c] (= (to-char ".") c))

(defn count-sentences [s]
  (count (filter period? s)))


;;TODO: AB against sentence text-only approach below
(defn sentences-from-ptags
  ([body k]
     (sentences-from-ptags body k ""))
  ([body k text]
  (let [d (dom body)
	ps (elements d "p")
	t (str text (text-from-dom (first ps)))]
       (if (or (>= (count-sentences t)
		   k)
	       (empty? (rest ps)))
	 t
	 (recur (rest ps) k t)))))

(defn first-k-sentences
"returns the first k sentences from a string t."
[t k]
  (let [sps (sent-spans t)
	idx (if (< (count sps) k)
	      (second (last sps))
	      (second (nth sps (- k 1))))]
    (when (> idx 0)
	(.substring t 0 (- idx 1)))))

(defn with-des [entry]
  (let [min-sentences 3
	d (:des entry)
	b (:body entry)
	c (:content entry)]
    (if (and d
             (not= d c)
             (>= (count-sentences (clean-text (dom d)))
                 min-sentences))
      entry
      (assoc entry :des (-> b extract-content (first-k-sentences  min-sentences))))))

(defn fetch-body
  "Takes an entry.  assocs' in the body of the link."
  [e]
  (assoc e :body
	 (:body (http/get (:link e)))))

(defrecord Feed [title des link entries])

(defrecord FeedEntry [title link content des date author])

(defn with-image [e]
  (imgs/with-best-img e :link :body))

(defn with-text [e]
  (assoc e :text
  (extract-content (:body e))))

(defn complete-entry [e]
  (-> e fetch-body with-text with-des with-image))

(defn- root-> [source f]
  (when-let [root (-> source parse zip/xml-zip)]
    (f root)))

;; RSS 

(defn- item-node-to-entry [item]
  (let [item-root (zip/xml-zip item)
        get-text (fn [k] (xml-zip/xml1-> item-root k xml-zip/text))]
    (FeedEntry.
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
     (try-silent (first (for [k [:pubDate :date :updatedDate :dc:date]
		       :let [s (get-text k)]
		       :when s] (compact-date-time s))))
     ;; author
     (get-text :author))))

;;TODO: add full spec and stuff people use: like language and avatar image.
(defn- feed-meta [root]
  {:title
   (xml-zip/xml1-> root :channel :title xml-zip/text)
   :des
   (xml-zip/xml1-> root :channel :description xml-zip/text)
   :gen
   (xml-zip/xml1-> root :channel :generator xml-zip/text)
   :link
   (xml-zip/xml1-> root :channel :link xml-zip/text)})

(defn- parse-feed-meta [source]
  (root-> source feed-meta))

(defn- feed-entries [root]
  (let [get-items (fn [k] (xml-zip/xml-> root :channel k zip/node))
	 nodes (find-first (complement empty?)
			   [(get-items :item) (get-items :entry)])]
    (for [n nodes
	  :let [entry (into {}
			    (filter second
				    (item-node-to-entry n)))]]
      entry)))

(defn- parse-entries [source]
  (root-> source feed-entries))

(defn- parse-rss [source]
  "returns record Feed representing a snapshot of a feed. Supports keys
  :title Name of feed
  :des Description of feed
  :link link to feed
  :entries seq of Entry records, see doc below for entries"
  (root-> source
	  (fn [root]
	    (let [{t :title d :des l :link} (feed-meta root)
		  es (feed-entries root) ]
	      (Feed. t d l es)))))

;; Atom

(defn- rome-entry-as-map [e]
   (into {} (map #(if (nil? (second %)) [(first %) ""] %)
   {:date (-> e  .getPublishedDate time-coerce/from-date time-coerce/to-string )
    :author (.getAuthor e)
    :title (.getTitle e)
    :link (.getLink e)
    :des  (-?> e .getDescription .getValue)
    :content (-?>> e .getContents
		   (map #(.getValue %))
		   (apply str))})))

(defn- parse-atom [source]
  (try-silent
   (let [synd-feed (.build (SyndFeedInput.) (XmlReader. source))]
     {:title (.getTitle synd-feed)
      :des (.getDescription synd-feed)
      :link (.getTitle synd-feed)
      :entries (map rome-entry-as-map (.getEntries synd-feed))})))

(defn- parse-feed [url]
  (let [source (slurp url)
	to-is #(-> source (.getBytes "UTF-8") java.io.ByteArrayInputStream.)
	root (-> (to-is)		 		 
		 parse
		 zip/xml-zip)]
    (cond
     (xml-zip/xml1-> root :channel) (-> (to-is) parse-rss)
     (xml-zip/xml-> root :entry) (-> (to-is) parse-atom)
     :default
       (RuntimeException. "Unknown feed format"))))

(defn- entries [source]
  "
  takes a string or inputstream
  return seq of entries from rss feed source.
  Each entry is a map with string values
  :title entry title
  :img image url
  :des  descritpion
  :date String of date (uses to-string canonical
   rep in clj-time.coerce)
  :author author string
  :content Content of entry (or :description if not content)
  :link Link to content. "
  (if-let [es (parse-entries source)]
    (map (partial into {}) es)))

(defn fetch-entries [u]
  (-> u url input-stream entries))

(defn fetch-feed-meta [u]
  (-> u url input-stream parse-feed-meta))

(defn extract-entries [body]
  (-> body (.getBytes "UTF-8")
	       input-stream
	       entries))

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
  ;;most sites go with the standard that the rss or atom feed is in the head, so we only check the header for now.
  (let [d (-> page header-str dom)
	first-attempt
	;; sometimes rss feeds are marked as
	;; <link type="application/rss+xml">
	(when-let [all-links (elements d "link")]
	  (for [l all-links
		:when l
		:let [attr (attr-map l)
		      #^String type (:type attr)
		      #^String link (->> attr :href
					 (fix-link (host-url (str page))))]
		:when (and link
			   type
			   (or (.contains type "rss")
			       (.contains type "atom"))) ]
	    link))

;;most of the search urls are of the form:
;;<link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="FriendFeed Search"/>

;;specificly, they type="application/opensearchdescription+xml".  We can filter these out below, but first let's see how often we get a good feed if we ignore the fallback case and take only feeds in the header and identified by the standard.

	;;if we didn't get anything on the first attempt, get all links in head and see if we can find an rss link.
	;; all-feeds (if (not (empty? first-attempt))
	;; 	    first-attempt
		    
	;; 	    (when-let [head-links (-> d head links-from-dom)]
	;; 	      (->> head-links
	;; 		   (map (partial fix-link (str page)))
	;; 		   (filter #(and (not (comment? %)) (rss-suffix? %))))))
	]

    (into #{}	  
	  (filter identity #_(comp feed? url) first-attempt))))


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
  (use 'webmine.readability)
  (fetch-entries "http://www.rollingstone.com/siteServices/rss/allNews")
  (compact-date-time "Sun, 31 Oct 2010 03:03:00 EDT")
  (fetch-entries (java.net.URL. "http://www.rollingstone.com/siteServices/rss/allNews"))
  (canonical-feed "http://www.rollingstone.com/")
  (canonical-feed "http://techcrunch.com/2010/11/02/andreessen-horowitz-650m-fund/")
  ; This one requires fix-link, otherwise doesn't work
  (canonical-feed "http://npr.org")
  (fetch-entries "http://scripting.com/rss.xml")
  (fetch-entries "http://feeds.nytimes.com/nyt/rss/HomePage")
  (canonical-feed "http://io9.com/")
  (canonical-feed "http://www.huffingtonpost.com/"))