(ns webmine.feeds
  (:use [clojure.xml :only [parse]]
        clojure.set
        clojure.contrib.java-utils
        webmine.readability
        html-parse.parser
        webmine.urls
        webmine.core
        mochi.nlp.process.sent-splitter
        plumbing.core
        plumbing.error
        [clojure.java.io :only [input-stream]]
	[fetcher.client :only [request]])
  (:require [work.core :as work]
            [clojure.zip :as zip]
            [webmine.images :as imgs]
            [clojure.contrib.logging :as log]
            [clojure.contrib.zip-filter :as zip-filter]
            [clojure.contrib.zip-filter.xml :as xml-zip]
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

;;
;; Date Stuff
;;

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
		    (catch java.lang.Exception _ nil))]
	  :when d]
      (-> d time-coerce/from-date time-coerce/to-string))
    (for [fmt (vals time-fmt/formatters)
	  :let [d (try (time-fmt/parse fmt s)
		       (catch java.lang.Exception _ nil))]
	  :when d] (-> d time-coerce/to-string)))))

;; 
;; Sentence Parsing
;;

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
  [k t]
  (let [sps (sent-spans t)
	idx (if (< (count sps) k)
	      (second (last sps))
	      (second (nth sps (- k 1))))]
    (when (> idx 0)
      (.substring t 0 (- idx 1)))))

(defn with-des
  "assumes entry already has {:keys [text]}
   fields present. returns entry with :des field"
  [entry]
  (let [min-sentences 3
        d (:des entry)
        c (:content entry)
        t (:text entry)
        des (->> (if (and d
                          (>= (count-sentences (clean-text (dom d)))
                              min-sentences))
                   d t)
                 dom
                 clean-text
                 (first-k-sentences min-sentences))]
    (assoc entry :des des)))

(defn fetch-body
  "Takes an entry.  assocs' in the body of the link."
  [e]
  (assoc e :body
	 (:body (request :get (:link e)))))

(defn with-image [e]
  (imgs/with-best-img e :link :body))

(defn with-text [e]
  (assoc e :text
         (-> e
             :body
             dom
             readability-div
             pretty-dom
             clean-text
	     dom
             html-str2)))

(defn complete-entry [e]
  (-x> e with-text with-des with-image))

(defn- root-> [source f]
  (when-let [root (-> source parse zip/xml-zip)]
    (f root)))

(defn node-reader [root k]
  (xml-zip/xml1-> root k xml-zip/text))

(defn- rss-item-node-to-entry [item]
  (let [item-root (zip/xml-zip item)
	get-text (partial node-reader item-root)]
    {:title
     (get-text :title)
     :link
     (get-text :link)
     :content
     (apply max-key count
            (map get-text [:content :description :content:encoded]))
     :des
     (first (filter identity
		    (map get-text [:description :content :content:encoded])))
     :date
     (first (for [k [:pubDate :date :updatedDate :dc:date]
		  :let [s (get-text k)]
		  :when s] s))
     :author
     (first (filter identity
                    (map #(get-text %)
                         [:author :dc:creator])))}))

(defn- rss-feed-meta [root]
  (let [get-text (partial node-reader root)])
  {:title
   (xml-zip/xml1-> root :channel :title xml-zip/text)
   :des
   (xml-zip/xml1-> root :channel :description xml-zip/text)
   :gen
   (xml-zip/xml1-> root :channel :generator xml-zip/text)
   :lang
   (xml-zip/xml1-> root :channel :language xml-zip/text)
   :img
   (xml-zip/xml1-> root :channel :image :url xml-zip/text)
   :link
   (xml-zip/xml1-> root :channel :link xml-zip/text)})

(defn- rss-feed-entries [root]
  (let [rss2-items (fn [k] (xml-zip/xml-> root :channel k zip/node))
        rss1-items (fn [k] (xml-zip/xml-> root k zip/node))
        nodes (find-first (complement empty?)
                          [(rss1-items :item) (rss1-items :entry)
                           (rss2-items :item) (rss2-items :entry)])]
    (for [n nodes
          :let [entry (into {}
                            (filter second
                                    (rss-item-node-to-entry n)))]]
      entry)))

(defn- parse-rss [root]
  "returns record Feed representing a snapshot of a feed. Supports keys
  :title Name of feed
  :des Description of feed
  :link link to feed
  :entries seq of Entry records, see doc below for entries"
  (let [feed-meta (rss-feed-meta root)]
    (assoc feed-meta :entries (rss-feed-entries root))))

(defn- atom-item-node-to-entry [node]
  {:date (find-first
          (map
           (fn [k] (-> (xml-zip/xml1-> node k xml-zip/text)))
           [:updated
            :modified
            :created
            :published
            :issued]))
   :author (xml-zip/xml1-> node :author :name xml-zip/text)
   :title (xml-zip/xml1-> node :title xml-zip/text)
   :link (xml-zip/xml1-> node :link (xml-zip/attr :href))
   :des (xml-zip/xml1-> node :summary xml-zip/text)
   :content (xml-zip/xml1-> node :content xml-zip/text)})
 

(defn- atom-feed-meta [root]
  {:title
   (xml-zip/xml1-> root  :title xml-zip/text)
   :des
   (xml-zip/xml1-> root :subtitle  xml-zip/text)
   :gen
   (xml-zip/xml1-> root :generator xml-zip/text)
   :lang
   (xml-zip/xml1-> root :link (xml-zip/attr :hreflang))
   :img
   (xml-zip/xml1-> root  :image xml-zip/text)
   :link
   (xml-zip/xml1-> root :link (xml-zip/attr :href))})

;;TODO:  make this case work: view-source:http://cityroom.blogs.nytimes.com/feed/
(defn- parse-atom [root]
  (assoc (atom-feed-meta root)
    :entries (map atom-item-node-to-entry (xml-zip/xml-> root :entry))))

(defn ensure-title [feed]
  (if (:title feed)
    feed
    (let [title
	  (-> (:entries feed)
	      first
	      :link
	      url
	      host
	      strip-subdomain)]
      (assoc feed :title title))))

(defn parse-feed [url-or-source]
  (let [source (if (or (instance? java.net.URL url-or-source)
                       (.startsWith ^String url-or-source "http"))
                 (slurp url-or-source)
                 url-or-source)
        root (-> ^String source
		 (.getBytes "UTF-8")
		 java.io.ByteArrayInputStream.
                 parse
                 zip/xml-zip)]
    (-> (cond 
         (xml-zip/xml1-> root :channel) (parse-rss root)
         (xml-zip/xml-> root :entry) (parse-atom root)
         :default
         (RuntimeException. "Unknown feed format"))
        ;; Ensure date compacted
        (update-in [:date] compact-date-time)
        (update-in [:entries] (fn [es] (map #(update-in % [:date] compact-date-time)
                                            es)))
        ensure-title)))

(defn- entries [url]
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
  (when-let [es (-> url parse-feed :entries)]
    (map (partial into {}) es)))

(defn fetch-entries [u]
  (-> u url entries))

(defn fetch-feed-meta [u]
  (-> u url parse-feed (dissoc :entries)))

(defn extract-entries [body]
  (-> body parse-feed :entries))

(defn feed? [item]
  (and item
       (let [feed (parse-feed item)]
	 (-> feed :entries empty? not))))

(defn links-from-entry [e]
  (-> e :content url-seq))

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

(defn- bad-ext? [link]
  (or  (.contains link "comments")
       (.endsWith link "xmlrpc.php")
       (.endsWith link "osd.xml")
       (.endsWith link "/opensearch.xml")))

(defn- good-feed-name? [n]
  (or (.contains n "rss")
      (.contains n "atom")))

(defn good-rss?
  ([link] (good-rss? link nil))
  ([link type]
     (or
      (and
       link
       (not type)
       (not (bad-ext? link))
       (good-feed-name? link))
      (and
       link
       type
       (not (bad-ext? link))
       (or (good-feed-name? link)
	   (good-feed-name? type))))))

(defn fix-link
  "fix absolute links"
  [base #^String link]
  (when link
    (let [l (.trim link)]
      (if (.startsWith link "/")
	(str base link)
	link))))

;;TODO: factor out duplciaiton between good-feed-elements and good-feed-links
(defn good-feed-elements
  [page-url all-links]
  (into #{}
	(remove nil? 
		(for [l all-links
		      :when l
		      :let [attr (attr-map l)
			    #^String type (:type attr)
			    #^String link
			    (->> attr
				 :href
				 (fix-link
				  (host-url (str page-url))))]
		      :when (good-rss? link type)]
		  link))))

(defn good-feed-links
  [page-url all-links]
  (into #{}
	(remove nil? 
		(for [l all-links
		      :when l
		      :let [#^String link
			    (fix-link
			     (host-url (str page-url)) l)]
		      :when (good-rss? link)]
		  link))))

(defn host-rss-feeds
  "checks head of page for rss feed links. Does two checks, any links
   that are marked as rss/xml/atom in the link type or if
   any link has rss xml or  "
  [page-url & [body]]
  ;;most sites go with the standard that the rss or atom feed is in the head, so we only check the header for now.
  (let [body (or body (:body (request :get (str page-url))))
	d (dom body)
	element-feeds (good-feed-elements
		       page-url (elements d "link"))]
    (if (not (empty? element-feeds))
      element-feeds
      (good-feed-links page-url (map str (url-seq body))))))

(defn attempt-rss2
  "attempt to get an rss2 feed if this is an rss feed"
  [u]
  (if (and u (.endsWith u "rss"))
    (let [rss2 (str u "2")]
      (if (exists? rss2)
	rss2
	u))
    u))

(defn ^{:doc "Avoid subscribing to multiple feeds on the same blog.
Initial heuristic is to take url with min length.
May not be a good idea for blogs that have many useful feeds, for example, for a news site like NYT."}
     canonical-feed [& args]
     (let [fds (apply host-rss-feeds args)]
       (-> fds
           min-length
           attempt-rss2)))

;;
;; Feed outlink crawling
;;

(defn find-outlinks
  "string (page body) -> url (blog homepage) -> outlinks"
  [s h]
  (seq (into #{}
	     (work/filter-work
	      #(external? (url h) %)
	      (url-seq s)
	      20))))

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
	       (work/map-work canonical-feed 20 outs))
	;;we need to filter same host feeds again, as they can get filtered from outlinsk but then be found again when extracting canonical feeds.
	ex-feeds (into #{} (filter #(external? (url u) (url %))
				   feeds))]
    (seq ex-feeds)))

(defn home-feed-outlinks
  [u]
  (find-feed-outlinks (:body (request :get u)) u))

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
  (def es (fetch-entries "http://blog.fontdeck.com/rss"))
  (first es)
  (fetch-entries "http://www.rollingstone.com/siteServices/rss/allNews")
  (compact-date-time "Sun, 31 Oct 2010 03:03:00 EDT")
  (fetch-entries (java.net.URL. "http://www.rollingstone.com/siteServices/rss/allNews"))
  (fetch-entries "http://scripting.com/rss.xml")
  (fetch-entries "http://feeds.nytimes.com/nyt/rss/HomePage")
  (fetch-entries "http://feeds.feedburner.com/oreilly/news")
  (fetch-entries "http://feeds.feedburner.com/oreilly/news"))
