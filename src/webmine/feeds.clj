(ns webmine.feeds
  (:use [clojure.xml :only [parse]]
        clojure.set
        webmine.readability
        plumbing.core
        plumbing.error
        [clojure.java.io :only [input-stream file]]
	[fetcher.core :only [fetch]]
	[clojure.string :only [trim]])
  (:require [work.core :as work]
            [clojure.zip :as zip]
            [webmine.images :as imgs]
            [html-parse.parser :as parser]
            [clojure.contrib.logging :as log]
            [clojure.contrib.zip-filter :as zip-filter]
            [clojure.contrib.zip-filter.xml :as xml-zip]
            [clj-time.format :as time-fmt]
            [clj-time.coerce :as time-coerce])  
  (:import [org.joda.time.format DateTimeFormat]
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
  [^String s]
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
     (find-first
      (map get-text [:link :Link :guid]))
     :content
     (apply max-key count
            (map get-text [:content :description :content:encoded]))
     :des
     (find-first
      (map get-text [:description :content :content:encoded]))
     :date
     (find-first
      (map get-text [:pubDate :date :updatedDate :dc:date]))
     :author
     (find-first (map get-text
		      [:author :dc:creator]))}))

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
    (let [^String title
	  (-> (:entries feed)
	      first
	      :link
	      parser/url
	      parser/host)]
      (assoc feed
	:title (.replaceAll title "www." "")))))

(defn parse-feed [url-or-source]
  (let [source (if (or (instance? java.net.URL url-or-source)
                       (.startsWith ^String url-or-source "http"))
                 (slurp url-or-source)
                 url-or-source)
        root (-> ^String source
		 (.getBytes)
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
        (update-in [:entries]
		   (fn [es]
		     (map #(update-in % [:date] compact-date-time)
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
  (-> u parser/url entries))

(defn fetch-feed-meta [u]
  (-> u parser/url parse-feed (dissoc :entries)))

(defn assoc-title [{:keys [title entries] :as feed}]
  (map #(assoc % :feed-title title) entries))

(defn extract-entries [body]
  (-> body parse-feed assoc-title))

(defn with-des
  [{:keys [des] :as entry}]
  (if (not des) entry
      (assoc entry :des (parser/clean-text (parser/dom des)))))

(defn with-text [{:keys [dom resolved title] :as entry}]
  (let [div (readability-div dom)
	text (->> div
		  parser/text-from-dom
		  parser/replace-unicode-control
		  trim)
	html (->> div
		  parser/pretty-dom
		  (parser/expand-relative-urls resolved)
		  parser/html-str2
		  parser/replace-unicode-control
		  trim)]
    (-> entry
	(assoc :text text
	       :html html))))

(defn feed? [item]
  (and item
       (let [feed (parse-feed item)]
	 (-> feed :entries empty? not))))

(defn links-from-entry [e]
  (-> e :content parser/url-seq))

(defn- bad-ext? [^String link]
  (or  (.contains link "comments")
       (.contains link "comment")
       (.endsWith link "xmlrpc.php")
       (.endsWith link "osd.xml")
       (.endsWith link ".jpg")
       (.endsWith link ".png")
       (.endsWith link "/opensearch.xml")))

(defn- good-feed-name? [^String n]
  (or (.contains n "rss")
      (.contains n "atom")
      (.contains n "feed")
      (.contains n "xml")))

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

(defn drop-front [^Integer n ^String s]
  (.substring s n (.length s)))

(defn drop-back [^Integer n ^String s]
  (.substring s 0 (- (.length s) n)))

(defn to-url [base ^String rel]
  (let [r (if (.startsWith rel "/")
	    (drop-front 1 rel)
	    rel)
	bs (str base)
	b (if (.endsWith bs "/")
	    (drop-back 1 bs)
	    bs)]
    (str b "/" r)))

(defn make-absolute
  "fix absolute links"
  [base #^String link]
  (let [l (.trim link)]
    (cond
     (.startsWith l "http") l
     (.startsWith l "www") l
     (.startsWith l "feed://") (.replace l "feed://" "http://")
     :else (to-url base l))))

(defn good-links
  [extract page-url all-links]
  (into #{}
	(for [l all-links
	      :when l
	      :let [[^String lk ^String type] (extract l)
		    link (make-absolute
			  (parser/host-url (str page-url)) lk)]
	      :when (and link (good-rss? link type))]
	  link)))

(def good-feed-elements
     (partial good-links
	      (fn [l]
		(let [attr (parser/attr-map l)]
		  [(:href attr) (:type attr)]))))

(def good-feed-links
     (partial good-links (fn [l] [l nil])))

(defn first-good-link [urls]
  (some (fn [e]
	  (let [attrs (parser/attr-map e)]
	    (when (and (or  (= "application/atom+xml"
			       (:type attrs))
			    (= "application/rss+xml"
			       (:type attrs)))
		       (= "alternate"
			  (:rel attrs)))
	      (:href attrs))))
	urls))

;;check standard head link location first

(defn head-feed [d]
  (let [head (first (parser/elements d "head"))
	links (parser/elements head "link")
	good (first-good-link links)]
    (if good good
	(first-good-link (parser/elements d "link")))))

(defn host-rss-feeds
  [page-url & [body]]
  (let [body (or body (:body (fetch :get (str page-url))))
	d (parser/dom body)]
    (if-let [l (head-feed d)]
      ;;returns the singleton best feed.  TODO: can return multiple feeds when we convert to a learned algorithm.
      [(make-absolute
	 (parser/host-url (str page-url)) l)]
      (good-feed-links page-url (map str (parser/url-seq body))))))

(defn attempt-rss2
  "attempt to get an rss2 feed if this is an rss feed"
  [^String u]
  (if (and u (.endsWith u "rss"))
    (let [rss2 (str u "2")]
      (if (= 200 (:status (fetch :head rss2)))
	rss2
	u))
    u))

(defn canonical-feed [& args]
  (->> (apply host-rss-feeds args)
       (min-by (comp count str))
       attempt-rss2))