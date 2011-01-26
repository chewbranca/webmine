(ns webmine.parser
  ^{:doc
  "parsing foo for learning machine hackers of the interwebs
  htmlparsing showdown: http://www.benmccann.com/dev-blog/java-html-parsing-library-comparison/
  revisit the HtmlParser and DOMBuilder in nutch and bixo if it seems like we
  are having issues and they ahve a few more error cases handled."}  
  (:require [clojure.contrib.seq-utils :as seq]
	    [clojure.string :as str])
  (:use clojure.xml
        webmine.core
        webmine.urls
        [plumbing.core :only [ToSeqable to-seq maybe-comp]])
  (:import java.io.StringReader
           org.ccil.cowan.tagsoup.Parser
           (org.w3c.dom Node Document Element NodeList Attr)
           (org.xml.sax XMLReader InputSource)
           (javax.xml.transform Transformer TransformerFactory)
           javax.xml.transform.sax.SAXSource
           javax.xml.transform.dom.DOMResult
           org.apache.commons.lang.StringEscapeUtils))

(defn dom [source]
  "html string -> dom using TagSoup.
   the features we set on the parser come from different implementations that I found in nutch, HtmlParser, as well as other parsers."
  (when source
    (try
     (let [result (org.apache.xalan.xsltc.trax.SAX2DOM.)
	   input (if (instance? java.net.URL source)
		   (.openStream ^java.net.URL source)
		   (StringReader. ^String source))
	   parser (doto (Parser.)
		    (.setContentHandler result)
		    (.setFeature Parser/namespacesFeature false)
		    (.setFeature Parser/namespacePrefixesFeature false)
		    (.setFeature Parser/bogonsEmptyFeature false)
		    (.setFeature Parser/ignoreBogonsFeature true)
		    (.parse (InputSource. ^java.io.Reader input)))]
       (.getDOM result))
     (catch org.w3c.dom.DOMException _ )
     (catch java.io.IOException _ )))) ;;pushback buffer overflow

; const unsigned short  ELEMENT_NODE                   = 1;
; const unsigned short  ATTRIBUTE_NODE                 = 2;
; const unsigned short  TEXT_NODE                      = 3;
; const unsigned short  CDATA_SECTION_NODE             = 4;
; const unsigned short  ENTITY_REFERENCE_NODE          = 5;
; const unsigned short  ENTITY_NODE                    = 6;
; const unsigned short  PROCESSING_INSTRUCTION_NODE    = 7;
; const unsigned short  COMMENT_NODE                   = 8;
; const unsigned short  DOCUMENT_NODE                  = 9;
; const unsigned short  DOCUMENT_TYPE_NODE             = 10;
; const unsigned short  DOCUMENT_FRAGMENT_NODE         = 11;
; const unsigned short  NOTATION_NODE                  = 12;

(defn element? [#^Node node]
 (and node (= (.getNodeType node) Node/ELEMENT_NODE)))

(defn text-node? [#^Node node]
  (and node (= (.getNodeType node) Node/TEXT_NODE)))

(extend-protocol  ToSeqable
  org.w3c.dom.NodeList
  (to-seq [this]
       (for [i (range 0 (.getLength this))]
	 (.item this (int i)))))

(defn children
  "children of current node"
  [^Node n]
  (to-seq (.getChildNodes n)))

(defn attr [#^Node n #^String a]
  (if-let [attrs (.getAttributes n)]
    (if-let [att (.getNamedItem attrs a)]
      (.getValue att))))
        
(defn attr-map 
  "returns node attributes as map of keyword attribute keys to str value"
  [#^Node n]
  (when-let [attrs (.getAttributes n)]
       (into {}
          (for [i (range (.getLength attrs))
                :let [^Attr item (.item attrs i)]
		:when (.getSpecified item)]
            [(-> item (.getName) keyword) (.getTextContent item)]))))      

(defn href [n] (attr n "href"))
(defn src [n] (attr n "src"))
(defn node-type [n] (attr n "type"))

(defn- attr-str [^Node n]
  (str/join " " (map (fn [[k v]] (format "%s=\"%s\"" (.substring (str k) 1) v)) (attr-map n))))

(defn html-str
  "return the html string representing the node; should
   be semantically equivlaent but attribute order and other
   things like spacing and formatting may be gone."
  [^Node n]
  (cond
     (= (.getNodeType n) Node/DOCUMENT_NODE)
         (html-str (first (children n)))
     (element? n)
	  (str "<" (.getNodeName n) " "
	           (attr-str n)
		">"
		(apply str (map html-str (children n)))
		"</" (.getNodeName n) ">")
     (text-node? n) (.getNodeValue n)))

;;TODO: script thing still not working?
(defn extract-text [^Node n]
  (if (not (text-node? n))
    ""
    (.getNodeValue n)))

(defn extract-href [n]
  (if-let [link (href n)]
    link ""))

(defn elements
  "gets the elements of a certian name in the dom
   (count (divs (dom (:body (fetch (url \"http://ftalphaville.ft.com/\")))))) -> 199"
  [p ^String t]
  (when p
    (let [node-list (condp #(isa? %2 %1) (class p)
		      Document (.getElementsByTagName ^Document p t)
		      Element (.getElementsByTagName ^Element p t))]
      (filter identity (to-seq node-list)))))

(defn strip-from-dom
  [#^Document d es]
  (doseq [#^Node e es]
    (.removeChild (.getParentNode e) e))
  (.normalize d)
  d)

(defn strip-tags [d & tags]
  (if (or (not tags)
	  (empty? tags))
    d
    (recur (strip-from-dom d (elements d (first tags)))
	   (rest tags))))

;;TODO: WTF is up with the required calling of strip-non-content twice?
;;something about the side effects happening in the stip tags or stip from dom fns?
(defn strip-non-content [d]
  (let [f #(strip-tags % "script" "style")]
    (f (f d))))

(defn divs
  "gets the divs in a dom.
   (count (divs (dom (:body (fetch (url \"http://ftalphaville.ft.com/\")))))) -> 199"
  [d]
  (elements d "div"))

(defn anchors [d] (elements d "a"))

(defn head [^Document d]
  (.item
   (.getElementsByTagName d "head")
   0))

(defn hrefs [es]
  (filter (comp not nil?) (map (comp url href) es)))

(defn do-children [#^Node n f]
  (if (not (and n (.hasChildNodes n)))
    []
    (let [children (.getChildNodes n)]
      (doall (for [i (range 0 (.getLength children))]
	       (f (.item children i)))))))

(defn walk-dom
  "recursively walk the dom.
  combine: result of visiting a single node & rest -> combines them as
  appropriate for the representational structure
  init: initilial value for combine
  visit: visits one node and produces a result. if result of visit
  is :kill, then do not recurse on node and ignore node"
  ([d visit combine]
     (let [extractor (fn extract [n]
		       (let [r (visit n)]
			 (when-not (= r :kill)
			   (combine r
				    (do-children n extract)))))]
       (extractor d))))

(defn text-from-dom
  "recursively get the text content from Nodes.
   inspired by: http://www.prasannatech.net/2009/02/convert-html-text-parser-java-api.html"
  [d]
  (walk-dom
   d
   extract-text
   (fn [head tail]
     (let [results (cons head (flatten tail))
	   buffer  (StringBuffer.)]
       (doall (map #(.append buffer %) results))
       (str buffer)))))

(defn unescape-html
  "Convert HTML entities to proper Unicode characters."
  [^String t]
  (StringEscapeUtils/unescapeHtml t))

(defn clean-text [d]
  "Returns a string of sanitized text content from an HTML document."
  (-> d
      strip-non-content
      text-from-dom
      unescape-html))

(defn scrub-html
  "takes a document map and a list of keys containing html strings.
returns a map with the values at those keys scrubbed down to clean text."
  [doc & keys]
  (reduce (fn [d k]
            (assoc d k
                   (or ((maybe-comp clean-text dom k) d)
                       (k d))))
          doc
          keys))

;;(hrefs (elements (head d) "link"))
;;(links-from-dom (head d))
(defn links-from-dom
  "recursively get the links content from Nodes."
  [d]
  (flatten (walk-dom
    d
    extract-href
    (fn [head tail]
      (filter (comp not empty?) (cons head tail))))))

(defn count-with
  "count how many nodes match a pred.
  usage:  (count-with some-dom element?) -> number of nodes of type element-node"
  [d pred]
  (walk-dom
    d
    (fn [n] (if (pred n) 1 0))
    (fn [head tail]
      (apply + (flatten (cons head tail))))))

(defn extract-features [response]
  (assoc response
    :body ((maybe-comp text-from-dom dom :body) response)))