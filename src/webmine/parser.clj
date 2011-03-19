(ns webmine.parser
  ^{:doc
  "parsing foo for learning machine hackers of the interwebs
  htmlparsing showdown: http://www.benmccann.com/dev-blog/java-html-parsing-library-comparison/
  revisit the HtmlParser and DOMBuilder in nutch and bixo if it seems like we
  are having issues and they ahve a few more error cases handled."}  
  (:require [clojure.contrib.seq-utils :as seq]
            [clojure.string :as str])
  (:use clojure.xml
        clojure.set
        webmine.urls
        [plumbing.core :only [ToSeqable to-seq maybe-comp]])
  (:import java.io.StringReader
	   java.io.StringWriter
           org.ccil.cowan.tagsoup.Parser
           (org.w3c.dom Node Document Element NodeList Attr)
           (org.xml.sax XMLReader InputSource)
           (javax.xml.transform Transformer TransformerFactory)
	   javax.xml.parsers.DocumentBuilderFactory
	   javax.xml.transform.sax.SAXSource
           javax.xml.transform.dom.DOMResult
           javax.xml.transform.dom.DOMSource
           javax.xml.transform.OutputKeys
	   javax.xml.transform.stream.StreamResult
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

(defn document []
  (-> (DocumentBuilderFactory/newInstance)
      (.newDocumentBuilder)
      (.newDocument)))

(defn node? [n]
  (instance? Node n))

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
   (count (elements doc \"div\")) -> 199"
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

(defn raise-in-dom
  [#^Document d es]
  (doseq [^Node e es]
    (let [children (children e)
	  parent (.getParentNode e)]
      (doseq [^Node child children]
	(.insertBefore parent child e))
      (.removeChild parent e)))
  (.normalize d)
  d)

(defn raise-tags [d & tags]
  (if (or (not tags)
	  (empty? tags))
    d
    (recur (raise-in-dom d (elements d (first tags)))
	   (rest tags))))

;;TODO: WTF is up with the required calling of strip-non-content twice?
;;something about the side effects happening in the stip tags or stip from dom fns?
(defn strip-non-content [d]
  (let [f #(strip-tags %
		       "script" "style" "form"
		       "object" "table" "h1"
		       "h2" "h3" "iframe")]
    (f (f d))))

(def html-elements
#{"dd" "big" "col" "head" "sub" "tt" "basefont" "a" "b" "body" "tfoot" "acronym" "pre" "img" "form" "iframe" "meta" "caption" "small" "noframes" "var" "dl" "em" "fieldset" "isindex" "i" "h1" "h2" "hr" "span" "input" "del" "h3" "script" "html" "dfn" "h4" "noscript" "optgroup" "legend" "bdo" "dir" "param" "area" "h5" "frame" "kbd" "code" "h6" "sup" "table" "ins" "font" "blockquote" "br" "p" "dt" "td" "abbr" "q" "samp" "div" "style" "base" "button" "strike" "s" "thead" "th" "label" "address" "center" "u" "option" "frameset" "tbody" "cite" "ul" "strong" "title" "applet" "textarea" "link" "select" "map" "li" "ol" "tr" "colgroup" "menu" "object"})

(def whitelist-elements
#{"dd" "col" "a" "tfoot" "acronym" "pre" "caption" "dl" "em" "h1" "h2" "del" "h3" "h4" "h5" "code" "h6" "table" "ins" "blockquote" "p" "dt" "td" "abbr" "thead" "address" "tbody" "body" "html" "cite" "ul" "strong" "li" "ol" "tr" "colgroup" "i" "hr" "strike" "th" "sub" "tt" "b" "big" "q"})

(def blacklist-elements
     (difference html-elements whitelist-elements))

(defn strip-blacklist [d]
  (let [f #(apply strip-tags
		  % blacklist-elements)]
    (f (f d))))

(defn raise-content [d]
  (let [f #(raise-tags
		  % "div" "span")]
    (f (f d))))

(defn pretty-dom [d]
  (-> d
      raise-content
      strip-blacklist))

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
    (doall (map f (children n)))))

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

(defn text-from-elements [es]
  (apply str (map extract-text es)))

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

(defn strip-space [s]
  (.trim (.replaceAll s "[\n \t]{3,}" "\n")))

(defn clean-text [d]
  "Returns a string of sanitized text content from an HTML document."
  (-> d
      strip-non-content
      text-from-dom
      unescape-html
      strip-space))

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

(defn html-str
  "return the html string representing the node; should
   be semantically equivlaent but attribute order and other
   things like spacing and formatting may be gone."
  [^Node node]
  (let [d (if (instance? org.w3c.dom.Document node)
            node
            (.getOwnerDocument node))]
    (-> d
        (.getImplementation)
        (.createLSSerializer)
        (.writeToString node))))

(defn html-str2 [d]
  (let 
      [out (StreamResult. (StringWriter.))
       tf (doto
	      (.newTransformer (TransformerFactory/newInstance))
	    (.setOutputProperty
	     OutputKeys/OMIT_XML_DECLARATION "yes")
	    (.setOutputProperty
	     OutputKeys/METHOD "xml")
	    (.setOutputProperty OutputKeys/ENCODING "UTF-8")
	    (.setOutputProperty OutputKeys/INDENT "yes"))]
    (.transform tf (DOMSource. d) out)
    (strip-space
     (.toString (.getWriter out)))))

(defn charset
  "Get charset from meta tag."
  [d]
  (let [ms (elements d "meta")]
    (if-let [n (first (filter #(= "Content-Type"
                                  (-> (attr-map %) :http-equiv))
                              ms))]
      (-> (attr-map n)
          :content
          (str/split #"=")
          last
          str/trim)
      nil)))

(defn replace-unicode-control
  [^String s]
  (when s
    (-> s
        (.replace \u2028 \u000a)
        (.replace \u2029 \u000a))))