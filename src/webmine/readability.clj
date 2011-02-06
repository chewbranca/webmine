(ns webmine.readability
  ^{:doc "Simple heuristics for selecting content divs. Uses
    a lot from python Reability port 
    (http://github.com/srid/readability/blob/master/src/readability.py).
    
    See comment form below for example sites that work 
    and one that doesn't. "
    :author "Aria Haghighi <me@aria42.com>"}
  (:use [infer.measures :only [sparse-dot-product]]
	[infer.core :only [max-by min-by]]
        [clojure.contrib.def :only [defvar-]])
  (:require [clojure.string :as clj-str]
            [webmine.parser :as parser])
  (:import [org.apache.xerces.dom ElementNSImpl]
	   [org.w3c.dom Node Element]))

(defvar- NEGATIVE  
  #"comment|meta|footer|navigation|footnote|foot"
  "Bad words for class or ids")  

(defvar- POSITIVE  
  #"article|post|hentry|entry|content|text|body|article"
  "Good words for class or ids")  
  
(defvar- PUNC  
  #"[!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~]"
  "Punctuation")  
  
(defvar- SHOULD-BE-PAR
  #"<br */? *>[ \r\n]*<br */? *>"
  "Should be a paragraph break")

(defvar- BASE-ELEM
  #"a|blockquote|dl|div|img|ol|p|pre|table|ul"
  "Base element")
  
(defn- to-binary [x]
  (if x 1.0 0.0))   
  
(defn- has-match? [s m]
  (try
    (re-seq s m)
    (catch Exception _ false)))  

; (defn is-text-div [n]
;   (let [children (parser/do-children n identity)]
;     (and children
;          (-> children count (= 1))
;          (-> children first parser/text-node?))))

(def ^:private word-re #"\w+")
(def ^:private comma-re #",")

(defn- div-feat-vec 
  "Features of the given div for readability"
  [^Element div]
  { :num-children-pars  ; how many immediate par children    
   (count 
    (filter      
     (fn [^Node c] 
       (try
	 (.equalsIgnoreCase (.getNodeName c) "p")
	 (catch Exception _ false)))        
     (parser/do-children div identity)))  
     :only-base-children 
   (to-binary
    (every?
     (fn [^Element  c]
       (or (parser/text-node? c)
	   (re-matches BASE-ELEM
		       (.getNodeName c))))
     (parser/do-children div identity)))
     :num-inner-divs
   (count
    (filter
     (fn [^Node c]
       (and (parser/element? c) (= (.getNodeName c) "div")))
     (parser/do-children div identity)))
     :good-class-word
   (->> div parser/attr-map :class (has-match? POSITIVE) to-binary)
     :good-id-word
   (->> div parser/attr-map :id (has-match? POSITIVE) to-binary)    
    :bad-class-word 
   (->> div parser/attr-map :class (has-match? NEGATIVE) to-binary)
    :bad-id-word 
   (->> div parser/attr-map :id (has-match? NEGATIVE) to-binary)    
    :long-text?
   (to-binary (> (-> div .getTextContent count) 10))
    :num-commas
   (->> div .getTextContent (re-seq comma-re) count)
   :total-words
   (->> div .getTextContent (re-seq word-re) count)     
   })
    ; :num-children-text-divs
    ;  (count
    ;    (filter is-text-div (parser/do-children div identity)))    
    ; :total-punc
    ;   (->> div .getTextContent (re-seq PUNC) count)    
            
(defvar- content-weight-vec 
  {:num-children-pars 100
   :only-base-children  10
   :num-inner-divs -1
   :long-text? 1.0
   :num-commas 1.0
   :good-class-word 25
   :good-id-word 25
   :bad-class-word -1000
   :num-words 0.01
   :bad-id-word -1000 }
   "Div Features set by hand from readability port")        

(defn- div-content-score [div]
   (let [dfv (div-feat-vec div)]
    (sparse-dot-product dfv content-weight-vec))) 
  
(defn ^Node find-best-content-div 
  "For the given dom root, what is the best DIV. Returns
   best DIV unaltered.  Return root if there are no divs."
  [root]
  (when-let [divs 
	     (filter (fn [^Node d]
		       (>= (-> d .getTextContent count) 140))
		     (parser/divs root))]
    (if (= 0 (count divs)) root
	(apply max-key div-content-score divs))))

(defn find-bad-divs [root]
         (filter
            (fn [div]
                (let [^String style (-> div parser/attr-map :style)]
                  (and style
                       (re-matches #"display:\s*none;" (.toLowerCase style)))))
            (parser/divs root)))
                ; Footers Bad
                ;; #_(or 
                ;;   (->> div parser/attr-map :class (has-match? #"footer"))
                ;;   (->> div parser/attr-map :id (has-match? #"footer")))
                ; Related
                ;; (or 
                ;;   (->> div parser/attr-map :class (has-match? #"related")))

;;TODO: do we need to look at p tags, and then look at the parent nodes of those p tags that ahve most p tags.

;;https://github.com/tjweir/arc90-readability/blob/master/js/readability-0.1-debug.js
;;https://gist.github.com/c87c071bddfe7ea9a945
;; // Replace all doubled-up <BR> tags with <P> tags :
;; var pattern = new RegExp ("<br/?>[ \r\n\s]*<br/?>", "g");
;; document.body.innerHTML = document.body.innerHTML.replace(pattern, "</p><p>");

(defn div-stats
  [div]
  (let [txt (parser/clean-text div)
	a (parser/elements div "a")]
    {:div div
     :p (count (parser/elements div "p"))
     :img (count (parser/elements div "img"))
     :li (count (parser/elements div "li"))
     :a (count a)
     :embed (count (parser/elements div "embed"))
     :commas (count (re-seq comma-re txt))
     :awords (count (re-seq word-re
			    (apply str
				   (map parser/text-from-dom a))))
     :words (count (re-seq word-re txt))}))
  
(defn strip-bads [root]
  (let [ds (parser/divs root)]
    (if (= 0 (count ds)) root
	(let [best-div 
	      (->> (cons root ds)
		   (map div-stats)
		   (filter
		    (fn [{:keys [p img li
				 a embed txt
				 commas words awords]}]
		      ;;(or
		      ;; (< commas 10)
		      (and 
		       ;; (> commas 5) 
		       (> words 0)
		       (> awords 0))))
		   ;; (<= words p) ;;(* 5 p))
		   ;; (<= commas p)
		   ;; (> img p)
		   ;; (> li p)
		   ;; (> a p)
		   ;; (> embed 0))))
		   ;;TODO: for li text too?
		   (min-by 
		    (fn [{:keys [words awords]}]
		      (float (/ awords words))))
		   :div)]
	  (if (not (= best-div root))
	    best-div
	    (parser/strip-from-dom root ds))))))

(defn strip-bad-divs! 
  "before finding-best-content-div use this to remove
  divs which might be bad that are likely inside the best div. 
  
  Returns modified root with bad divs removed"
  [root]
  (let [bad-divs (find-bad-divs root)]
    (parser/strip-from-dom root bad-divs)))

(defn readability-div
  "given a dom, returns the best div."
  [d]
  (-> d
      parser/strip-non-content
      strip-bad-divs!
      find-best-content-div
      strip-bads))

(defn- strip-tags [d tags]
  (apply d tags))

(defn extract-content
  "return the readability text from raw-html string"
  [raw-html]
  (parser/clean-text
   (readability-div (parser/dom raw-html))))

(defn format-plain-text-content
  "return plain text rendering of html dom element. should
   strip tags and replace <p> tags with newline"
  [dom]
  (-> dom
      ^String (parser/walk-dom
        (fn [^Node node]
	  (cond
	   (parser/element? node)
	   (cond (= (.getNodeName node) "p") "\n\n"
		 (= (.getNodeName node) "br") "\n"
		 :default nil)		 		  
	   (parser/text-node? node)
	   (.getNodeValue node)))
	(comp clj-str/join cons))
      (.replaceAll "\n{3,}" "\n\n")
      (.replaceAll "&nbsp;" "\t")))