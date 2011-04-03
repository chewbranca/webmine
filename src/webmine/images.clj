(ns webmine.images
  (:use [infer.core :only [max-by]]
        webmine.urls
        webmine.readability
        html-parse.parser
	[clojure.contrib.profile :only [prof]]
	[fetcher.client :only [request]]
	plumbing.error
	plumbing.core)
  (:require [work.core :as work])
  (:import [org.w3c.dom Node Attr])
  (:import javax.imageio.ImageIO)
  (:import [java.awt.image BufferedImage])
  (:import java.awt.image.ConvolveOp)
  (:import java.awt.image.Kernel)
  (:import java.awt.RenderingHints))


;;http://www.mkyong.com/regular-expressions/10-java-regular-expression-examples-you-should-know/
;; (defn imgs [t]
;;   (re-seq #"([^\\s]+(\\.(?i)(jpg|png|gif|bmp))$)" t))
;;"([^\s]+(\.(?i)(jpg|png|gif|bmp))$)" t))

;; (defn ints [xs]
;;   (map #(Integer/parseInt %) xs))

(defn hw? [h w]
  (and h w
       (not (= "" h))
       (not (= "" w))))

(defn extract-dim [^String d]
  (Integer/parseInt
   (.replaceAll d "[^0-9]" "")))

(defn to-hw [h w] 
  (if (hw? h w)
    {:width (extract-dim w)
     :height (extract-dim h)}
    nil))

(defn hw-from-str
  [^String s]
  (let [wi (.indexOf s "width")
	hi (.indexOf s "height")]
    (if (or (= -1 wi)
	    (= -1 hi))
      nil
      (let [[a b] (re-seq #"[0-9]+" s)]
	(if (< hi wi) (to-hw a b) (to-hw b a))))))

(defn hw-from-style
  "in case hight and width are burried inside inline style tag."
  [^Attr st]
  (hw-from-str (.getValue st)))

(defn hw-from-tags [^Attr h ^Attr w]
  (to-hw (.getValue h)
	 (.getValue w)))

(defn img? [^String u]
  (or (.contains u ".jpg")
      (.contains u ".png")
      (.contains u ".gif")
      (.contains u ".bmp")))

(defn img-urls [us] (filter img? us))

;;http://www.w3schools.com/tags/tag_IMG.asp
(defn size [^Node n]
  (if-let [attrs (.getAttributes n)]
    (let [w (.getNamedItem attrs "width")
	  h (.getNamedItem attrs "height")
	  st (.getNamedItem attrs "style")]
      (cond
       (and w h) (hw-from-tags h w)
       st (hw-from-style st)
       :else nil))))

(defn imgs [d] (map (fn [n]
		      {:url (src n)
		       :size (size n)})
		    (elements d "img")))

(defn img-area [i]
  (let [s (:size i)]
  (* (:width s) (:height s))))

(defn big-img
[images]
  ;;discard images with no size information.
  (let [is (filter :size images)]
	(max-by img-area is)))

;;example usage
;;(big-img (imgs (dom (:body (cl/get "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/")))))

(defn read-img [u]
  (try (ImageIO/read u)
       (catch java.lang.Exception _ nil)))

(defn ^BufferedImage
  fetch-img [u]
  (if-let [ur (url u)]
    (read-img ur)))

(defn img-size [u]
  (when-let [i (prof :fetch-img (fetch-img u))]
    {:width (.getWidth i)
     :height (.getHeight i)}))

(defn img-content-size [^String u]
  (or (-?> (request :head (.replaceAll u " " "%20"))
	   :headers
	   (get "content-length")
	   Integer/parseInt)
      0))

(defn fetch-sizes [imgs]
  (map 
   (fn [i]
     (if-let [s (:size i)]
       i
       (let [s (img-size (:url i))]
	 (assoc i :size s))))     
   imgs))

(defn fetch-content-sizes [imgs]
  (work/map-work
   (fn [i]     
     (if  (:content-size i) 
       i
       (assoc i :content-size (img-content-size (:url i)))))
   (min (count imgs) 100)
   imgs))

;;example usage
;;(big-img (fetch-sizes (imgs (dom (:body (cl/get "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/"))))))

(defn extract-all [d]
  (flatten
   (map
    #(flatten (map (fn [t] (do-children t identity))
		   (do-children % identity)))
    (divs d))))

(defn big-div [d]
  (max-by (comp count :textContent bean) (extract-all d)))

(defn at-least [min imgs]
  (filter #(> (img-area %) min) imgs))

;; TODO: better way of dealing with min size.  should really be composed more like classifiers, but let's leave it flat and lame until we figure out more about what we really need to do.

(defn core-imgs [u] 
  (imgs (readability-div (dom (:body (request :get u))))))

(defn all-imgs [u] 
  (imgs (dom (:body (request :get u)))))

(defn best-img
  ([u] (best-img u nil))
  ([u opts] (best-img u (slurp u) opts))
  ([u content {:keys [min, height-backoff] :or {min 0 height-backoff 0}}]
     (let [d (dom content)	   
	   core-imgs (loop [n (readability-div d) count 0]
		       (let [node-imgs (imgs n)]
			 (if (or (>= count height-backoff)
				 (not (empty? node-imgs)))
			   node-imgs
			   (recur (.getParentNode n) (inc count)))))]
       (when-not (empty? core-imgs)
	 (let [eis (expand-relative-urls u core-imgs)
	       ;;ensure we have sizes for all images.
	       sized (fetch-content-sizes eis)
	       sizes (if min (filter (fn [i] (>= (:content-size i) min)) sized) sized)]
	   (when-not (empty? sizes) 
	     ;;take the first image we find that has no follow image that is larger than twice it's size.
	     (when-let [best (reduce (fn [best next]
				       (if (> (:content-size next)
					      (* (:content-size best) 2))
					 next
					 best))
				     sizes)]
	       (assoc best
		 :size (img-size (:url best))))))))))

(defn with-best-img [m url-key content-key & {:as opts}]
  (let [img (try
	      (best-img (url-key m)
			(content-key m)
			opts)
	      (catch java.lang.Exception _ nil))]
  (assoc m :img img)))

;;RESIZING & CROPPING
;;http://www.componenthouse.com/article-20
;;http://today.java.net/pub/a/today/2007/04/03/perils-of-image-getscaledinstance.html
;;http://www.dreamincode.net/forums/topic/162774-java-image-manipulation-part-2-resizing/
;;http://www.java-tips.org/java-se-tips/javax.imageio/java-2d-api-enhancements-in-j2se-5.0.html

(def hints
     {:bilinear RenderingHints/VALUE_INTERPOLATION_BILINEAR
      :bicubic RenderingHints/VALUE_INTERPOLATION_BICUBIC})

(defn resize [^BufferedImage i h w & [hint]]
  (let [hint ((or hint :bilinear)
	      hints)
	old-w (.getWidth i nil)
	old-h (.getHeight i nil)
	bi (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
	bg (.createGraphics bi)]
    (.setRenderingHint bg
		       RenderingHints/KEY_INTERPOLATION
		       hint)
    (.scale bg (/ w old-w) (/ h old-h))
    (.drawImage bg i 0 0 nil)
    (.dispose bg)
    bi))

(defn scale [^BufferedImage i h-percent w-percent & [hint]]
  (let [old-w (.getWidth i nil)
	old-h (.getHeight i nil)]
  (resize i (* old-h h-percent) (* old-w w-percent) hint)))

(defn scale-dim [dim new-dim old-dim]
  (* dim (/ new-dim old-dim)))

(defn scale-to
"takes an image and either hight or width.
returns the scaled image, retaining aspect ratio."
[^BufferedImage i {h :height w :width} & [hint]]
  (let [old-w (.getWidth i nil)
	old-h (.getHeight i nil)
	height (or h (scale-dim old-h w old-w))
	width (or w (scale-dim old-w h old-h))]
  (resize i height width hint)))

;;TODO: get into a stream for the server API.
(defn save-img [^BufferedImage image filename ext]
  (let [f (java.io.File. (str filename "." ext))]
    (try
     (ImageIO/write image ext f)
     (catch java.lang.Exception e 
       (println (.printStackTrace e))))))

;;http://www.exampledepot.com/egs/java.awt.image/Sharpen.html

;;Filters
;;http://www.jhlabs.com/ip/filters/index.html

;;sharpen
;;contrast (might not work well for screenshots)
;;white balance
;;white color balance
(defn kernel [^BufferedImage image]
  (let [kernel (ConvolveOp. (Kernel. 3, 3,
			(float-array [-1, -1, -1, -1, 9, -1, -1, -1, -1])))]
    (.filter kernel image nil)))

(comment
  (best-img-at "http://channel9.msdn.com/posts/DC2010T0100-Keynote-Rx-curing-your-asynchronous-programming-blues")

)