(ns webmine.images
  (:use webmine.urls
        webmine.readability
        html-parse.parser
	[fetcher.core :only [fetch]]
	plumbing.error
	plumbing.core)
  (:require [work.core :as work])
  (:import [org.w3c.dom Node Attr Element])
  (:import javax.imageio.ImageIO)
  (:import [java.awt.image BufferedImage])
  (:import java.awt.image.ConvolveOp)
  (:import java.awt.image.Kernel)
  (:import java.awt.RenderingHints))

(defn expand-relative-imgs [url d]
  (let [host (host-url url)
	expand (fn [^Element e]
		 (let [^String  src (.getAttribute e "src")]
		   (if (.startsWith src "/")
		     (.setAttribute e "src" (str host src)))))]
    (doseq [e (elements d "img")]
      (expand e))
    d))

(defn extract-dim [^String d]
  (Integer/parseInt
   (.replaceAll d "[^0-9]" "")))

(defn to-hw [h w] 
  (if (and h w
       (not (= "" h))
       (not (= "" w)))
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

;;http://www.w3schools.com/tags/tag_IMG.asp
(defn size [^Node n]
  (if-let [attrs (.getAttributes n)]
    (let [^Attr w (.getNamedItem attrs "width")
	  ^Attr h (.getNamedItem attrs "height")
	  ^Attr st (.getNamedItem attrs "style")]
      (cond
       (and w h) (to-hw (.getValue h)
			(.getValue w))
       st (hw-from-str (.getValue st))
       :else nil))))

(defn img-size [u]
  (when-let [^BufferedImage i (ImageIO/read ^java.net.URL (url u))]
    {:width (.getWidth i)
     :height (.getHeight i)}))

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
(defn save-img [^BufferedImage image ^String filename ^String ext]
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