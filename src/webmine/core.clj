(ns webmine.core)

;; From infer.core
(defn best-by [compare keyfn coll]
  (if (empty? coll) nil
      (let [best-finder (fn [best next-elem]
		    (if (compare (keyfn best) (keyfn next-elem))
		      best
		      next-elem))]
	(reduce best-finder coll))))

;; From infer.core
(defn max-by [keyfn coll]
  (best-by > keyfn coll))

;; From infer.core
(defn min-by [keyfn coll]
  (best-by < keyfn coll))

;; From l.core
(defn min-length [us]
  (min-by (comp count str) us))