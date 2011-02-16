(ns cleverish.adaptive-random-search
  (:use cleverish.utils))

;;;;
;;;; First try following closely with the author's code.
;;;;

(def small-factor 1.3)
(def large-factor 3.0)
(def max-no-improvement 30)
(def iter-mult 10)

(defn rand-in-bounds [min max]
  "Produce a random number between min & max."
  (+ min (* (- max min) (rand))))

(defn rand-vec [soln-bounds]
  (into [] (apply map #(rand-in-bounds %1 %2) soln-bounds)))

;; hmm this needs cleanup
(defn take-step [soln-bounds cur-soln step-size]
  (into [] (map #(let [[d-min d-max] %1
		       new-min (max d-min (- %2 step-size))
		       new-max (min d-max (+ %2 step-size))]
		   (rand-in-bounds new-min new-max)) soln-bounds cur-soln)))

;; yes, java lovers, clojure treats commas as whitespace & ignores them
(defn large-step-size [iter, step-size, s-factor, l-factor, iter-mult]
  (if (and (pos? iter) (zero? (mod iter iter-mult)))
    (* step-size l-factor)
    (* step-size s-factor)))
    
(defn take-steps [soln-bounds cur-soln step-size big-step-size]
  "Returns a vector of two new steps: [step big-step]"
  [(take-step soln-bounds cur-soln step-size)
   (take-step soln-bounds cur-soln big-step-size)])

;; this is Fugly!  I feel dirty...
(defn recursive-search [max-iter soln-bounds init-factor s-factor l-factor iter-mult max-no-impr cost-fn]
  (loop [iter 0
	 no-impr-cnt 0
	 step-size (* (- (second (first soln-bounds)) (first (first soln-bounds))) init-factor)
	 cur-soln (rand-vec soln-bounds)]
    (let [big-step-size (large-step-size iter step-size s-factor l-factor iter-mult)
	  [new-step new-large-step] (take-steps soln-bounds cur-soln step-size big-step-size)]
      (if (> iter max-iter)
	cur-soln
	(let [is-better (or (<= (cost-fn new-step) (cost-fn cur-soln))
			    (<= (cost-fn new-large-step) (cost-fn cur-soln)))
	      [count new-step-size new-soln] (if is-better
					       (if (<= (cost-fn new-large-step) (cost-fn new-step))
						 [0 big-step-size new-large-step]
						 [0 step-size new-step])
					       (if (>= no-impr-cnt max-no-impr)
						 [0 (/ step-size s-factor) cur-soln]
						 [(inc no-impr-cnt) step-size cur-soln]))]
	  (recur (inc iter) count new-step-size new-soln))))))

;; not much better
(defn recursive-search2 [max-iter soln-bounds init-factor s-factor l-factor iter-mult max-no-impr cost-fn]
  (loop [iter 0
	 no-impr-cnt 0
	 step-size (* (- (second (first soln-bounds)) (first (first soln-bounds))) init-factor)
	 cur-soln (rand-vec soln-bounds)]
    (let [big-step-size (large-step-size iter step-size s-factor l-factor iter-mult)
	  [new-step new-large-step] (take-steps soln-bounds cur-soln step-size big-step-size)]
      (if (> iter max-iter)
	cur-soln
	(if (or (<= (cost-fn new-step) (cost-fn cur-soln))
		(<= (cost-fn new-large-step) (cost-fn cur-soln)))
	  (if (<= (cost-fn new-large-step) (cost-fn new-step))
	    (recur (inc iter) 0 big-step-size new-large-step)
	    (recur (inc iter) 0 step-size cur-soln))
	  (recur (inc iter) (if (>= no-impr-cnt max-no-impr) 0 (inc no-impr-cnt))
		 (if (>= no-impr-cnt max-no-impr) (/ step-size s-factor) step-size) cur-soln))))))



;   (recursive-search 1000 [[-5 5] [-5 5]] 0.05 small-factor large-factor iter-mult max-no-improvement sum-squares))

;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/adaptive_random_search.clj")
  (in-ns 'cleverish.adaptive-random-search)
  )