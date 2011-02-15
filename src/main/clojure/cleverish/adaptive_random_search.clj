(ns cleverish.adaptive-random-search
  (:use cleverish.random-search2)
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

(defn rand-vec [space-bounds]
	(into [] (apply map #(rand-in-bounds %1 %2) space-bounds)))

;; hmm this needs cleanup
(defn take-step [space-bounds cur-loc step-size]
	    (into [] (map #(let [[d-min d-max] %1
			    new-min (max d-min (- %2 step-size))
			    new-max (min d-max (+ %2 step-size))]
			    (rand-in-bounds new-min new-max)) space-bounds cur-loc)))

;; yes, java lovers, clojure treats commas as whitespace & ignores them
(defn large-step-size [iter, step_size, s_factor, l_factor, iter_mult]
  (if (and (pos? iter) (zero? (mod iter iter_mult)))
    (* step_size l_factor)
    (* step_size s_factor)))
    

;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/adaptive_random_search.clj")
  (in-ns 'cleverish.adaptive-random-search)
  )