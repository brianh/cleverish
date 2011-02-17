(ns cleverish.utils)

;;;;
;;;; Maybe utility functions...???
;;;;

(defn bounded-rand-gen [rand-fn min max]
  "Produces a no arg function that, when called, generates
   a random number between min & max:  [min, max)"
  (let [xspan (- max min)]
    (fn []
      (+ min (rand-fn xspan)))))

(defn coster-gen [compare-fn cost-fn]
  "Function that generates another function that determines
   which solution is better using the provide comparison and
   cost functions."
  (fn [s1 s2]
    (if (compare-fn (cost-fn s1) (cost-fn s2))
      s1
      s2)))

(defn sum-squares [s]
  "Squares the elements of the provided sequence and then
   sums them."
  (reduce + (map #(Math/pow % 2.0) s)))

(def minimizer (coster-gen < sum-squares))
(def maximizer (coster-gen > sum-squares))

(defprotocol Costable
  (calc-cost [this] "Calculates the cost."))

;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/utils.clj")
  (in-ns 'cleverish.utils)
  )