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

;; function that generates another function that determines which solution is better
(defn test-gen [compare-fn cost-fn]
  (fn [s1 s2]
    (if (compare-fn (cost-fn s1) (cost-fn s2))
      s1
      s2)))


;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/utils.clj")
  (in-ns 'cleverish.utils)
  )