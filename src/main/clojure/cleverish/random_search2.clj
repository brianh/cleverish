(ns cleverish.random-search2
  (:use cleverish.utils))

;;;;
;;;; A solution to the random search algorithm using just functions.
;;;;
;;;; see-> http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
;;;;

(defn random-vec-generator [n rand-gen]
  "Generates a random vector of length n utilizing
   the provided random number generator."
  (repeatedly #(into [] (take n (repeatedly rand-gen)))))
  
(def soln-seq (random-vec-generator 2 (bounded-rand-gen rand -5 5)))

(defn solve [n]
  (reduce minimizer (take n soln-seq)))


;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/random_search2.clj")
  (in-ns 'cleverish.random-search2)
  )