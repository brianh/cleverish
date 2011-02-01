(ns cleverish.random-search2
  (:use cleverish.utils))

;;;;
;;;; A solution to the random search algorithm using just functions.
;;;;
;;;; see-> http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
;;;;

(defn sum-squares [v]
  (reduce + (map #(Math/pow % 2.0) v)))

(def r-gen (bounded-rand-gen rand -5 5))
(def minimizer (test-gen < sum-squares))
(def maximizer (test-gen > sum-squares))
(def vec-soln-seq (repeatedly #(into [] (take 2 (repeatedly r-gen)))))

(defn solve [test-fn solutions]
  (reduce test-fn solutions))


;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/random_search2.clj")
  (in-ns 'cleverish.random-search2)
  )