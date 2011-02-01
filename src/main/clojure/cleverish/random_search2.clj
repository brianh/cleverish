(ns cleverish.random-search2
  (:use cleverish.utils))
;;
;; A solution to the random search algorithm using just functions.
;;
;; see-> http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/random_search2.clj")
  (in-ns 'cleverish.random-search2)
  )

(def r-gen (bounded-rand-generator rand -5 5))
;(def r-gen (bounded-rand-generator rand-int -5 5))

;; capturing in a var keeps it repeatable within a compilation lifespan
;; the down side is we're holding onto the head and could blow the stack...
(def vec-soln-seq (repeatedly #(into [] (take 2 (repeatedly r-gen)))))

(defn sum-squares [v]
  (reduce + (map #(Math/pow % 2.0) v)))

(comment
  (solve (soln-comparer-gen < sum-squares) (take 4 vec-soln-seq))
  ;;[-0.5048769252134813 1.7657004782115049]
  
  (solve (soln-comparer-gen > sum-squares) (take 400 vec-soln-seq))
  ;;[-4.7871174338069435 4.798260204713685]

  (solve (soln-comparer-gen > sum-squares) (take 4000 vec-soln-seq))
  ;;[4.876171696478503 4.967191439581324]
  
  ;;test
  (solve soln-comparer [[3 4] [2 5] [8 1] [1 2] [1 4]])
  ;; [1 2]
  (solve (soln-comparer-gen < sum-squares) [[3 4] [2 5] [8 1] [1 2] [1 4]])
  ;; [1 2]
  (solve (soln-comparer-gen > sum-squares) [[3 4] [2 5] [8 1] [1 2] [1 4]])
  ;;[8 1]
  )