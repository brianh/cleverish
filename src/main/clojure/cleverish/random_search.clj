(ns cleverish.random-search
  (:use cleverish.utils))

;;;;
;;;; A solution to the random search algorithm using protocols & records
;;;;
;;;; see-> http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
;;;;

(defprotocol Costable
  (calc-cost [this] "Calculates the cost."))

(defrecord Point2d [x y]
  Costable
  (calc-cost [this]
	     (+ (Math/pow (:x this) 2.0)
		(Math/pow (:y this) 2.0))))

(def r-gen (bounded-rand-gen rand -5 5))
(def minimizer (test-gen < calc-cost))
(def maximizer (test-gen > calc-cost))
(def pt-soln-seq (map #(Point2d. %1 %2) (repeatedly r-gen) (repeatedly r-gen)))

(defn solve [test-fn solutions]
  (reduce test-fn solutions))

;;;;
;;;; that's all folks....
;;;;


;;
;; random length java String coster using same protocol
(extend-protocol Costable
  java.lang.String
  (calc-cost [this]
	     "Simply sums each characters numeric value for the
              entire string."
	     (reduce + (map #(Character/getNumericValue %) this))))

(defn rand-str-gen [n]
  (let [a "abcdefghijklmnopqrstuvwxyz"
	str-gen (bounded-rand-gen rand-int 0 (count a))]
    (fn []
      (apply str (take n (map #(nth a %) (repeatedly str-gen)))))))
    
(def string-soln-seq (repeatedly (rand-str-gen 4)))


;;
;; another more verbose way
(defn rand-solution []
  (Point2d. (r-gen) (r-gen)))

(defn another-solver [n test-fn soln-producer]
  (loop [cnt n
	 best (soln-producer)
	 new-sol (soln-producer)]
    (if (zero? cnt)
      best
      (recur (dec cnt) (test-fn best new-sol) (soln-producer)))))


;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/random_search.clj")
  (in-ns 'cleverish.random-search)
  )