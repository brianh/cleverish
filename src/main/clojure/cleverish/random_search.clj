(ns cleverish.random-search)
;;
;; A solution to the random search algorithm using protocols & records
;;
;; see-> http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/random_search.clj")
  (in-ns 'cleverish.random-search)
  )

(defn bounded-rand-generator [rand-fn min max]
  (let [xspan (- max min)]
    (fn []
      (+ min (rand-fn xspan)))))

(def r-gen (bounded-rand-generator rand -5 5))
;(def r-gen (bounded-rand-generator rand-int -5 5))

(defprotocol Costable
  (calc-cost [this] "Calculates the cost."))

(defrecord Point2d [x y]
  Costable
  (calc-cost [this]
	     (+ (Math/pow (:x this) 2.0)
		(Math/pow (:y this) 2.0))))

(defn rand-solution []
  (Point2d. (r-gen) (r-gen)))

;; capturing in a var keeps it repeatable within a compilation lifespan
;; the down side is we're holding onto the head and could blow the stack...
(def soln-seq (map #(Point2d. %1 %2) (repeatedly r-gen) (repeatedly r-gen)))

(defn minimizer [s1 s2]
  (if (< (calc-cost s1) (calc-cost s2))
    s1
    s2))

;; give it a pretty name...
(defn solve [test-fn solutions]
  (reduce test-fn solutions))

(comment

  (solve  minimizer (take 5 soln-seq))
  ;;#:cleverish.random-search.Point2d{:x -0.06519004969610087, :y -1.2873767681677495}
  
  (solve minimizer (take 10 soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 1.090818924123786, :y 0.3794868451930693}
  
  (solve minimizer (take 20 soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 1.090818924123786, :y 0.3794868451930693}

  (solve minimizer (take 50 soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 0.4239935816500262, :y -0.06718153034240704}

  (solve minimizer (take 100 soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 0.4239935816500262, :y -0.06718153034240704}

  (solve minimizer (take 5000 soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 0.10164701076605986, :y -0.12280458345470535}

  ;;test
  (solve minimizer (seq [(Point2d. 1 2) (Point2d. 3 -4) (Point2d. 0 1)]))
  ;;#:cleverish.random-search.Point2d{:x 0, :y 1}

  (solve minimizer (seq [(Point2d. 1 2) (Point2d. 3 -4) (Point2d. 10 1)]))
  ;;#:cleverish.random-search.Point2d{:x 1, :y 2}
  
  (defrecord Candidate [value cost]
    Costable
    (calc-cost [this]
	       (:cost this)))
  )

;; another more verbose way
(defn find-solution-dumb [n test-fn soln-producer]
  (loop [cnt n
	 best (soln-producer)
	 new-sol (soln-producer)]
    (if (zero? cnt)
      best
      (recur (dec cnt) (test-fn best new-sol) (soln-producer)))))