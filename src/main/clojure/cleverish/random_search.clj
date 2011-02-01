(ns cleverish.random-search
  (:use cleverish.utils))
;;
;; A solution to the random search algorithm using protocols & records
;;
;; see-> http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/random_search.clj")
  (in-ns 'cleverish.random-search)
  )

(def r-gen (bounded-rand-generator rand -5 5))
;(def r-gen (bounded-rand-generator rand-int -5 5))

;; think of this kind of like an interface
(defprotocol Costable
  (calc-cost [this] "Calculates the cost."))

(defrecord Point2d [x y]
  Costable
  (calc-cost [this]
	     (+ (Math/pow (:x this) 2.0)
		(Math/pow (:y this) 2.0))))

;; capturing in a var keeps it repeatable within a compilation lifespan
;; the down side is we're holding onto the head and could blow the stack...
(def pt-soln-seq (map #(Point2d. %1 %2) (repeatedly r-gen) (repeatedly r-gen)))

(defn rand-str-builder [n]
  (let [s-gen (bounded-rand-generator rand-int 0 26)
	alphabet "abcdefghijklmnopqrstuvwxyz"]
    (fn []
      (apply str (take 4 (map #(nth alphabet %) (repeatedly s-gen)))))))
    
(def string-soln-seq (repeatedly (rand-str-builder 4)))
			   
(extend-protocol Costable
  java.lang.String
  (calc-cost [this]
	     (reduce + (map #(Character/getNumericValue %) this))))

(comment
  (solve (soln-comparer-gen < calc-cost)  (take 5 pt-soln-seq))
  ;;#:cleverish.random-search.Point2d{:x -0.06519004969610087, :y -1.2873767681677495}

  (solve (soln-comparer-gen < calc-cost)  (take 15000 string-soln-seq))
  ;;"bcca"
  
  (solve (soln-comparer-gen < calc-cost) (take 10 pt-soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 1.090818924123786, :y 0.3794868451930693}
  
  (solve (soln-comparer-gen < calc-cost) (take 20 pt-soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 1.090818924123786, :y 0.3794868451930693}

  (solve (soln-comparer-gen < calc-cost) (take 50 pt-soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 0.4239935816500262, :y -0.06718153034240704}

  (solve (soln-comparer-gen < calc-cost) (take 100 pt-soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 0.4239935816500262, :y -0.06718153034240704}

  (solve (soln-comparer-gen < calc-cost) (take 5000 pt-soln-seq))
  ;;#:cleverish.random-search.Point2d{:x 0.10164701076605986, :y -0.12280458345470535}

  (solve (soln-comparer-gen < calc-cost)  (take 50 pt-soln-seq))
  ;;{:x 0.44875836186140994, :y -0.5658904863123206}
  
  ;;test
  (solve (soln-comparer-gen < calc-cost) (seq [(Point2d. 1 2) (Point2d. 3 -4) (Point2d. 0 1)]))
  ;;#:cleverish.random-search.Point2d{:x 0, :y 1}

  (solve (soln-comparer-gen < calc-cost) (seq [(Point2d. 1 2) (Point2d. 3 -4) (Point2d. 10 1)]))
  ;;#:cleverish.random-search.Point2d{:x 1, :y 2}
  )

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