(ns cleverish.iterated-local-search
  (:use cleverish.utils))

;;;;
;;;; Another way... using agents
;;;;
(def berlin52 [[565,575],[25,185],[345,750],[945,685],[845,655],
	       [880,660],[25,230],[525,1000],[580,1175],[650,1130],[1605,620],
	       [1220,580],[1465,200],[1530,5],[845,680],[725,370],[145,665],
	       [415,635],[510,875],[560,365],[300,465],[520,585],[480,415],
	       [835,625],[975,580],[1215,245],[1320,315],[1250,400],[660,180],
	       [410,250],[420,555],[575,665],[1150,1160],[700,580],[685,595],
	       [685,610],[770,610],[795,645],[720,635],[760,650],[475,960],
	       [95,260],[875,920],[700,500],[555,815],[830,485],[1170,65],
	       [830,610],[605,625],[595,360],[1340,725],[1740,245]])

(def max-no-improvement 50)

(defprotocol Metrizable
  (dist [this that]))

(extend-protocol Metrizable
  clojure.lang.IPersistentVector
  (dist [this that]
	(Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2) this that)))))

(defrecord Path [p]
  Costable
  (calc-cost [this]
	     (+ (reduce + (map dist (:p this) (rest (:p this))))
		(dist (first (:p this)) (last (:p this))))))

(def soln (atom (Path. (shuffle berlin52))))
(def soln-size (count (:p @soln)))

(defn unique-rand-ints [r]
  "Constucts a vector of two unique random ints within
   the provided range"
  (let [r1 (rand-int r)]
    [r1 (first (filter (partial not= r1)
		       (repeatedly #(rand-int r))))]))

(defn unique-rand-int [r]
  "Toy.  In reality, you're better off randomizing a vector of natural
   numbers up to the size you want...

  Constucts a function that will return a unique random int within
   the provided range until the range is exhausted (at which time it
   will start over with a new random value)."
  (let [s (atom #{})]
    (fn []
      (if (>= (count @s) r)
	(swap! s (constantly #{})))
      (let [result (first (filter (complement @s) (repeatedly #(rand-int r))))]
	(swap! s conj result)
	result))))

(defn swap-two-rand [v]
  "Swaps two random vector slots."
  (let [[r1 r2] (unique-rand-ints (count v))
	val1 (nth v r1)
	val2 (nth v r2)]
    (-> v
	(assoc r1 val2)
	(assoc r2 val1))))		
  
(defn local-search []
  (let [cnt (atom 0)]
    (while (< cnt max-no-improvement)
      (let [soln @soln
	    soln-cost (calc-cost soln)
	    new-soln (swap-two-rand soln)
	    new-soln-cost (calc-cost new-soln)]
	(if (< new-soln-cost soln-cost)
	  (do (swap! soln (constantly new-soln))
	      (swap! cnt (constantly 0)))
	  (swap! cnt inc))))))

	   
;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/iterated_local_search.clj")
  (in-ns 'cleverish.iterated-local-search)
  )