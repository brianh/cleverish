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
(def soln-size (count @soln))

(defn stochastic-two-opt []
  (let [p1 (inc (rand-int (dec soln-size)))
	p2  (rand-int soln-size)]
    (flatten [(subvec v 0 3) (nth v 6) (subvec v 4 6) (nth v 3) (subvec v 7 9)])))
		
  
(defn local-search []
  (let [cnt (atom 0)]
    (while (< cnt max-no-improvement)
      (let [soln @soln
	    soln-cost (calc-cost soln)
	    new-soln (stochastic-two-opt)
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