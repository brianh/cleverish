(ns cleverish.hill-climbing-search
  (:refer-clojure :exclude [bit-or bit-and bit-flip])
  (:use cleverish.utils))

(defprotocol BitOps
  (bit-or [this a])
  (bit-and [this a])
  (bit-flip [this n]))

(extend-protocol BitOps
  clojure.lang.IPersistentVector
  (bit-or [this a]
	  (map clojure.core/bit-or this a))
  (bit-and [this a]
	   (map clojure.core/bit-and this a))
  (bit-flip [this n]
	    (let [new-bit (if (zero? (nth this n)) 1 0)]
	      (assoc this n new-bit)))
  java.lang.Number
  (bit-or [this a]
	  (clojure.core/bit-or this a ))
  (bit-and [this a]
	   (clojure.core/bit-and this a))
  (bit-flip [this n]
	    (clojure.core/bit-flip this n)))

(extend-protocol Costable
  clojure.lang.IPersistentVector
  (calc-cost [this]
	     (reduce + this)))

(def num-bits 64)

(def soln (atom (into [] (take num-bits (repeatedly #(rand-int 2))))))
(def soln-set (atom [@soln]))
(def neighbors (repeatedly #(rand-int num-bits)))

(defn solve [max-iter]
  (into []
	(filter identity
		(for [n (take max-iter neighbors) :while (< (calc-cost @soln) num-bits)]
		  (let [the-soln @soln
			new-soln (bit-flip the-soln n)]
		    (if (> (calc-cost new-soln) (calc-cost the-soln))
		      (swap! soln (constantly new-soln))))))))

;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/hill_climbing_search.clj")
  (in-ns 'cleverish.hill-climbing-search)
  )