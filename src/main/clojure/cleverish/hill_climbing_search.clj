(ns cleverish.hill-climbing-search
  (:refer-clojure :exclude [bit-or bit-and bit-flip])
  (:use cleverish.utils))

(defprotocol BitOps
  (bit-or [this a])
  (bit-and [this a])
  (bit-flip [this n])
  (len [this]))

(extend-protocol BitOps
  clojure.lang.IPersistentVector
  (bit-or [this a]
	  (map clojure.core/bit-or this a))
  (bit-and [this a]
	   (map clojure.core/bit-and this a))
  (bit-flip [this n]
	    (let [new-bit (if (zero? (nth this n)) 1 0)]
	      (assoc this n new-bit)))
  (len [this]
       (count this))
  
  java.lang.String
  (bit-or [this a]
	  (apply str (map (fn [s1 s2]
	       (if (and (= s1 \1) (not= s1 s2))
		   \1
		   \0)))))
  (bit-and [this a]
	   (apply str (map (fn [s1 s2]
	       (if (and (= s1 \1) (= s1 s2))
		   \1
		   \0)) this a)))
  (bit-flip [this n]
	    (apply str (map (fn [cnt c]
	       (if (= n cnt)
		   (if (= c \1)
		       \0
		       \1)
		   c)) (iterate inc 0) this)))
  (len [this]
       (count this)))

(extend-protocol Costable
  clojure.lang.IPersistentVector
  (calc-cost [this]
	     (reduce + this))
  java.lang.String
  (calc-cost [this]
	     (reduce + (map #(if (= \1 %) 1 0) this))))

(def soln (atom "10010101001011110110010101101010010101"))
;(def soln (atom (into [] (take num-bits (repeatedly #(rand-int 2))))))
(defn neighbors [num-bits]
  (repeatedly #(rand-int num-bits)))

(defn solve [max-iter]
  (let [a-len (len @soln)]
    (into []
	  (filter identity
		  (for [n (take max-iter (neighbors a-len)) :while (< (calc-cost @soln) a-len)]
			(let [the-soln @soln
			      new-soln (bit-flip the-soln n)]
			  (if (> (calc-cost new-soln) (calc-cost the-soln))
			    (swap! soln (constantly new-soln)))))))))

;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/hill_climbing_search.clj")
  (in-ns 'cleverish.hill-climbing-search)
  )