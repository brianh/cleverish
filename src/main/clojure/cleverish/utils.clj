 (ns cleverish.utils)
;;
;; Maybe utility functions...???
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/utils.clj")
  (in-ns 'cleverish.utils)
  )

(defn bounded-rand-generator [rand-fn min max]
  (let [xspan (- max min)]
    (fn []
      (+ min (rand-fn xspan)))))

;; function that generates another function that determines which solution is better
(defn soln-comparer-gen [compare-fn cost-fn]
  (fn [s1 s2]
    (if (compare-fn (cost-fn s1) (cost-fn s2))
      s1
      s2)))

;; give it a pretty name...
(defn solve [test-fn solutions]
  (reduce test-fn solutions))