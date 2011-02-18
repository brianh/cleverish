(ns cleverish.adaptive-random-search2
  (:use cleverish.utils))

;;;;
;;;; Another way... using agents
;;;;

(def small-factor 1.3)
(def large-factor 3.0)
(def max-no-improvement 30)
(def iter-mult 10)
(def init-factor 0.05)
(def solution-space [[-5 5] [-5 5]])
(def iter (atom 0))
(def step-size (agent (* (- (second (first solution-space)) (first (first solution-space))) init-factor)))
(def no-chg-cnt (agent 0 :validator #(< % max-no-improvement)
		       :error-handler (fn [] (send step-size #(/ % small-factor)))))

(defn rand-in-range [min max]
  "Produce a random number between min & max."
  (+ min (* (- max min) (rand))))

(defn rand-vec []
  (into [] (map (fn [[min max]] (rand-in-range min max)) solution-space)))

(defn make-soln [v]
  {:vec v :cost (sum-squares v)})

(def the-soln (agent (make-soln (rand-vec))
;		     :validator #(do (prn @the-soln) (<= (:cost %) (:cost @the-soln)))
		     :error-mode :continue))

(defn take-step [size]
  (into [] (map #(let [[d-min d-max] %1
		       new-min (max d-min (- %2 size))
		       new-max (min d-max (+ %2 size))]
		   (rand-in-range new-min new-max)) solution-space (:vec @the-soln))))

(defn large-step-size []
  (let [i @iter]
    (if (and (pos? i) (zero? (mod i iter-mult)))
      (* @step-size large-factor)
      (* @step-size small-factor))))

(defn take-steps []
  "Takes two steps in the solution space & sends the winner to the soln.  If the
   bigger step is the winner, adjusts the current step size to it."
  (let [big-step-size (large-step-size)
	new-step (make-soln (take-step @step-size))
	big-step (make-soln (take-step big-step-size))]
    (if (< (:cost big-step) (:cost new-step))
      (do
	(prn new-step)
	(send step-size (constantly big-step-size))
	(prn big-step)
	(send the-soln (constantly big-step)))
      (send the-soln (constantly new-step)))))

(defn solve []
  (set-validator! the-soln #(<= (:cost %) (:cost @the-soln)))
  (add-watch the-soln :solve
	     (fn [_ _ _ _] (send no-chg-cnt (constantly 0))))
  (add-watch iter :solve
	     (fn [_ _ _ _] (take-steps)))
  (dotimes [n 1000]
    (swap! iter inc)))
  
;;
;; Non-code stuff for easy REPL loading...
;;
(comment 
  (load-file "/home/brian/code/clj/cleverish/src/main/clojure/cleverish/adaptive_random_search2.clj")
  (in-ns 'cleverish.adaptive-random-search2)
  )