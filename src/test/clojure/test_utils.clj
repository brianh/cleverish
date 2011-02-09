(ns cleverish.test-utils
  (:use clojure.test)
  (:use cleverish.utils))
;;
;; testing...

(deftest bounded-rand-gen-test
  (is (= 8 ((bounded-rand-gen (constantly 8) 0 5)))))

(deftest test-gen-test
  (let [f (test-gen < identity)
	f2 (test-gen < :x)]
    (is (= 3 (f 8 3)))
    (is (= 3 (f 3 8)))
    (is (= 3 (f 3 3)))
    (is (= {:x 1 :y -3} (f2 {:x 4 :y 8} {:x 1 :y -3})))
    (is (= {:x 4 :y 8} (f2 {:x 4 :y 8} {:x 9 :y -3})))))