(ns sicp.monte-carlo-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.monte-carlo :refer [estimate-integral
                                      in-circle?
                                      random-in-range]]))

(deftest monte-carlo-test
  (testing "Random in range"
    (is (>= 100 (random-in-range 10 100)))
    (is (<= 10 (random-in-range 10 100)))
    (is (>= 10 (random-in-range 9 10)))
    (is (<= 9 (random-in-range 9 10))))

  (testing "In circle"
    (is (= true (in-circle? 5 7)))
    (is (= false (in-circle? 10 10)))
    (is (= true (in-circle? 7 9)))
    (is (= false (in-circle? 8 9))))

  (testing "Estimate integral"
    (is (= 0 (estimate-integral in-circle? 0 1 0 1 1)))
    (is (= 0 (estimate-integral in-circle? 0 1 0 1 100)))
    (is (= 4 (estimate-integral in-circle? 4 6 6 8 100)))
    (is (> 3.5 (estimate-integral in-circle? 2 8 4 10 100)))
    (is (< 2.8 (estimate-integral in-circle? 2 8 4 10 100)))))
