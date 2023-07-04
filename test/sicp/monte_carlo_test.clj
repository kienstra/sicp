(ns sicp.monte-carlo-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.monte-carlo :refer [random-in-range]]))

(deftest monte-carlo-test
  (testing "Random in range"
    (is (>= 100 (random-in-range 10 100)))
    (is (<= 10 (random-in-range 10 100)))
    (is (>= 10 (random-in-range 9 10)))
    (is (<= 9 (random-in-range 9 10)))))
