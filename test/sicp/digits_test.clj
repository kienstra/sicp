(ns sicp.digits-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.digits :refer [digits]]))

(deftest digits-test
  (testing "Digits"
    (is (= 0 (digits 0 0)))
    (is (= 0 (digits 0 1)))
    (is (= 0 (digits 3 4)))
    (is (= 1 (digits 0 2)))
    (is (= 9 (digits 0 10)))
    (is (= 189 (digits 0 100)))
    (is (= 0 (digits 9 10)))
    (is (= 2 (digits 9 11)))
    (is (= 180 (digits 9 100)))
    (is (= 3 (digits 100 102)))
    (is (= 2889 (digits 0 1000)))
    (is (= 6969 (digits 0 2020)))
    (is (= 38889 (digits 0 10000)))
    (is (= 788888889 (digits 0 100000000)))
    (is (= 820359675 (digits 0 103496754)))
    (is (= 31378682729 (digits 0 3248979384)))
    (is (= 1724870258940729 (digits 0 122398758003456)))
    (is (= 1088888888889N (digits 0 10E10)))
    (is (= 1388888888888889N (digits 0 10E13)))))
