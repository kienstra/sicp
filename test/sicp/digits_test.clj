(ns sicp.digits-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.digits :refer [digits]]))

(deftest digits-test
  (testing "Digits"
    (is (= 0 (digits 0 1)))
    (is (= 0 (digits 3 4)))
    (is (= 1 (digits 0 2)))
    (is (= 9 (digits 0 10)))
    (is (= 189 (digits 0 100)))
    (is (= 0 (digits 9 10)))
    (is (= 2 (digits 9 11)))
    (is (= 180 (digits 9 100)))
    (is (= 2889 (digits 0 1000)))
    (is (= 6969 (digits 0 2020)))
    (is (= 38889 (digits 0 10000)))
    (is (= 3 (digits 100 102)))))
