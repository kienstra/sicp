(ns sicp.digits-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.digits :refer [digits]]))

(deftest digits-test

  (testing "Digits"
    (is (= 0 (digits 0 1)))
    (is (= 0 (digits 3 4)))
    (is (= 1 (digits 0 2)))
    (is (= 9 (digits 0 10)))
    (is (= 179 (digits 9 100)))))
