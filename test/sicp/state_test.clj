(ns sicp.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.state :refer [make-accumulator
                                make-monitored]]
            [sicp.fixed-point :refer [sqrt]]))

(deftest state-test
  (testing "Make accumulator"
    (is (= 15 ((make-accumulator 5) 10)))
    (is (= 20 (let [A (make-accumulator 5)
                    _ (A 5)
                    a2 (A 10)]
                a2)))
    (is (= 0 (let [A (make-accumulator 5)
                   _ (A 5)
                   a2 (A -10)]
               a2))))
  (testing "Make monitored"
    (is (= 10.0 ((make-monitored sqrt) 100)))
    (is (= 1 (let [monitored (make-monitored sqrt)
                   _ (monitored 25)]
               (monitored 'how-many-calls?))))
    (is (= 2 (let [monitored (make-monitored sqrt)
                   _ (monitored 25)
                   _ (monitored 49)]
               (monitored 'how-many-calls?))))))
