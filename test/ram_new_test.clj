(ns ram-new-test
  (:require [ram-new :refer :all]
            [clojure.test :refer :all]))

(deftest test-nand-gate
  (let [s1 (-> empty-state
               (wire-nand-gate :a :b :o)
               (trigger-many [:a :b] [1 0]))
        s2 (-> s1
               (trigger :b 1))]
    (testing "just a is on"
      (is (= '(1 0 1) (charges s1 [:a :b :o]))))
    (testing "both a and b are on"
      (is (= '(1 1 0) (charges s2 [:a :b :o]))))))

(deftest test-not-gate
  (let [s1 (-> empty-state
               (wire-not-gate :a :o)
               (trigger :a 0))
        s2 (-> s1
               (trigger :a 1))]
    (testing "a is off"
      (is (= '(0 1) (charges s1 [:a :o]))))
    (testing "a is on"
      (is (= '(1 0) (charges s2 [:a :o]))))))

