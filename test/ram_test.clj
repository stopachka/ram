(ns ram-test
  (:require [ram :refer :all]
            [clojure.test :refer :all]))

(deftest test-nand-gate
  (let [s1 (-> empty-state
               (wire-nand-gate :a :b :o)
               (trigger :a 1))
        s2 (-> s1
               (trigger :b 1))]
    (testing "just a is on"
      (is (= '(1 0 1) (charges s1 [:a :b :o]))))
    (testing "both a and b are on"
      (is (= '(1 1 0) (charges s2 [:a :b :o]))))))

(deftest test-not-gate
  (let [s1 (-> empty-state
               (wire-not-gate :a :o))
        s2 (-> s1
               (trigger :a 1))]
    (testing "a is off"
      (is (= '(0 1) (charges s1 [:a :o]))))
    (testing "a is on"
      (is (= '(1 0) (charges s2 [:a :o]))))))

(deftest test-and-gate
  (let [s1 (-> empty-state
               (wire-and-gate :a :b :o)
               (trigger :a 1))
        s2 (-> s1
               (trigger :b 1))]
    (testing "just a is on"
      (is (= '(1 0 0) (charges s1 [:a :b :o]))))
    (testing "a and b on"
      (is (= '(1 1 1) (charges s2 [:a :b :o]))))))

(deftest test-memory-bit
  (let [s1 (-> empty-state
               (wire-memory-bit :s :i :o)
               (trigger :i 1))
        s2 (-> s1
               (trigger :s 1))
        s3 (-> s2
               (trigger :s 0)
               (trigger :i 0))]
    (testing "turning i on does not affect the rest"
      (is (= '(0 1 0) (charges s1 [:s :i :o]))))
    (testing "enabling set transfers i to o"
      (is (= '(1 1 1)
             (charges s2 [:s :i :o]))))
    (testing "disabling set, removes further effects on o"
      (is (= '(0 0 1)
             (charges s3 [:s :i :o]))))))

(deftest test-byte
  (let [ii (wires :i 8)
        os (wires :o 8)
        s1 (-> empty-state
               (wire-byte :s ii os)
               (trigger (w# ii 0) 1)
               (trigger (w# ii 1) 1)
               (trigger (w# ii 2) 1))]
    (testing "disabling set, removes further effects on o"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s1 ii)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 os))))))
