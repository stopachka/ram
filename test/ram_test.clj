(ns ram-test
  (:require [ram :refer :all]
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

(deftest test-and-gate
  (let [s1 (-> empty-state
               (wire-and-gate :a :b :o)
               (trigger-many [:a :b] [1 0]))
        s2 (-> s1
               (trigger :b 1))]
    (testing "just a is on"
      (is (= '(1 0 0) (charges s1 [:a :b :o]))))
    (testing "a and b on"
      (is (= '(1 1 1) (charges s2 [:a :b :o]))))))

(deftest test-memory-bit
  (let [s1 (-> empty-state
               (wire-memory-bit :s :i :o)
               (trigger-many [:i :s] [1 0]))
        s2 (-> s1
               (trigger :s 1))
        s3 (-> s2
               (trigger-many [:s :i] [0 0]))]
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
               (trigger-many ii [1 1 1 0 0 0 0 0])
               (trigger :s 0))
        s2 (-> s1
               (trigger :s 1))
        s3 (-> s2
               (trigger :s 0)
               (trigger-many ii [0 0 0 0 0 0 0 1]))]
    (testing "disabling set, removes further effects on o"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s1 ii)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 os))))
    (testing "set s, so os become 1 1 1"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 os))))
    (testing "freeze by disabling s. see that further changes to i do nothing to o"
      (is (= '(0 0 0 0 0 0 0 1)
             (charges s3 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 os))))))

(deftest test-enabler
  (let [ii (wires :i 8)
        os (wires :o 8)
        s1 (-> empty-state
               (wire-enabler :e ii os)
               (trigger-many ii [1 1 1 0 0 0 0 0])
               (trigger :e 0))
        s2 (trigger s1 :e 1)
        s3 (trigger s2 :e 0)]
    (testing "is should be set, but os should be 0"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s1 ii)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 os))))
    (testing "os should pass if enabled"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 os))))
    (testing "os should revert if disabled"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 ii)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s3 os))))))

(deftest test-register
  (let [ii (wires :i 8)
        bs (wires :b 8)
        os (wires :o 8)
        s1 (-> empty-state
               (wire-register :s :e ii bs os)
               (trigger-many ii [1 1 1 0 0 0 0 0])
               (trigger :s 0)
               (trigger :e 0))
        s2 (trigger s1 :s 1)
        s3 (trigger s2 :e 1)
        s4 (-> s3
               (trigger :s 0)
               (trigger-many ii [0 0 0 0 0 0 0 1]))
        s5 (trigger s4 :e 0)]
    (testing "is should be set, but bs and os should be 0, b/c s & e are 0"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s1 ii)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 bs)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 os))))
    (testing "is & bs should be set, as s is on. but os should be 0, b/c e is off"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 bs)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s2 os))))
    (testing "is & bs should be set, as s is on. but os should be 0, b/c e is off"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 bs)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 os))))
    (testing "is should be new v, but bs and os should be the old value"
      (is (= '(0 0 0 0 0 0 0 1)
             (charges s4 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s4 bs)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s4 os))))
    (testing "os should 0 out again"
      (is (= '(0 0 0 0 0 0 0 1)
             (charges s5 ii)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s5 bs)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s5 os))))))
(deftest test-wire-bus
  (let [bw (wires :bw 8)
        r1-bits (wires :r1 8)
        r2-bits (wires :r2 8)
        r3-bits (wires :r2 8)
        s1 (-> empty-state
               (wire-bus bw :s1 :e1 r1-bits)
               (wire-bus bw :s2 :e2 r2-bits)
               (wire-bus bw :s3 :e3 r3-bits)
               (trigger-many bw [1 1 1 0 0 0 0 0])
               (trigger-many [:s1 :s2 :s3] [0 0 0])
               (trigger-many [:e1 :e2 :e2] [0 0 0]))
        s2 (-> s1
               (trigger :s1 1)
               (trigger :s1 0)
               (trigger-many bw [0 0 0 0 0 0 0 0]))
        s3 (-> s2
               (trigger :e1 1))]
    (testing "only bus should have charge"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s1 bw)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 r1-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 r2-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s1 r3-bits))))
    (testing "r1 should have the charge"
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s2 bw)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s2 r1-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s2 r2-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s2 r3-bits))))
    (testing "r3 should have the charge too"
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 bw)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 r1-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s3 r2-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s3 r3-bits))))))
