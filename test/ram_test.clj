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
               (trigger :s1 0)
               (trigger :e1 0))
        s2 (-> s1
               (trigger :s1 1)
               (trigger :s1 0)
               (trigger-many bw [0 0 0 0 0 0 0 0]))
        s3 (-> s2
               (trigger :e1 1)
               (trigger :s3 1)
               (trigger :s3 0)
               (trigger :e1 0))]
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
    (testing "move r1 to r3"
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s3 bw)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 r1-bits)))
      (is (= '(0 0 0 0 0 0 0 0)
             (charges s2 r2-bits)))
      (is (= '(1 1 1 0 0 0 0 0)
             (charges s3 r3-bits))))))

(deftest test-and-n
  (let [ii [:a :b :c :d :e]
        s1 (-> empty-state
               (wire-and-n ii :o)
               (trigger-many ii [1 1 1 1 0]))
        s2 (trigger-many s1 ii [1 1 1 1 1])]
    (testing "if only some are charged, o is off"
      (is (= '(1 1 1 1 0)
             (charges s1 ii)))
      (is (= 0 (charge s1 :o))))
    (testing "if all are on, we are on"
      (is (= '(1 1 1 1 1)
             (charges s2 ii)))
      (is (= 1 (charge s2 :o))))))

(deftest test-decoder
  (let [ii (wires :i 4)
        os (wires :o 16)
        sels (wire-mapping (count ii))
        sel (nth sels 5)
        o (nth os 5)
        s1 (-> empty-state
               (wire-decoder ii os)
               (trigger-many ii sel))]
    (testing "only 1 output is on"
      (is (= 1 (charge s1 o)))
      (is (every? zero? (charges s1 (remove #{o} os)))))))

(deftest test-mar
  (let [ii (wires :i 8)
        os (wires :o 8)
        first-4-decoders (wires :fd 16)
        last-4-decoders (wires :ld 16)
        s1 (-> empty-state
               (wire-mar :s ii os first-4-decoders last-4-decoders)
               (trigger-many ii [0 0 0 0 0 0 0 0])
               (trigger :s 0))
        sel (nth (wire-mapping 4) 5)
        s2 (trigger-many s1 ii (concat sel sel))
        s3 (-> s2
               (trigger :s 1)
               (trigger :s 0))
        test-idx (fn [state idx]
                   (let [fd (nth first-4-decoders idx)
                         ld (nth last-4-decoders idx)]
                     (is (= 1 (charge state fd)))
                     (is (every? zero? (charges state (remove #{fd} first-4-decoders))))
                     (is (= 1 (charge state ld)))
                     (is (every? zero? (charges state (remove #{ld} last-4-decoders))))))]
    (testing
      "by default only one wire is on, and it's the correct mapping"
      (test-idx s1 0))
    (testing
      "even if ii changes, sel doesn't change b/c s is 0"
      (test-idx s2 0))
    (testing
      "once s triggers, the sel does change"
      (test-idx s3 5))))

(deftest test-io
  (let [ios (wires :io 8)
        rs (wires :r 8)
        s1 (-> empty-state
               (wire-io :s :e ios :w1 :w2 rs)
               (trigger-many ios [0 0 0 0 0 0 0 0])
               (trigger-many [:s :e :w1 :w2] [0 0 0 0])
               (trigger-many ios [1 1 1 0 0 0 0 0]))
        s2 (trigger s1 :s 1)
        s3 (trigger-many s2 [:w1 :w2] [1 1])
        s4 (-> s3
               (trigger :s 0)
               (trigger-many ios [0 0 0 0 0 0 0 0]))
        s5 (trigger s4 :e 1)]
    (testing "io set, but reg not affected"
      (is (= '(1 1 1 0 0 0 0 0) (charges s1 ios)))
      (is (= '(0 0 0 0 0 0 0 0) (charges s1 rs))))
    (testing "io set enable doesn't change, because intersection is not on"
      (is (= '(1 1 1 0 0 0 0 0) (charges s2 ios)))
      (is (= '(0 0 0 0 0 0 0 0) (charges s2 rs))))
    (testing "once intersection is on, charge transfers"
      (is (= '(1 1 1 0 0 0 0 0) (charges s3 ios)))
      (is (= '(1 1 1 0 0 0 0 0) (charges s3 rs))))
    (testing "once s turns off again, changes to io don't make a difference"
      (is (= '(0 0 0 0 0 0 0 0) (charges s4 ios)))
      (is (= '(1 1 1 0 0 0 0 0) (charges s4 rs))))
    (testing "if we turn on e, the r charge transfers to the io"
      (is (= '(1 1 1 0 0 0 0 0) (charges s5 ios)))
      (is (= '(1 1 1 0 0 0 0 0) (charges s5 rs))))))
