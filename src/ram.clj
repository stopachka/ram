(ns ram
  (:require [clojure.math.combinatorics :as c]))

; wires
; ------

(defn kw [& args]
  (->> args
       (map (fn [x] (if ((some-fn keyword? symbol?) x)
                      (name x)
                      x)))
       (apply str)
       keyword))

(def _names (atom {}))
(defn wire
  ([] (wire :w))
  ([n]
   (swap! _names update n (fn [i] (inc (or i 0))))
   (let [i (@_names n)]
     (if (> i 1)
       (kw n "#" i)
       n))))

(defn wires [n r]
  (mapv (fn [i] (wire (kw n "-" i))) (range r)))

(def w# nth)

; state
; -----

(def empty-state {:charge-map {} :machines []})
(defn add-machine [s m] (update s :machines conj m))
(defn dependent-machines [state wire]
  (->> state
       :machines
       (filter (fn [{:keys [in]}]
                 (some #{wire} in)))))

(comment
  (dependent-machines
    (add-machine empty-state {:in [:a :b]
                              :out :o})
    :a))

; nand
; ----

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn wire-nand-gate [state a b o]
  (add-machine state {:in [a b] :out o}))

(comment
  (wire-nand-gate empty-state :a :b :o))

; trigger
; -------

(defn charge [{:keys [charge-map]} w]
  (if-let [charges (vals (charge-map w))]
    (apply max charges)
    0))

(defn charges [state ws]
  (map (partial charge state) ws))

(declare trigger)

(defn trigger-machine [state {:keys [in out]}]
  (let [new-v (apply nand-xf (charges state in))]
    (trigger
      (apply kw (conj in out))
      state out new-v)))

(defn trigger
  ([state wire new-v] (trigger :repl state wire new-v))
  ([source state wire new-v]
   (let [state' (assoc-in state [:charge-map wire source] new-v)]
     (reduce (fn [s m] (trigger-machine s m))
             state'
             (dependent-machines state' wire)))))

(comment
  (do
    (def s (wire-nand-gate empty-state :a :b :o))
    (def s1 (trigger s :a 1))
    (println
      "a 1, b 0 o 1 \n"
      (charges s1 [:a :b :o]))
    (def s2 (trigger s1 :b 1))
    (println
      "a 1 b 1 o 0 \n"
      (charges s2 [:a :b :o]))))

; not
; ---

(defn wire-not-gate
  ([state a o]
   (wire-nand-gate state a a o)))

(comment
  (do
    (def s (wire-not-gate empty-state :a :o))
    (def s2 (trigger s :a 0))
    (println
      "a 0 o 1 \n" (charges s2 [:a :o]))
    (def s3 (trigger s2 :a 1))
    (println
      "a 1 o 0 \n" (charges s3 [:a :o]))))

(defn wire-and-gate [state a b o]
  (let [nand-o (wire (kw a b :and-nand))]
    (-> state
        (wire-nand-gate a b nand-o)
        (wire-not-gate nand-o o))))

(comment
  (do
    (def s (wire-and-gate empty-state :a :b :o))
    (def s2 (trigger s :a 1))
    (println
      "a 1 b 0 o 0 \n" (charges s2 [:a :b :o]))
    (def s3 (trigger s2 :b 1))
    (println
      "a 1 b 1 o 1 \n" (charges s3 [:a :b :o]))))

; memory-bit
; ----------

(defn wire-memory-bit
  "
                           a                              +------------+
        +----------+    +---------------------------------+            |         o
 i +----+          |    |                                 |            +----+-----+
        |          +----+                            +----+            |    |
        |          |    |                            |    +------------+    |
   +----+          |    |                            |                      |
   |    +----------+    |                            +-------------------------+
   |                    |                                                   |  |
   |                    |                              +--------------------+  |
   |                    |                              |                       |
   |                    |   +------------+             | +-------------+       |
   |                    +---+            |             | |             |       |
   |                        |            | b           +-+             +-------+
   |                        |            +----+          |             |  c
+--+------------------------+            |    +----------+             |
 s                          +------------+               |             |
                                                         +-------------+

  "
  ([state s i o]
   (let [a (wire)
         b (wire)
         c (wire)]
     (-> state
         (wire-nand-gate i s a)
         (wire-nand-gate a s b)
         (wire-nand-gate a c o)
         (wire-nand-gate b o c)))))

(comment
  (do
    (def s (wire-memory-bit empty-state :s :i :o))
    (def s2 (trigger s :i 1))
    (println
      "b/c s 0, i 1 o 0  \n"
      (charges s2 [:s :i :o]))
    (def s3 (trigger s2 :s 1))
    (println
      "b/c 1, i 1 o 1 \n"
      (charges s3 [:s :i :o]))
    (def s4 (trigger (trigger s3 :s 0) :i 0))
    (println
      "b/c s 0, i 0 o  1 \n"
      (charges s4 [:s :i :o]))))

; byte
; ----

(defn wire-byte [state s ins outs]
  (reduce (fn [acc-state [i o]]
            (wire-memory-bit acc-state s i o))
          state
          (map vector ins outs)))

(comment
  (do
    (def is (wires :i 8))
    (def os (wires :o 8))
    (def s
      (wire-byte empty-state :s is os))
    (def s2 (-> s
                (trigger (w# is 0) 1)
                (trigger (w# is 1) 1)
                (trigger (w# is 2) 1)))
    (println
      "change i, but do not set o yet \n"
      [(charges s2 is)
       (charges s2 os)])
    (def s3 (-> s2 (trigger :s 1)))
    (println
      "okay, set s, so i1-3 are in os \n"
      [(charges s3 is)
       (charges s3 os)])
    (def s4 (-> s3 (trigger :s 0)))
    (println
      "okay, disable s, so os are frozen \n"
      [(charges s4 is)
       (charges s4 os)])
    (def s5 (-> s4
                (trigger (w# is 0) 0)
                (trigger (w# is 1) 0)
                (trigger (w# is 2) 0)
                (trigger (w# is 3) 1)))
    (println
      "i4 should be 1, but only o1-3 should be 1 \n"
      [(charges s5 is)
       (charges s5 os)])))

(defn wire-enabler
  [state e ins outs]
  (reduce
    (fn [acc-state [in out]]
      (wire-and-gate acc-state e in out))
    state
    (map vector ins outs)))

(comment
  (do
    (def is (wires :i 8))
    (def os (wires :o 8))
    (def s (-> (wire-enabler empty-state :e is os)
               (trigger (w# is 0) 1)
               (trigger (w# is 1) 1)
               (trigger (w# is 2) 1)))
    (println
      "i1-3 should be 1, but os should all be 0 \n"
      [(charges s is)
       (charges s os)])
    (def s2 (trigger s :e 1))
    (println
      "os should be triggered now \n"
      [(charges s2 is)
       (charges s2 os)])
    (def s3 (trigger s :e 0))
    (println
      "os should be blocked to 0 again \n"
      [(charges s3 is)
       (charges s3 os)])))

(defn wire-register [state s e ins bits outs]
  (-> state
      (wire-byte s ins bits)
      (wire-enabler e bits outs)))

(comment
  (do
    (def is (wires :i 8))
    (def bs (wires :b 8))
    (def os (wires :o 8))
    (def s (-> (wire-register empty-state :s :e is bs os)
               (trigger (w# is 0) 1)
               (trigger (w# is 1) 1)
               (trigger (w# is 2) 1)))
    (println
      "i1-3 should be 1, but os should all be 0 \n"
      [(charges s is)
       (charges s bs)
       (charges s os)])
    (def s2 (trigger s :s 1))
    (println
      "b1-3 should be 1, but os should be 0 \n"
      [(charges s2 is)
       (charges s2 bs)
       (charges s2 os)])
    (def s3 (trigger s2 :e 1))
    (println
      "os should be 1 now \n"
      [(charges s3 is)
       (charges s3 bs)
       (charges s3 os)])))


(defn wire-bus [state bus-wires register-infos]
  (reduce
    (fn [acc-state [s e bits]]
      (wire-register acc-state s e bus-wires bits bus-wires))
    state
    register-infos))

(comment
  (do
    (def bw (wires :bw 8))
    (def r1-bits (wires :r1 8))
    (def r2-bits (wires :r2 8))
    (def r3-bits (wires :r3 8))
    (def s (wire-bus empty-state
                    bw
                    [[:s1 :e1 r1-bits]
                      [:s2 :e2 r2-bits]
                      [:s3 :e3 r3-bits]]))
    (def s2 (-> s
                (trigger (w# r1-bits 0) 1)
                (trigger (w# r1-bits 1) 1)))
    (println
      "set r1 bits. no other reg should be affected \n"
      [:bus (charges s2 bw)
       :r1 (charges s2 r1-bits)
       :r2 (charges s2 r2-bits)
       :r3 (charges s2 r3-bits)])
    (def s3 (-> s2 (trigger :e1 1)))
    (println
      "enable r1. bus should now have the same charge \n"
      [:bus (charges s3 bw)
       :r1 (charges s3 r1-bits)
       :r2 (charges s3 r2-bits)
       :r3 (charges s3 r3-bits)])
    (def s4 (-> s3 (trigger :s3 1)))
    (println
      "set r3. charge should now move to r3 \n"
      [:bus (charges s4 bw)
       :r1 (charges s4 r1-bits)
       :r2 (charges s4 r2-bits)
       :r3 (charges s4 r3-bits)])))

; and-n
; -----

(defn wire-and-n [state ins out]
  (let [[a b] (take 2 ins)
        rem (drop 2 ins)]
    (if-not (seq rem)
      (wire-and-gate state a b out)
      (let [w (wire (kw a b :-and))]
        (wire-and-n
          (wire-and-gate state a b w)
          (list* w rem)
          out)))))

(comment
  (do
    (def is [:a :b :c :d :e])
    (def o :f)
    (def s (wire-and-n empty-state
                       is
                       o))
    (def s2 (trigger s (w# is 0) 1))
    (println
      "out is 0 since some is are 0"
      [(charges s2 is)
       (charges s2 [o])])
    (def s3 (reduce #(trigger %1 %2 1) s2 is))
    (println
      "out is 1 since all is are 1"
      [(charges s3 is)
       (charges s3 [o])])))

; decoder
; -------

(def wire-mapping (partial c/selections [0 1]))

(defn wire-decoder
  "a decoder maps input buts to wires that represent each possible selection:
  [a b] -> [w1 w2 w3 w4]
  (0 0) w1
  (0 1) w2
  (1 0) w3
  (1 1) w4

  This is done by associating each in to a not gate.

  we then wire the combo of in and not gates onto an and-n gate
  each and-n gate will _only_ turn on, if the selection it represents is on
  "
  [state ins outs]
  (let [ins-nots (mapv #(wire (kw % :-not)) ins)
        state' (reduce
                 (fn [acc-state [in out]]
                   (wire-not-gate acc-state in out))
                 state
                 (map vector ins ins-nots))
        state'' (reduce
                  (fn [acc-state [sel out]]
                    (let [and-ins (map-indexed
                                  (fn [i sign]
                                    (if (= sign 0)
                                      (nth ins-nots i)
                                      (nth ins i)))
                                  sel)]
                      (println and-ins out)
                      (wire-and-n acc-state and-ins out)))
                  state'
                  (map vector (wire-mapping (count ins)) outs))]
    state''))

(comment
  (do
    (def ins (wires :i 4))
    (def outs (wires :o 16))
    (def s (wire-decoder empty-state ins outs))
    (def sels (wire-mapping (count ins)))
    (def sel (nth sels 5))
    (def out (nth outs 5))
    (def s2 (reduce
              (fn [acc-state [in v]]
                (trigger acc-state in v))
              s
              (map vector ins sel)))

    (println
      (charges s2 ins)
      "the correct wire should be 1: "
      (charges s2 [out]) "\n"
      "rest should be 0: "
      (charges s2 (remove #{out} outs)))))

; ram
; ---

(defn wire-mar
  "mar: memory access register

  This sets up a register with a grid of decoders.

  When a byte is set into the mar register, only _1_ intersection
  will be active in the grid.
  "
  [state s is os first-4-outs last-4-outs]
  (-> state
      (wire-byte s is os)
      (wire-decoder (take 4 os) first-4-outs)
      (wire-decoder (drop 4 os) last-4-outs)))

(comment
  (do
    (def is (wires :i 8))
    (def os (wires :o 8))
    (def first-4-outs (wires :fw 16))
    (def last-4-outs (wires :lw 16))
    (def s (wire-mar empty-state :s is os first-4-outs last-4-outs))
    (def s2 (-> s
                (trigger (w# is 0) 1)
                (trigger (w# is 5) 1)
                (trigger :s 1)
                (trigger :s 0)))
    (println
      ["os are set \n"
       (charges s2 os) "\n"
       "only 1 wire on in first-4-outs \n"
       (charges s2 first-4-outs) "\n"
       "only 1 wire on in last-4-outs \n"
       (charges s2 last-4-outs)])))

(defn wire-io
  "for each intersection, we will set up a register.

  this register can be set and updated by the io bus's io-s and io-e"
  [state io-s io-e ios decoder-o-1 decoder-o-2 register-bits]
  (let [x (wire (kw decoder-o-1 decoder-o-2 :x))
        s (wire (kw decoder-o-1 decoder-o-2 :s))
        e (wire (kw decoder-o-1 decoder-o-2 :e))]
    (-> state
        (wire-and-gate decoder-o-1 decoder-o-2 x)
        (wire-and-gate x io-s s)
        (wire-and-gate x io-e e)
        (wire-register s e ios register-bits ios))))

(comment
  (do
    (def ios (wires :ios 8))
    (def rs (wires :r 8))
    (def s (wire-io empty-state :s :e ios :w1 :w2 rs))
    (def s2 (-> s
                (trigger (w# ios 0) 1)
                (trigger (w# ios 1) 1)
                (trigger :s 1)))
    (println
      ["io set & enabled, but register not affected \n"
       :ios (charges s2 ios) "\n"
       :e (charges s2 [:e]) "\n"
       :rs (charges s2 rs)])
    (def s3 (-> s2
                (trigger :w1 1)
                (trigger :w2 1)))
    (println
      ["io set & enabled, intersection enabled, register got value \n"
       :ios (charges s3 ios) "\n"
       :e (charges s3 [:e]) "\n"
       :rs (charges s3 rs)])))

(defn wire-ram [state set-mar mar-is io-s io-e ios]
  (let [mar-os (wires :mar-o 8)
        mar-first-4-outs (wires :mar-dec-f 16)
        mar-last-4-outs (wires :mar-dec-l 16)
        state' (wire-mar state set-mar mar-is mar-os mar-first-4-outs mar-first-4-outs)
        intersections (c/cartesian-product mar-first-4-outs mar-last-4-outs)
        state'' (reduce
                  (fn [acc-state [fw lw]]
                    (wire-io acc-state io-s io-e ios fw lw
                             (wires (kw fw lw :rb) 8)))
                  state'
                  intersections)]
    state''))

; hmm, okay, next up:
; a. make this fast
; b. make it easier to test out
(comment
  (do
    (def mar-is (wires :mar-i 8))
    (def ios (wires :io 8))
    (def s (wire-ram empty-state :set-mar mar-is :io-s :io-e ios))
    (def s' (-> s
                (trigger (w# mar-is 0) 1)
                (trigger (w# mar-is 5) 1)))))
