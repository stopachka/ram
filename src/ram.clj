(ns ram
  (:require [clojure.math.combinatorics :as c]))

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
    (add-machine empty-state {:in [:a :b] :out :o})
    :a))

; wires
; ------

(defn wire-name [n & args]
  (keyword (apply str (list* (name n) args))))

(def _names (atom {}))
(defn wire
  ([] (wire :w))
  ([n]
   (swap! _names update n (fn [i] (inc (or i 0))))
   (let [i (@_names n)]
     (if (> i 1)
       (wire-name n "#" i)
       n))))

(defn wires [n r]
  (mapv (fn [i] (wire (wire-name n "-" i))) (range r)))

(def w# nth)

(defn read-wires [{:keys [charge-map]} ws]
  (map #(or (charge-map %) 0) ws))

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

(declare trigger)

(defn trigger-machine [state {:keys [in out]}]
  (let [curr-v (get-in state [:charge-map out])
        new-v (apply nand-xf (map (:charge-map state)
                                  in))]
    (if (= curr-v new-v)
      state
      (trigger state out new-v))))

(defn trigger [state wire new-v]
  (let [state' (assoc-in state [:charge-map wire] new-v)
        machines (dependent-machines state wire)]
    (reduce (fn [s m] (trigger-machine s m)) state' machines)))

(comment
  (do
    (def s (wire-nand-gate {} :a :b :o))
    (def s1 (trigger s :a 1))
    (println
      "a 1, b 0 o 1 \n"
      (read-wires s1 [:a :b :o]))
    (def s2 (trigger s1 :b 1))
    (println
      "a 1 b 1 o 0 \n"
      (read-wires s2 [:a :b :o]))))

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
      "a 0 o 1 \n" (read-wires s2 [:a :o]))
    (def s3 (trigger s2 :a 1))
    (println
      "a 1 o 0 \n" (read-wires s3 [:a :o]))))

(defn wire-and-gate [state a b o]
  (let [nand-o (wire)]
    (-> state
        (wire-nand-gate a b nand-o)
        (wire-not-gate nand-o o))))

(comment
  (do
    (def s (wire-and-gate empty-state :a :b :o))
    (def s2 (trigger s :a 1))
    (println
      "a 1 b 0 o 0 \n" (read-wires s2 [:a :b :o]))
    (def s3 (trigger s2 :b 1))
    (println
      "a 1 b 1 o 1 \n" (read-wires s3 [:a :b :o]))))

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
      (read-wires s2 [:s :i :o]))
    (def s3 (trigger s2 :s 1))
    (println
      "b/c 1, i 1 o 1 \n"
      (read-wires s3 [:s :i :o]))
    (def s4 (trigger (trigger s3 :s 0) :i 0))
    (println
      "b/c s 0, i 0 o  1 \n"
      (read-wires s4 [:s :i :o]))))

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
      [(read-wires s2 is)
       (read-wires s2 os)])
    (def s3 (-> s2 (trigger :s 1)))
    (println
      "okay, set s, so i1-3 are in os \n"
      [(read-wires s3 is)
       (read-wires s3 os)])
    (def s4 (-> s3 (trigger :s 0)))
    (println
      "okay, disable s, so os are frozen \n"
      [(read-wires s4 is)
       (read-wires s4 os)])
    (def s5 (-> s4
                (trigger (w# is 0) 0)
                (trigger (w# is 1) 0)
                (trigger (w# is 2) 0)
                (trigger (w# is 3) 1)))
    (println
      "i4 should be 1, but only o1-3 should be 1 \n"
      [(read-wires s5 is)
       (read-wires s5 os)])))

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
      [(read-wires s is)
       (read-wires s os)])
    (def s2 (trigger s :e 1))
    (println
      "os should be triggered now \n"
      [(read-wires s2 is)
       (read-wires s2 os)])
    (def s3 (trigger s :e 0))
    (println
      "os should be blocked to 0 again \n"
      [(read-wires s3 is)
       (read-wires s3 os)])))

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
      [(read-wires s is)
       (read-wires s bs)
       (read-wires s os)])
    (def s2 (trigger s :s 1))
    (println
      "b1-3 should be 1, but os should be 0 \n"
      [(read-wires s2 is)
       (read-wires s2 bs)
       (read-wires s2 os)])
    (def s3 (trigger s2 :e 1))
    (println
      "os should be 1 now \n"
      [(read-wires s3 is)
       (read-wires s3 bs)
       (read-wires s3 os)])))


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
      [:bus (read-wires s2 bw)
       :r1 (read-wires s2 r1-bits)
       :r2 (read-wires s2 r2-bits)
       :r3 (read-wires s2 r3-bits)])
    (def s3 (-> s2 (trigger :e1 1)))
    (println
      "enable r1. bus should now have the same charge \n"
      [:bus (read-wires s3 bw)
       :r1 (read-wires s3 r1-bits)
       :r2 (read-wires s3 r2-bits)
       :r3 (read-wires s3 r3-bits)])
    (def s4 (-> s3 (trigger :s3 1)))
    (println
      "set r3. charge should now move to r3 \n"
      [:bus (read-wires s4 bw)
       :r1 (read-wires s4 r1-bits)
       :r2 (read-wires s4 r2-bits)
       :r3 (read-wires s4 r3-bits)])))

; and-n
; -----

(defn wire-and-n [state ins out]
  (let [[a b] (take 2 ins)
        rem (drop 2 ins)]
    (if-not (seq rem)
      (wire-and-gate state a b out)
      (let [w (wire (wire-name a b :-and))]
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
      [(read-wires s2 is)
       (read-wires s2 [o])])
    (def s3 (reduce #(trigger %1 %2 1) s2 is))
    (println
      "out is 1 since all is are 1"
      [(read-wires s3 is)
       (read-wires s3 [o])])))

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

  the mapping is produced by
  "
  [state ins outs]
  (let [ins-nots (mapv #(wire (wire-name % :-not)) ins)
        state' (reduce
                 (fn [acc-state [in out]]
                   (wire-not-gate acc-state in out))
                 state
                 (map vector ins ins-nots))
        state'' (reduce
                  (fn [acc-state [sel out]]
                    (wire-and-n acc-state
                                (map-indexed
                                  (fn [i sign]
                                    (if (= sign 0)
                                      (nth ins-nots i)
                                      (nth ins i)))
                                  sel)
                                out))
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
      "the correct wire should be 1 \n"
      (read-wires s2 [out])
      "rest should be 0 \n"
      (every? zero? (read-wires s2 (remove #{out} outs)))
      )))
