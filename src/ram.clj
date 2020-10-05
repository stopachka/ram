(ns ram
  (:require [clojure.math.combinatorics :as c]
            [clojure.set :as set]))

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

(defn add-machine [s m]
  (update s :machines conj m))

; nand
; ----

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn wire-nand-gate [state a b o]
  (add-machine state {:ins [a b] :out o}))

; trigger
; -------

(defn charge [{:keys [charge-map]} w]
  (when-let [charges (vals (charge-map w))]
    (apply max charges)))

(defn charges [state ws]
  (map (partial charge state) ws))

(defn set-charge
  ([state w v] (set-charge :repl state w v))
  ([source state w v]
   (assoc-in state [:charge-map w source] v)))

(declare trigger)

(defn trigger-machine
  [state {:keys [ins out]}]
  (let [new-v (apply nand-xf (charges state ins))]
    (trigger
      (apply kw (conj ins out))
      state out new-v)))

(defn trigger
  ([state wire new-v] (trigger :repl state wire new-v))
  ([source state wire new-v]
   (let [old-c (charge state wire)
         state' (set-charge source state wire new-v)
         new-c (charge state' wire)]
     (if (= old-c new-c)
       state'
       (reduce (fn [s out] (trigger-machine s out))
               state'
               (filter
                 #(some #{wire} (:ins %))
                 (:machines state)))))))

(defn trigger-many [state ws vs]
  (reduce
    (fn [acc-state [w v]]
      (trigger acc-state w v))
    state
    (map vector ws vs)))

(defn simulate-circuit [{:keys [out->ins] :as state}]
  (reduce
    (fn [acc-state out]
      (trigger-machine acc-state out))
    state
    (keys out->ins)))


; not
; ---

(defn wire-not-gate
  ([state a o]
   (wire-nand-gate state a a o)))

(defn wire-and-gate [state a b o]
  (let [nand-o (wire (kw a b :and-nand))]
    (-> state
        (wire-nand-gate a b nand-o)
        (wire-not-gate nand-o o))))

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
   (let [a (wire :a)
         b (wire :b)
         c (wire :c)]
     (-> state
         (wire-nand-gate i s a)
         (wire-nand-gate a s b)
         (wire-nand-gate a c o)
         (wire-nand-gate b o c)))))

; byte
; ----

(defn wire-byte [state s ins outs]
  (reduce (fn [acc-state [i o]]
            (wire-memory-bit acc-state s i o))
          state
          (map vector ins outs)))

(defn wire-enabler
  [state e ins outs]
  (reduce
    (fn [acc-state [in out]]
      (wire-and-gate acc-state e in out))
    state
    (map vector ins outs)))

(defn wire-register [state s e ins bits outs]
  (-> state
      (wire-byte s ins bits)
      (wire-enabler e bits outs)))

(defn wire-bus [state bus-wires s e bits]
  (wire-register state s e bus-wires bits bus-wires))

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
                      (wire-and-n acc-state and-ins out)))
                  state'
                  (map vector (wire-mapping (count ins)) outs))]
    state''))

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
                (set-charge (w# is 0) 1)
                (set-charge (w# is 5) 1)
                (set-charge :s 1)
                simulate-circuit
                (set-charge :s 0)
                simulate-circuit))
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
                (set-charge (w# ios 0) 1)
                (set-charge (w# ios 1) 1)
                (set-charge :s 1)
                simulate-circuit))
    (println
      ["io set, but register not affected \n"
       :ios (charges s2 ios) "\n"
       :e (charges s2 [:e]) "\n"
       :rs (charges s2 rs)])
    (def s3 (-> s2
                (set-charge :w1 1)
                (set-charge :w2 1)
                simulate-circuit))
    (println
      ["io set, intersection enabled, register got value \n"
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

(defn set-charges [state is vs]
  (let [state' (reduce
                 (fn [acc-state [i v]]
                   (set-charge acc-state i v))
                 state
                 (map vector is vs))]
    (-> state')))

(defn ram-repl []
  (let [mar-is (wires :mar-i 8)
        ios (wires :mar-io 8)
        out-rs (wires :out-r 8)
        initial-state (-> empty-state
                          (wire-ram :mar-s mar-is :io-s :io-e ios)
                          (wire-register :out-s :out-e ios out-rs ios))]
    (loop [state initial-state]
      (let [input (read-string (read-line))
            cmd (first input)]
        (condp = cmd
          'read
          (let [state' (-> state
                           (set-charges mar-is (second input))
                           (set-charge :mar-s 1)
                           (set-charge :io-e 1)
                           simulate-circuit
                           (set-charge :io-e 0)
                           (set-charge :mar-s 0)
                           simulate-circuit)]

            (println
              "selected reg value: \n"
              (charges state' ios))
            (recur state'))
          'write
          (let [state' (-> state
                           (set-charge mar-is (second input))
                           (set-charge ios (nth input 2))
                           (set-charge :mar-s 1)
                           (set-charge :io-s 1)
                           simulate-circuit
                           (set-charge :mar-s 0)
                           (set-charge :io-s 0)
                           (set-charge ios '(0 0 0 0 0 0 0 0))
                           simulate-circuit)]
            (println "ok")
            (recur state'))
          'exit
          nil)))))

; --
; current stage
; a. look into bus -- why is r1 being affected?
; b. clean up and get the repl working

(comment
  (ram-repl))
