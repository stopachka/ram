(ns ram
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as str]))

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

; state
; -----

; inefficient v

(def empty-state {:charge-map {} :machines []})
(defn add-machine [s m]
  (update s :machines conj m))
(defn dependent-machines [s w]
  (filter #(some #{w} (:ins %)) (:machines s)))

; efficient v
(def empty-state {:charge-map {} :in->machines {}})
(defn add-machine [s {:keys [ins] :as m}]
  (reduce
    (fn [acc-state in]
      (update-in acc-state
                 [:in->machines in]
                 (fn [xs] (conj (or xs []) m))))
    s
    ins))
(defn dependent-machines [s w]
  (get-in s [:in->machines w]))

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
               (dependent-machines state' wire))))))

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
        (wire-bus ios s e register-bits))))

(defn wire-ram [state mar-s mar-is io-s io-e ios]
  (let [mar-os (wires :mar-o 8)
        mar-first-4-outs (wires :mar-dec-f 16)
        mar-last-4-outs (wires :mar-dec-l 16)
        state' (wire-mar state mar-s mar-is mar-os mar-first-4-outs mar-first-4-outs)
        intersections (c/cartesian-product mar-first-4-outs mar-last-4-outs)
        state'' (reduce
                  (fn [acc-state [fw lw]]
                    (wire-io acc-state io-s io-e ios fw lw
                             (wires (kw fw lw :rb) 8)))
                  state'
                  intersections)]
    state''))


(defn initialize-ram [mar-s mar-is io-s io-e ios]
  (-> empty-state
      (wire-ram mar-s mar-is io-s io-e ios)
      (trigger-many mar-is [0 0 0 0 0 0 0 0])
      (trigger-many ios [0 0 0 0 0 0 0 0])
      (trigger-many [mar-s io-s io-e]
                    [0 0 0])))

(defn set-mar [state mar-s mar-is mar-vs]
  (-> state
      (trigger-many mar-is mar-vs)
      (trigger mar-s 1)
      (trigger mar-s 0)))

(defn process-repl [state input])

(defn ram-repl []
  (println
    (str "🔥 Ram Simulation: Type a command. Here's what you can do: \n"
         "   (read [1 0 1 0 1 0 1 0]) \n"
         "   (write [1 0 1 0 1 0 1 0] [1 1 1 1 1 1 1 1]) \n"
         "   (exit)"))
  (let [mar-is (wires :mar-i 8)
        ios (wires :mar-io 8)
        initial-state (initialize-ram :mar-s mar-is :io-s :io-e ios)]
    (loop [state initial-state]
      (let [input (read-string (read-line))
            cmd (first input)
            args (rest input)]
        (condp = cmd
          'read
          (let [loc (first args)
                charge-bus-with-register (-> state
                                             (set-mar :mar-s mar-is loc)
                                             (trigger :io-e 1))
                next (-> charge-bus-with-register
                         (trigger :io-e 0))]
            (println (str "> " (str/join (charges charge-bus-with-register
                                                  ios))))
            (recur next))
          'write
          (let [loc (first args)
                vs (second args)
                next (-> state
                         (set-mar :mar-s mar-is loc)
                         (trigger-many ios vs)
                         (trigger :io-s 1)
                         (trigger :io-s 0)
                         (trigger-many ios [0 0 0 0 0 0 0 0]))]
            (println "> done")
            (recur next))
          'exit
          (println "> Goodbye!"))))))

(comment
  (ram-repl))
