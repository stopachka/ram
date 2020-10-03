(ns ram)

; state
; -----

(def empty-state {:charge-map {} :machines #{}})
(defn add-machine [s m] (update s :machines conj m))
(defn dependent-machines [state wire]
  (->> state
       :machines
       (filter (fn [{:keys [in]}]
                 (some #{wire} in)))))

; naming
; ------

(def _name-c (atom 0))
(defn wire
  ([] (wire :w))
  ([s] (keyword (str (name s) (swap! _name-c inc)))))

; nand
; ----

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn wire-nand-gate [state a b o]
  (add-machine state {:in [a b] :out o}))


(comment
  (def s (wire-nand-gate empty-state :a :b :o))
  [s
   (dependent-machines s :a)])

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
  (def s (wire-nand-gate {} :a :b :o))
  (def s' (trigger s :a 1))
  s'
  (def s'' (trigger s' :b 1))
  s'')

;; not-gate

(defn wire-not-gate
  ([state a o]
   (wire-nand-gate state a a o)))

(comment
  (def s (wire-not-gate empty-state :a :o))
  (def s' (trigger s :a 0))
  s'
  (def s'' (trigger s' :a 1))
  s'')

(defn wire-and-gate [state a b o]
  (let [nand-o (wire)]
    (-> state
        (wire-nand-gate a b nand-o)
        (wire-not-gate nand-o o))))

(comment
  (def s (wire-and-gate empty-state :a :b :o))
  (def s' (trigger s :a 1))
  s'
  (def s'' (trigger s' :b 1))
  s'')

;; memory-bit

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
  (def s (wire-memory-bit empty-state :s :i :o))
  (def s' (trigger s :i 1))
  s'
  (def s'' (trigger s' :s 1))
  s''
  (def s''' (trigger (trigger s'' :s 0) :i 0))
  s''')

;; byte

(defn wire-byte [state s ins outs]
  (reduce (fn [acc-state [i o]]
            (wire-memory-bit acc-state s i o))
          state
          (map vector ins outs)))

(comment
  (do
    (def is [:i1 :i2 :i3 :i4 :i5 :i6 :i7 :i8])
    (def os [:o1 :o2 :o3 :o4 :o5 :o6 :o7 :o8])
    (def s
      (wire-byte
        empty-state
        :s
        is
        os))
    (def s' (->
              s
              (trigger :i1 1)
              (trigger :i2 1)
              (trigger :i3 1)))
    (println
      "change i, but do not set o yet \n"
      [(select-keys (:charge-map s') is)
       (select-keys (:charge-map s') os)])
    (def s'' (-> s' (trigger :s 1)))
    (println
      "okay, set s, so i1-3 are in os \n"
      [(select-keys (:charge-map s'') is)
       (select-keys (:charge-map s'') os)])
    (def s''' (-> s'' (trigger :s 0)))
    (println
      "okay, disable s, so os are frozen \n"
      [(select-keys (:charge-map s''') is)
       (select-keys (:charge-map s''') os)])
    (def s4 (-> s'''
                (trigger :i1 0)
                (trigger :i2 0)
                (trigger :i3 0)
                (trigger :i4 1)))
    (println
      "i4 should be 1, but only o1-3 should be 1 \n"
      [(select-keys (:charge-map s4) is)
       (select-keys (:charge-map s4) os)])))

(defn wire-enabler
  [state e ins outs]
  (reduce
    (fn [acc-state [in out]]
      (wire-and-gate acc-state e in out))
    state
    (map vector ins outs)))

(comment
  (do
    (def is [:i1 :i2 :i3 :i4 :i5 :i6 :i7 :i8])
    (def os [:o1 :o2 :o3 :o4 :o5 :o6 :o7 :o8])
    (def s (-> (wire-enabler empty-state :e is os)
               (trigger :i1 1)
               (trigger :i2 1)
               (trigger :i3 1)))
    (println
      "i1-3 should be 1, but os should all be 0 \n"
      [(select-keys (:charge-map s) is)
       (select-keys (:charge-map s) os)])
    (def s' (trigger s :e 1))
    (println
      "os should be triggered now \n"
      [(select-keys (:charge-map s') is)
       (select-keys (:charge-map s') os)])
    (def s'' (trigger s :e 0))
    (println
      "os should be blocked to 0 again \n"
      [(select-keys (:charge-map s'') is)
       (select-keys (:charge-map s'') os)])))

(defn wire-register [state s e ins outs]
  (let [byte-os (map (fn [x] (wire (str "b-o-" (name x)))) ins)]
    (-> state
        (wire-byte s ins byte-os)
        (wire-enabler e byte-os outs))))

(comment
  (do
    (def is [:i1 :i2 :i3 :i4 :i5 :i6 :i7 :i8])
    (def os [:o1 :o2 :o3 :o4 :o5 :o6 :o7 :o8])
    (def s (-> (wire-register empty-state :s :e is os)
               (trigger :i1 1)
               (trigger :i2 1)
               (trigger :i3 1)))
    (println
      "i1-3 should be 1, but os should all be 0 \n"
      [(select-keys (:charge-map s) is)
       (select-keys (:charge-map s) os)])
    (def s' (trigger s :s 1))
    (println
      "b-o 1-3 should be 1, but os should be 0 \n"
      [(:charge-map s')
       (select-keys (:charge-map s') os)])
    (def s'' (trigger s' :e 1))
    (println
      "os should be 1 now \n"
      [(select-keys (:charge-map s'') is)
       (select-keys (:charge-map s'') os)])))

; hmm think about this one
; what should the inputs and outputs be fore these registers?
;   perhaps they should have their own wires? then a new "connect" functionality,
;   to wire the inputs and outputs of the wires to the bus
; also we'll need a better way to test stuff. maybe i can make some helpers to make
; the comment blocks above a bit simpler
(defn wire-bus [state bus-wires register-setters register-enablers]
  (reduce
    (fn [acc-state [s e]]
      (wire-register acc-state s e bus-wires bus-wires))
    state
    (map vector register-setters register-enablers)))

(comment
  (do
    (def bus-wires [:b1 :b2 :b3 :b4 :b5 :b6 :b7])))
