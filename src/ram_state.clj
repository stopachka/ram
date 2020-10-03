(ns ram-state
  (:require [clojure.core.async
             :refer [go-loop chan alts! >! <! >!!
                     mult tap]]))


(def empty-state {:charge-map {} :machines #{}})

;; naming

(def _name-c (atom 0))

(defn uniq-name [s] (keyword (str (name s) "-" (swap! _name-c inc))))

(defn wire [s] (uniq-name s))

;; wire up a nand gate

(defn add-machine [s m] (update s :machines conj m))

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn wire-nand-gate [state a b o]
  (add-machine
    state
    {:name (uniq-name :nand)
     :in [a b]
     :f nand-xf
     :out o}))

(defn dependent-machines [state wire]
  (->> state
       :machines
       (filter (fn [{:keys [in]}]
                 (some #{wire} in)))))

(comment
  (def s (wire-nand-gate empty-state :a :b :o))
  [s
   (dependent-machines s :a)])

;; trigger

(declare trigger)

(defn trigger-machine [state m]
  (let [wire (:out m)
        f (:f m)
        curr-v (get-in state [:charge-map wire])
        new-v (apply f (map (:charge-map state)
                            (:in m)))]
    (if (= curr-v new-v)
      state
      (trigger state wire new-v))))

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
  (let [nand-o-wire (wire :nand-o)]
    (-> state
        (wire-nand-gate a b nand-o-wire)
        (wire-not-gate nand-o-wire o))))

(comment
  (def s (wire-and-gate empty-state :a :b :o))
  (def s' (trigger s :a 1))
  s'
  (def s'' (trigger s' :b 1))
  s'')

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
  ([state i s o]
   (let [a (wire :mem-a)
         b (wire :mem-b)
         c (wire :mem-c)]
     (-> state
         (wire-nand-gate i s a)
         (wire-nand-gate a s b)
         (wire-nand-gate a c o)
         (wire-nand-gate b o c)))))

(comment
  (def s (wire-memory-bit empty-state :i :s :o))
  (def s' (trigger s :i 1))
  s'
  (def s'' (trigger s' :s 1))
  s''
  (def s''' (trigger (trigger s'' :s 0) :i 0))
  s''')
