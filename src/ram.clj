(ns ram
  (:require [clojure.core.async
             :refer [go-loop chan alts! >! <! >!!
                     mult tap]]))

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn nand-gate
  ([a b] (nand-gate (chan) a b))
  ([out a b]
   (let [chs [a b]
         ch->idx {a 0 b 1}]
     (go-loop [state [nil nil]]
       (let [[v ch] (alts! chs)
             new-state (assoc state (ch->idx ch) v)]
         (>! out (apply nand-xf new-state))
         (recur new-state)))
     out)))

(comment
  (def a-wire (chan))
  (def b-wire (chan))
  (def nand-wire (nand-gate a-wire b-wire))
  (go-loop []
    (println :nand> (<! nand-wire))
    (recur))
  (>!! a-wire 1)
  (>!! b-wire 1)
  (>!! a-wire 0))

(defn not-gate
  ([a] (not-gate (chan) a))
  ([out a]
   (let [mult-wire (mult a)
         left-wire (chan)
         right-wire (chan)]
     (tap mult-wire left-wire)
     (tap mult-wire right-wire)
     (nand-gate out left-wire right-wire))))

(comment
  (def a-wire (chan))
  (def not-wire (not-gate a-wire))
  (go-loop []
    (println :not>  (<! not-wire))
    (recur))
  (>!! a-wire 1)
  (>!! a-wire 0))

(def and-gate (comp not-gate nand-gate))

(comment
  (def a-wire (chan))
  (def b-wire (chan))
  (def and-wire (and-gate a-wire b-wire))
  (go-loop []
    (println :and> (<! and-wire))
    (recur))
  (>!! a-wire 1)
  (>!! b-wire 1)
  (>!! a-wire 0))


(defn memory-bit
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
  ([i s] (memory-bit (chan) i s))
  ([o i s]
   (let [a (nand-gate i s)
         b (nand-gate a s)
         c (nand-gate o b)]
     (nand-gate o a c))))

(comment
  (def i (chan))
  (def s (chan))
  (def o (memory-bit i s))
  (go-loop []
    (println :o> (<! o))
    (recur))
  ; set  i to 1
  (>!! i 1)
  ; => 0 b/c s is not 1 yet
  (>!! s 1)
  ; => 1 b/c s enabled
  (>!! s 0)
  ; => o should freeze to 1...but noo!
  )
