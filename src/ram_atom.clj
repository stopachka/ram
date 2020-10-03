(ns ram
  (:require [clojure.core.async
             :refer [go-loop chan alts! >! <! >!!
                     mult tap]])
  (:import (java.util UUID)))

(defn wire [] (atom 0))

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn nand-gate!
  ([a b o]
   (letfn [(transition [_ _ _ _]
             (println @a @b @o)
             (reset! o (nand-xf @a @b)))]
     (add-watch a :fn transition)
     (add-watch b :fn transition)
     :ok)))

(comment
  (do
    (def a (wire))
    (def b (wire))
    (def o (wire))
    (nand-gate! a b o)
    (add-watch o :log (fn [_ _ _ _] (println [:a @a :b @b :o @o]))))
  (reset! a 1)
  (reset! b 1))

(defn connect! [a b]
  (add-watch a
             (UUID/randomUUID)
             (fn [_ _ _ v] (reset! b v)))
  :ok)

(defn not-gate!
  ([a o]
   (let [left-wire  (wire)
         right-wire (wire)]
     (connect! a left-wire)
     (connect! a right-wire)
     (nand-gate! left-wire right-wire o)
     :ok)))

(comment
  (do
    (def a (wire))
    (def o (wire))
    (not-gate! a o)
    (add-watch o :log (fn [_ _ _ _] (println [:a @a :o @o]))))
  (reset! a 0)
  (reset! a 1))

(defn and-gate! [a b o]
  (let [nand-o (wire)]
    (nand-gate! a b nand-o)
    (not-gate! nand-o o)
    :ok ))

(comment
  (do
    (def a (wire))
    (def b (wire))
    (def o (wire))
    (and-gate! a b o)
    (add-watch o :log (fn [_ _ _ _] (println [:a @a :b @b :o @o]))))
  (reset! a 1)
  (reset! b 1))

(defn memory-bit!
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
  ([i s o]
   (let [a (wire)
         b (wire)
         c (wire)]
     (nand-gate! i s a)
     (nand-gate! a s b)
     (nand-gate! b o c)
     (nand-gate! a c o))
   :ok))

(comment
  (do
    (def i (wire))
    (def s (wire))
    (def o (wire))
    (memory-bit! i s o)
    (add-watch o :log (fn [_ _ _ _] (println [:i @i :s @s :o @o]))))
  (reset! i 1)
  (reset! s 1))
