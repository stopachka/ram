(ns ram
  (:require [clojure.core.async
             :refer [go-loop chan alts! >! <! >!!
                     mult tap]]))

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(defn nand-gate [a-wire b-wire]
  (let [out (chan)
        chs [a-wire b-wire]
        ch->idx {a-wire 0 b-wire 1}]
    (go-loop [state [nil nil]]
      (let [[v ch] (alts! chs)
            new-state (assoc state (ch->idx ch) v)]
        (when (every? some? new-state)
          (>! out (apply nand-xf new-state)))
        (recur new-state)))
    out))

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

(defn not-gate [a-wire]
  (let [mult-wire (mult a-wire)
        left-wire (chan)
        right-wire (chan)]
    (tap mult-wire left-wire)
    (tap mult-wire right-wire)
    (nand-gate left-wire right-wire)))

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
