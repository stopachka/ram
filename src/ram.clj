(ns ram
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as string]))

; ex v0
; -----

(def ex-state-v0 {:charge-map {:a 1 :b 1 :c 0}
                  :nand-gates [{:ins [:a :b]
                                :out :c}]})

; state v0
; ---------------

(def empty-state {:charge-map {} :nand-gates []})

(defn charge [state wire]
  (get-in state [:charge-map wire]))

(defn charges [state wires]
  (map (partial charge state) wires))

(defn set-charge [state wire charge]
  (assoc-in state [:charge-map wire] charge))

(defn wire-nand-gate [state a b o]
  (update state :nand-gates conj {:ins [a b] :out o}))

(comment
  (charges (-> empty-state
               (set-charge :a 1)
               (set-charge :b 0))
           [:a :b])
  (wire-nand-gate empty-state :a :b :c))


; trigger v0
; ----------

(defn nand-xf [a b]
  (if (= a b 1) 0 1))

(comment
  (nand-xf 0 0)
  (nand-xf 1 1))

(defn dependent-nand-gates [state wire]
  (filter (fn [{:keys [ins]}] (some #{wire} ins))
          (:nand-gates state)))

(comment
  (dependent-nand-gates (wire-nand-gate empty-state :a :b :c) :a))

(declare trigger-nand-gate)
(defn trigger
  ([state wire new-v]
   (let [old-c (charge state wire)
         state' (set-charge state wire new-v)
         new-c (charge state' wire)]
     (if (= old-c new-c)
       state'
       (reduce (fn [s out] (trigger-nand-gate s out))
               state'
               (dependent-nand-gates state' wire))))))

(defn trigger-nand-gate
  [state {:keys [ins out]}]
  (let [new-charge (apply nand-xf (charges state ins))]
    (trigger state out new-charge)))

(defn trigger-many [state wires charges]
  (reduce
    (fn [acc-state [wire charge]]
      (trigger acc-state wire charge))
    state
    (map vector wires charges)))


; not gate
; --------

(defn wire-not-gate
  ([state a o]
   (wire-nand-gate state a a o)))

; wires v0
; ----------

(def _u (atom {}))
(defn uniq-n [k]
  (swap! _u update k (fn [i] (inc (or i 0))))
  (get @_u k))

(defn kw [& args]
  (->> args
       (map (fn [x] (if ((some-fn keyword? symbol?) x)
                      (name x)
                      x)))
       (apply str)
       keyword))

(defn wire
  ([n]
   (let [i (uniq-n n)]
     (if (> i 1) (kw n "#" i) n))))

(comment
  [(wire :a)
   (wire :a)])

; and gate
; --------

(defn wire-and-gate [state a b o]
  (let [nand-o (wire (kw a b :and-nand-o))]
    (-> state
        (wire-nand-gate a b nand-o)
        (wire-not-gate nand-o o))))

; memory-bit
; ----------

(defn wire-memory-bit
  ([state s i o]
   (let [a (wire :a)
         b (wire :b)
         c (wire :c)]
     (-> state
         (wire-nand-gate i s a)
         (wire-nand-gate a s b)
         (wire-nand-gate a c o)
         (wire-nand-gate b o c)))))

; wires v1
; ----------

(defn names [n r]
  (mapv (fn [i] (kw n "-" i)) (range r)))

(def wires (comp (partial mapv wire) names))

(comment (wires :i 8))

; byte
; ----

(defn wire-byte [state s ins outs]
  (reduce (fn [acc-state [i o]]
            (wire-memory-bit acc-state s i o))
          state
          (map vector ins outs)))

; enabler
; -------

(defn wire-enabler
  [state e ins outs]
  (reduce
    (fn [acc-state [in out]]
      (wire-and-gate acc-state e in out))
    state
    (map vector ins outs)))


; register
; --------

(defn wire-register [state s e ins bits outs]
  (-> state
      (wire-byte s ins bits)
      (wire-enabler e bits outs)))

; state v1
; --------

(defn set-charge
  ([state source wire charge]
   (assoc-in state [:charge-map wire source] charge)))

(comment (set-charge empty-state :nand-a :w 1))

(defn charge [{:keys [charge-map]} w]
  (when-let [charges (vals (charge-map w))]
    (apply max charges)))

(comment
  (let [s1 (-> empty-state
               (set-charge :nand-a :w 1)
               (set-charge :nand-b :w 0))]
    (charge s1 :w)))

; trigger v1
; ----------

(defn trigger-nand-gate
  [state {:keys [ins out]}]
  (let [new-charge (apply nand-xf (charges state ins))]
    (trigger (apply kw (conj ins out)) state out new-charge)))

(defn trigger
  ([state wire new-v] (trigger :repl state wire new-v))
  ([source state wire new-v]
   (let [old-c (charge state wire)
         state' (set-charge state source wire new-v)
         new-c (charge state' wire)]
     (if (= old-c new-c)
       state'
       (reduce (fn [s out] (trigger-nand-gate s out))
               state'
               (dependent-nand-gates state' wire))))))

; wire-bus
; --------

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

(def decoder-mapping (partial c/selections [0 1]))

(comment (decoder-mapping 2))

(defn wire-decoder
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
                  (map vector (decoder-mapping (count ins)) outs))]
    state''))

; mar
; ---

(defn wire-mar
  [state s is os first-4-outs last-4-outs]
  (-> state
      (wire-byte s is os)
      (wire-decoder (take 4 os) first-4-outs)
      (wire-decoder (drop 4 os) last-4-outs)))

; io
; --

(defn wire-io
  [state io-s io-e ios decoder-o-1 decoder-o-2 register-bits]
  (let [x (wire (kw decoder-o-1 decoder-o-2 :x))
        s (wire (kw decoder-o-1 decoder-o-2 :s))
        e (wire (kw decoder-o-1 decoder-o-2 :e))]
    (-> state
        (wire-and-gate decoder-o-1 decoder-o-2 x)
        (wire-and-gate x io-s s)
        (wire-and-gate x io-e e)
        (wire-bus ios s e register-bits))))

; ram
; ---

(defn wire-ram [state mar-s mar-is io-s io-e ios]
  (let [mar-os (wires :mar-o 8)
        mar-first-4-outs (wires :mar-dec-f 16)
        mar-last-4-outs (wires :mar-dec-l 16)
        state' (wire-mar state mar-s mar-is mar-os mar-first-4-outs mar-last-4-outs)
        intersections (c/cartesian-product mar-first-4-outs mar-last-4-outs)
        state'' (reduce
                  (fn [acc-state [fw lw]]
                    (wire-io acc-state io-s io-e ios fw lw
                             (wires (kw fw lw :rb) 8)))
                  state'
                  intersections)]
    state''))

; state v2
; ----


(def empty-state {:charge-map {} :in->nand-gate {}})

(defn wire-nand-gate [s a b o]
  (reduce
    (fn [acc-state in]
      (update-in acc-state
                 [:in->nand-gate in]
                 (fn [xs] (conj (or xs []) {:ins [a b] :out o}))))
    s
    [a b]))

(defn dependent-nand-gates [s w]
  (get-in s [:in->nand-gate w]))

; repl
; ----

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

(defn handle-read [state mar-s mar-is io-e ios loc ]
  (let [charge-bus-with-register (-> state
                                     (set-mar mar-s mar-is loc)
                                     (trigger io-e 1))
        next (-> charge-bus-with-register
                 (trigger io-e 0))]
    (println (str "> " (string/join (charges charge-bus-with-register
                                          ios))))
    next))

(defn handle-write [state mar-s mar-is io-s ios loc vs]
  (let [next (-> state
                 (set-mar mar-s mar-is loc)
                 (trigger-many ios vs)
                 (trigger io-s 1)
                 (trigger io-s 0)
                 (trigger-many ios [0 0 0 0 0 0 0 0]))]
    (println "> done")
    next))

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
          (recur (handle-read state :mar-s mar-is :io-e ios (first args)))
          'write
          (recur (handle-write state :ms mar-is :io-s ios (first args) (second args)))
          'exit
          (println "> Goodbye!"))))))

(comment
  (ram-repl))
