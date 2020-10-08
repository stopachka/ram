(ns ram-new
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as string]))

; ex v0
; -----

(def ex-state-v0 {:charge-map {:a 1 :b 1 :c 0}
                  :nand-gates [{:ins [:a :b]
                                :out :c}]})

; update state v0
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

; and gate
; --------
