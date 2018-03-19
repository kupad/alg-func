(ns alg-func.ch06-sorting.heap-sort
  (:require [alg-func.ch05-abstract-data-types.pqueue :as pqueue]) 
  (:gen-class))

;; 6.5.5 Heapsort

; there is no code for a heapsort in the book. But here goes:

(defn heapsort [coll]
  (loop [coll coll
         pq (pqueue/build-pq coll)
         s []]
        (if (empty? coll)
          s
          (recur (rest coll) (pqueue/de-pq pq) (conj s (pqueue/front-pq pq))))))




