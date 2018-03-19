(ns alg-func.ch05-abstract-data-types.pqueue
  (:require [alg-func.ch05-abstract-data-types.heap :as heap]) 
  (:import [alg_func.ch05_abstract_data_types.heap Heap]) 
  (:gen-class))

;; 5.4 Priority queues

(defprotocol PriorityQueueProtocol
  (pq-empty? [pq])
  (en-pq [pq x])
  (de-pq [pq])
  (front-pq [pq])) 

;; 5.4.1 List implementation

(extend-protocol 
  PriorityQueueProtocol
  clojure.lang.Cons
  (pq-empty? [l] (empty? l))
  (en-pq [l x]
    ((fn insert [l x]
          (if (empty? l)
            (cons x l)
            (if (< x (first l) )
              (cons x l)
              (cons (first l) (insert (rest l) x)))))
     l x))
  (de-pq [l] (rest l))
  (front-pq [l] (first l)))

;; 5.4.2 Heap implementation
;; 5.8.1 Implementation of the priority queue ADT

(defn make-pq-heap [x] (heap/heap x))

(extend-protocol 
  PriorityQueueProtocol
  Heap
  (pq-empty? [h] (heap/empty? h))
  (en-pq [h x] (heap/ins h x))
  (de-pq [h] (heap/del h))
  (front-pq [h] (heap/find h)))

;; having nil defined here makes it easy to use reduce on a collection and turn it into a priority queue
(extend-protocol 
  PriorityQueueProtocol
  nil
  (en-pq [h x] (heap/ins h x)))

(defn build-pq [coll]
  "Build a priority from coll"
  (reduce en-pq nil coll))

