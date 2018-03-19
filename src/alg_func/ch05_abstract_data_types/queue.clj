(ns alg-func.ch05-abstract-data-types.queue
  (:gen-class))

;; 5.3 Queues 

;; this version uses 2 lists as in the book. 
;; (But, in clojure, using a single vector should be more efficient because of the O(1) efficiency of the subvec function)

(defprotocol QueueProtocol
  (queue-empty? [q])
  (enqueue [q x])
  (dequeue [q])
  (front [q]))

(defrecord Queue [f r])

(defn empty-queue [] (->Queue '() '()))

(extend-protocol 
  QueueProtocol
  Queue
  (queue-empty? [q] (and (empty? (:f q)) (empty? (:r q)))) ;; called queueEmpty in the book
  (enqueue [q x] (->Queue (:f q) (cons x (:r q))))
  (dequeue [q] 
    (if (empty? (:f q))
        (->Queue (rest (reverse (:r q))) '())
        (->Queue (rest (:f q)) (:r q))))
  (front [q]
    (if (empty? (:f q))
      (last  (:r q))
      (first (:f q)))))

;;;;;;;;;;;;;;;;;;;;;

;; In clojure, using a vector should be efficient because of the O(1) efficiency of the subvec function
;; the first element of the vector is the front, the last element of the vector is the back
;; - enqueue will use conj, which is O(1)
;; - dequeue will use subvec, which is also O(1)

(extend-protocol 
  QueueProtocol
  clojure.lang.IPersistentVector
  (queue-empty? [v] (empty? v)) ;; called queueEmpty in the book
  (enqueue [v x] (conj v x))
  (dequeue [v] (subvec v 1))
  (front [v] (first v)))

