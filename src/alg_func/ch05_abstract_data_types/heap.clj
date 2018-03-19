(ns alg-func.ch05-abstract-data-types.heap
  (:gen-class))

;; 5.8 heaps

(defprotocol HeapProtocol
  (empty? [h])
  (find [h])
  (ins [h x])
  (del [h]))

(defrecord Heap [x r a b])

(defn heap [x]
  (->Heap x 1 nil nil))

(defn- rank [h]
  (if (empty? h)
    0
    (:r h)))

(defn- make-hp [x a b]
  (if (>= (rank a) (rank b))
    (->Heap x (inc (rank b)) a b)
    (->Heap x (inc (rank a)) b a)))

(defn- merge [h1 h2]
  (let [{x :x a1 :a b1 :b} h1
        {y :x a2 :a b2 :b} h2]
    (cond
      (and (empty? h1) (empty? h2)) nil
      (and (empty? h1) (not-empty h2)) h2
      (and (not-empty h1) (empty? h2)) h1
      (<= x y) (make-hp x a1 (merge b1 h2))
      :else    (make-hp y a2 (merge h1 b2)))))

(extend-protocol
  HeapProtocol
  nil
  (empty? [h] true)
  (find [h] false)
  (ins [h x] (->Heap x 1 nil nil))
  (del [h] nil))

(extend-protocol
  HeapProtocol
  Heap
  (empty? [h] false)
  (find [h] (:x h))
  (ins [h x] (merge (->Heap x 1 nil nil) h))
  (del [h] (merge (:a h) (:b h))))

