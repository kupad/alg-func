(ns alg-func.ch05-abstract-data-types.sets
  (:require [alg-func.ch05-abstract-data-types.avl :as avl]) 
  (:import [alg_func.ch05_abstract_data_types.avl AVLTree]) 
  (:gen-class))

;; 5.5 Sets

(defprotocol SetProtocol
  (set-empty? [s])
  (in-set? [s x])
  (add-set [s x])
  (del-set [s x])) 

;; 5.5.1 List implementations

;; unordered list /w duplicates

(defrecord UnorderedSetWithDuplicates [ul])

(defn empty-unordered-set-with-duplicates [] (->UnorderedSetWithDuplicates '()))

(extend-protocol 
  SetProtocol
  UnorderedSetWithDuplicates  
  (set-empty? [s] (empty? (:ul s)))
  (in-set? [s x] (some #{x} (:ul s)))
  (add-set [s x] (->UnorderedSetWithDuplicates (cons x (:ul s))))
  (del-set [s x] (->UnorderedSetWithDuplicates (remove #{x} (:ul s)))))

;; unordered list /wo duplicates

(defrecord UnorderedSetNoDuplicates [ul])

(defn empty-unordered-set-no-duplicates [] (->UnorderedSetNoDuplicates '()))

;; remove only the first instance that meets pred
;; we'll use this for defining "delete"
(defn remove-first [pred coll]
  (let [s     (split-with (comp not pred) coll)
        left  (first s)
        right (rest (second s))]
    (into left right)))

;; the book uses "delete" to delete a single element from a list 
;; deletes the first instance that equals element 'e' in a collection
(defn delete [e coll]
  (remove-first #{e} coll))

(extend-protocol 
  SetProtocol
  UnorderedSetNoDuplicates  
  (set-empty? [s] (empty? (:ul s)))
  (in-set? [s x] (some #{x} (:ul s)))
  (add-set [s x] 
    (if (in-set? s x)
     s
     (->UnorderedSetNoDuplicates (cons x (:ul s)))))
  (del-set [s x] (->UnorderedSetNoDuplicates (delete x (:ul s)))))

;; ordered list w/o duplicates

(defrecord OrderedSet [ol])

(defn empty-ordered-set [] (->OrderedSet '()))

(extend-protocol 
  SetProtocol
  OrderedSet
  (set-empty? [s] (empty? (:ol s)))
  (in-set? [s x] (some #{x} (take-while #(<= x %) (:ol s))))
  (add-set [s x] 
    (let [add (fn add [ol x]
      (cond 
        (empty? ol) (cons x ol)
        (< x (first ol)) (cons x ol)
        (>= x (first ol)) (cons (first ol) (add (rest ol) x))
        :else ol))]
      (->OrderedSet (add (:ol s) x))))
  (del-set [s x] (->OrderedSet (delete x (:ol s)))))

;; 5.5.2 Tree Representations

;; AVL tree representation of the set

(extend-protocol 
  SetProtocol
  nil
  (set-empty? [t] true)
  (in-set? [t x] false))

(extend-protocol 
  SetProtocol
  AVLTree
  (set-empty? [t] false)
  (in-set? [t x] 
    (cond
      (set-empty? t) false
      (== x (:v t)) true
      (< x (:v t)) (recur (:lf t) x)
      :else (recur (:rt t) x)))
  (add-set [t x] 
    (when (not (in-set? t x)) 
      (avl/add t x))))

;; 5.5.3 Bit Vector

(def max-set 63)

(extend-protocol 
  SetProtocol
  java.lang.Long 
  (set-empty? [s] (== s 0))
  (in-set? [s i]
   (and (not= s 0)
        (odd? (long (/ s (Math/pow 2 i))))))
  (add-set [s i]
    (let [e (Math/pow 2 i)
          d (long (/ s e))
          m (mod s e)
          d' (if (odd? d) d (inc d))]
      (long (+ (* d' e) m))))
  (del-set [s i]
    (let [e (Math/pow 2 i)
          d (long (/ s e))
          m (mod s e)
          d' (if (odd? d) (dec d) d)]
      (long (+ (* d' e) m)))))

(defn set-2-list [s]
  ((fn s2l [s i]
       (cond
         (== s 0) []
         (odd? s) (conj (s2l (long (/ s 2)) (inc i)) i)
         :else (s2l (long (/ s 2)) (inc i))))
   s 0))



