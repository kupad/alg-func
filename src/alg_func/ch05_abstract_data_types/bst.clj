(ns alg-func.ch05-abstract-data-types.bst
  (:gen-class))

;; 5.7 Binary Search Tree

;; this implementation uses nil as an empty bst

(defprotocol BST
  (in-tree? [bt v])
  (min-tree [bt])
  (add-tree [bt v])
  (del-tree [bt v])
  (in-order [bt]))

(defrecord BinTree [v lt rt])

; we're treating nil as an empty BinTree. The protocol needs to be defined for nil for
; that to work
(extend-protocol
  BST
  nil
  (in-tree [bt v] false)
  (min-tree [bt] nil)
  (add-tree [bt v] (->BinTree v nil nil)) 
  (del-tree [bt v] nil))

(extend-protocol
  BST
  BinTree
  (in-tree? [bt v]
    (cond
      (empty? bt) false
      (== v (:v bt)) true
      (< v (:v bt)) (recur (:lt bt) v)
      :else (recur (:rt bt) v)))
  (add-tree [bt v]
    (cond
      (== v (:v bt)) bt
      (< v (:v bt)) (->BinTree (:v bt) (add-tree (:lt bt) v) (:rt bt))
      :else (->BinTree (:v bt) (:lt bt) (add-tree (:rt bt) v))))
  (min-tree [bt]
    (if (empty? (:lt bt)) 
      (:v bt)
      (recur (:lt bt))))
  (del-tree [bt v']
    (let [v  (:v bt)
          lf (:lt bt)
          rt (:rt bt)]
      (cond
        (and (== v' v) (empty? lf)) rt
        (and (== v' v) (empty? rt)) lf
        (< v' v) (->BinTree v (del-tree lf v') rt)
        (> v' v) (->BinTree v lf (del-tree rt v'))
        :else
        (let [k (min-tree rt)]
          (->BinTree k lf (del-tree rt k)))))))

;; because inorder returns a collection and is recursive, it needs to be outside the protocol
(defn in-order [bt]
  (if (empty? bt)
    []
    (concat (in-order (:lt bt)) [(:v bt)] (in-order (:rt bt))))) 

(defn build-tree [lf]
  (reduce add-tree nil lf))

; this builds a more balanced tree, but s must already be sorted
(defn build-tree' [s]
  (if (empty? s)
    nil
    (let [n (int (/ (count s) 2))
        l1 (take n s)
        [x & l2]  (drop n s)]
      (->BinTree x (build-tree' l1) (build-tree' l2)))))
    
