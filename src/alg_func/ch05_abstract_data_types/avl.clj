(ns alg-func.ch05-abstract-data-types.avl
  (:gen-class))

;; 5.9 AVL trees

(defprotocol AVL
  (empty? [t])
  (add [t v])
  (height [t]))

(defrecord AVLTree [v lf rt])
 
(defn- rotate-left [t]
  (if (empty? t)
    nil
    (let [{v :v {lv :v lflf :lf lfrt :rt} :lf rt :rt} t]
      (->AVLTree lv lflf
                 (->AVLTree v lfrt rt)))))

(defn- rotate-right [t]
  (if (empty? t)
    nil
    (let [{v :v lf :lf {rv :v rtlf :lf rtrt :rt} :rt} t]
      (->AVLTree rv (->AVLTree v lf rtlf)
                 rtrt))))

(defn- d-rot-right-left [t]
  (let [{v :v lf :lf rt :rt} t 
        {rv :v rtlf :lf rtrt :rt} rt
        {rtlv :v rtlflf :lf rtlfrt :rt} rtlf]
    (->AVLTree rtlv (->AVLTree v lf rtlflf)
                    (->AVLTree rv rtlfrt rtrt))))

(defn- d-rot-left-right [t]
  (let [{v :v lf :lf rt :rt} t
        {lv :v lflf :lf lfrt :rt} lf
        {lfrv :v lfrtlf :lf lfrtrt :rt} lfrt]
    (->AVLTree lfrv (->AVLTree lv lflf lfrtlf)
               (->AVLTree v lfrtrt rt))))

(extend-protocol
  AVL
  nil
  (empty? [_] true)
  (add [_ v] (->AVLTree v nil nil))
  (height [_] 0))

(extend-protocol
  AVL
  AVLTree
  (empty? [_] false)
  (height [t] (+ 1 (max (height (:lf t)) (height (:rt t)))))
  (add [t i] 
    (let [{v :v lf :lf rt :rt} t]
      (if (< i v)
       (let [{newlfv :v :as newlf} (add lf i)]
         (if (== (- (height newlf) (height rt)) 2)
           (if (< i newlfv)
             (rotate-left (->AVLTree v newlf rt))
             (d-rot-left-right (->AVLTree v newlf rt)))
           (->AVLTree v newlf rt)))
       (let [{newrtv :v :as newrt} (add rt i)]
         (if (== (- (height newrt) (height lf)) 2)
           (if (> i newrtv)
             (rotate-right (->AVLTree v lf newrt))
             (d-rot-right-left (->AVLTree v lf newrt)))
           (->AVLTree v lf newrt)))))))

;; because inorder returns a collection and is recursive, it needs to be outside the protocol
(defn in-order [t]
  (if (empty? t)
    []
    (concat (in-order (:lf t)) [(:v t)] (in-order (:rt t))))) 

(defn build-tree [coll]
  (reduce add nil coll))

;(def t1 
;  (->AVLTree 6 (->AVLTree 3 
;                          (->AVLTree 2 nil nil)
;                          (->AVLTree 4 nil (->AVLTree 5 nil nil)))
;             (->AVLTree 7 nil nil)))


