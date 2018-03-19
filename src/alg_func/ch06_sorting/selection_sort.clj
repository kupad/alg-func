(ns alg-func.ch06-sorting.selection-sort
  (:gen-class))

;; 6.3.1 selection sort

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

;; the book uses a function called "minimum" to find the min member in a list
(defn minimum [coll]
  (reduce min coll))

(defn ssort [xs]
  (if (empty? xs)
    xs
    (let [m (minimum xs)]
      (cons m (ssort (delete m xs)))))) 

;;;;;;

;; ssort' uses split to avoid both searching for the min and seperating the min from the rest of the list in two steps,
;; instead doing it in one step.

(declare ssort')

(defn split [xs m r]
  (if (empty? xs)
    (cons m (ssort' r))
    (let [x  (first xs)
          xs (rest xs)]
      (if (< x m)
        (recur xs x (cons m r))
        (recur xs m (cons x r))))))

(defn ssort' [xs]
  (if (empty? xs)
    xs
    (split (rest xs) (first xs) [])))

;;;;;;;;;;;;;;;;;;;;

;; the book's ssort' is not TRO, limiting the size of the input
;;
;; this is my attempt at a tail-recursive version ssort'
;;
;; to do this, the split function doesn't call ssort directly, but returns a vector with the min in the head
;; and a collection of the rest of the collection, and ssort will use the results. 
;; (Personally, I find this easier to understand as well).

(defn split-tr [xs m r]
  (if (empty? xs)
    [m r]
    (let [x  (first xs)
          xs (rest xs)]
      (if (< x m)
        (recur xs x (cons m r))
        (recur xs m (cons x r))))))

(defn ssort'-tr [xs]
  (loop [xs xs
         acc []]
        (if (empty? xs)
          acc
          (let [[m r] (split-tr (rest xs) (first xs) [])]
            (recur r (conj acc m))))))

;;;;;;;;;;;;;;;;;;

;; ssort-trad: out of curiousity, I wanted to make a more tradition (ie imperative style) version of selection sort 
;; using swaps on a vector. it's interesting that ssort'-tr is significantly faster (on 10000 sized vectors, for instance)

;; "swap" two elements in a vector 
(defn swap [v i1 i2] 
   (assoc v i2 (v i1) i1 (v i2)))

(defn find-min-index [v start]
  (loop [i (inc start)
         mi start
         v v] 
        (cond
          (>= i (count v)) mi
          (< (nth v i) (nth v mi)) (recur (inc i) i v)
          :else                    (recur (inc i) mi v))))

(defn ssort-trad [v]
  (loop [i 0
         v v]
        (if (>= i (count v))
          v
          (recur (inc i) (swap v i (find-min-index v i))))))

