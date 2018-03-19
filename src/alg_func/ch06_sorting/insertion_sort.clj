(ns alg-func.ch06-sorting.insertion-sort
  (:gen-class))

;; 6.3.2 insertion sort

;; this insert follows the book's version closely
;; (I probably would have used filter instead of take-while, for instance)
(defn insert [x xs]
  (concat 
    (take-while #(<= % x) xs) 
    [x]
    (drop-while #(<= % x) xs)))

(defn isort [xs]
  (if (empty? xs)
    xs
    (insert (first xs) (isort (rest xs)))))

;; insert' is the version of insert from the book that does away with doing 2 passes via take-while and drop-while on every
;; call. it promises and looks like it would be faster
;; but what's interesting is that it doesn't seem to be any faster than the insert that uses take-while and drop-while
(defn insert' [key xs]
  (if (empty? xs)
    xs
    (let [l xs
          x (first l)
          xs (rest l)]
      (if (<= key x)
        (cons key l)
        (cons x (insert' key xs))))))

;; there isn't a foldr in clojure (afaik), so this isort' is exactly like isort, except that it uses insert'
(defn isort' [xs]
  (if (empty? xs)
    xs
    (insert' (first xs) (isort (rest xs)))))

