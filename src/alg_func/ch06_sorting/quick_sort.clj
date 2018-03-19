(ns alg-func.ch06-sorting.quick-sort
  (:gen-class))

;; 6.3.3 quick sort

;; the "naive" version from the book 
;; which performs quite well
(defn qsort [xs]
  (if (empty? xs)
    xs
    (let [pivot (first xs)
          lower (filter #(<= % pivot) (rest xs))
          upper (filter #(> % pivot) (rest xs))]
      (concat (qsort lower) [pivot] (qsort upper)))))

;; this version gets rid of concat (much like the book removes the append in haskell). 
;; however, it does not appear to run faster than qsort
;;
;; (FYI: this slightly departs from qsort' in the book, which takes 2 params. instead, qsort' takes one param
;; and then calls a called qs with 2 params. the qs function is like qsort' in the book
(defn qsort' [xs]
  ((fn qs [xs s]
       (if (empty? xs)
         s
         (let [pivot (first xs)
               lower (filter #(<= % pivot) (rest xs))
               upper (filter #(> % pivot) (rest xs))]
           (qs lower (cons pivot (qs upper s))))))
   xs []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this version uses split to divide lower and upper in one pass.
;; it also avoids concat the way the book avoids append

;; FYI: book has an error in the qsort'' definition in which it uses qsort' in split instead of qsort''
;; see: http://www.iro.umontreal.ca/~lapalme/AlgoFP/errata.html
;; the version below follows the version in the errata, but the functions are outside qsort''
;;
;; on larger lists, qsort'' really does perform significantly faster than qsort and qsort'

(declare qs)

(defn split [pivot xs lower upper s]
          (if (empty? xs)
            (qs lower (cons pivot (qs upper s)))
            (let [x  (first xs)
                  xs (rest xs)]
              (if (< x pivot)
                (recur pivot xs (cons x lower) upper s)
                (recur pivot xs lower (cons x upper) s))))) 
(defn qs [xs s]
           (cond
             (empty? xs) s
             (= 1 (count xs)) (cons (first xs) s) 
             :else (split (first xs) (rest xs) [] [] s)))


(defn qsort'' [l]
  (qs l []))

;;;;;;;;;
  
;; clojure's split-with allows for a single pass (like split) in an easier to understand fashion
;; also, i'm using concat here
(defn qsort''' [coll]
  (if (empty? coll)
    coll
    (let [pivot (first coll)
          [lower upper] (split-with #(< % pivot) (rest coll))]
      (concat (qsort''' lower) [pivot] (qsort''' upper)))))



