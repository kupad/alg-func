(ns alg-func.ch06-sorting.merge-sort
  (:gen-class))

;; 6.3.4 merge sort

;; merge two sorted collections
(defn merge [a b]
  (cond
    (and (empty? a) (not-empty b)) b
    (and (not-empty a) (empty? b)) a
    :else
      (let [[x & xs] a
            [y & ys] b]
        (if (<= x y)
          (cons x (merge xs b))
          (cons y (merge a ys))))))

(defn msort [xs]
  (cond
    (empty? xs) xs 
    (= 1 (count xs)) xs 
    :else 
      (let [k (/ (count xs) 2)  ;;we could have used clojure's split-at here, but I'm following the book
            xs1 (take k xs)
            xs2 (drop k xs)]
        (merge (msort xs1) (msort xs2)))))

;;;;;;;;;;;;;;;
;; bottom up merge-sort (msort'')

;; splits a collection into a singleton collections
(defn split [xs]
  (if (empty? xs)
    xs
    (cons [(first xs)] (split (rest xs)))))

(defn mergepairs [xs]
  (cond
    (empty? xs) xs
    (= 1 (count xs)) xs
    :else 
      (let [l1 (first xs)
            l2 (second xs)
            rest  (rest (rest xs))] 
        (cons (merge l1 l2) (mergepairs rest)))))

;; it's a bit harder to follow the naming conventions the book uses for the definition in clojure
(defn msort'' [xs]
  (loop [l (split xs)]
        (if (= 1 (count l))
          (first l)
          (recur (mergepairs l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The above versions from the book are not tail recursive optimized, here is my attempt to make them tail recursive

(defn merge-tr [a b]
  (loop [a a
         b b
         acc []]
    (cond
      (and (empty? a) (not-empty b)) (into acc b)
      (and (not-empty a) (empty? b)) (into acc a)
      (<= (first a) (first b)) (recur (rest a) b (conj acc (first a)))
      :else                   (recur a (rest b) (conj acc (first b))))))

(defn split-tr [xs]
  (loop [xs xs
         acc []]
        (if (empty? xs) 
          acc
          (recur (rest xs) (conj acc [(first xs)])))))

(defn mergepairs-tr [xs]
  (loop [xs xs
         acc []]
        (cond
          (empty? xs) acc
          (= 1 (count xs)) (into acc xs)
          :else 
            (let [l1 (first xs)
                  l2 (second xs)
                  rest (rest (rest xs))] 
              (recur rest (cons (merge-tr l1 l2) acc))))))

;; this one is exactly like msort'', except it uses the tail recursive versions of our functions
(defn msort''-tr [xs]
  (loop [l (split-tr xs)]
        (if (= 1 (count l))
          (first l)
          (recur (mergepairs-tr l)))))

