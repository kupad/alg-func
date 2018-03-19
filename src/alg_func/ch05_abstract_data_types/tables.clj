(ns alg-func.ch05-abstract-data-types.tables
  (:gen-class))

;; 5.6 Tables

(defprotocol TableProtocol
  (find-table [t i])
  (update-table [t e]))

;; 5.6.1 Implementing table as a function

; not using the protocol here (I don't think it's possible to extend-protocol to functions)

(defn find-fn-table [f i] (f i))

(defn update-fn-table [f i x]
  (fn [j]
      (cond 
        (== j i) x
        (nil? f) nil
        :else (f j))))

(defn create-fn-table [i x] (update-fn-table nil i x))

;; list table

; takes a list of (index, value) pairs
(defrecord ListTable [l])

(defn new-list-table [l]
  (->ListTable l))

(extend-protocol
  TableProtocol
  ListTable
  (find-table [t i]
    (let [ [[j v] & r :as l] (:l t)]
      (if (empty? l)
        nil
        (if (== i j)
          v
          (recur (new-list-table r) i)))))
  ; this implementation deviates just a bit from the book a bit, but is similar
  ; our update-list works on just the internal list of the ListTable. We take the result and return a new ListTable from it
  (update-table [t e]
    (let [update-list (fn ul [ [[j _ :as e'] & r :as l] [i _ :as e] ]
                          (cond
                            (empty? l) (cons e l)
                            (== j i) (cons e r)
                            :else (cons e' (ul r e))))]
      (->ListTable (update-list (:l t) e))))) 

;; "array implementation"
;; since vectors start at 0, it's practically just a vector. it's pretty silly

(defrecord ArrayTable [v])

;; this takes a list of [i,v] pairs
(defn new-array-table [coll]
  (->ArrayTable (into []  (map first coll))))

(extend-protocol
  TableProtocol
  ArrayTable
  (find-table [t i] (nth (:v t) i))
  (update-table [t [i v]] (->ArrayTable (assoc (:v t) i v))))


