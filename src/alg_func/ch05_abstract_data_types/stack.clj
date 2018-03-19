(ns alg-func.ch05-abstract-data-types.stack
  (:gen-class))

;; 5.2 Stacks

;; we use a defrecord to define the Stack type and a protocol to define the functions that we can perform on it

;; stack will be a record that takes a list as a parameter

(defprotocol StackProtocol
  (push [s x]) 
  (pop [s]) 
  (top [s]) 
  (stack-empty? [s]))

(defrecord Stack [l])

(defn empty-stack [] (->Stack [])) ;; book creates a new stack with a function called: emptyStack


(extend-protocol 
  StackProtocol
  Stack
  (push [s x] (->Stack (cons x (:l s))))
  (pop [s] (->Stack (rest (:l s)))) 
  (top [s] (first (:l s))) 
  (stack-empty? [s] (empty? (:l s)))) ;;called "stackEmpty" in the book
  ;;emptyStack is defined as empty-stack outside the protocol

