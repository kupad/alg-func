(ns alg-func.ch06-sorting.tree-sort
  (:require [alg-func.ch05-abstract-data-types.bst :as bst]) 
  (:require [alg-func.ch05-abstract-data-types.avl :as avl]) 
  (:gen-class))

;; 6.5.6 Tree sort

;; using bst
;; (this implementation is quite fast in practice)
(defn tsort [coll]
  (bst/in-order (bst/build-tree coll)))

;; using avl
(defn tsort' [coll]
  (avl/in-order (avl/build-tree coll)))


