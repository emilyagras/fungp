(ns fungp.test.trees-test
  (:require [fungp.core :as fungp.core]
            [fungp.logic.trees :as trees])
  (:use [clojure.test]))

(defn list-or-cons? [tree]
  (or (list? tree)
      (= clojure.lang.Cons (type tree))))

(def test-functions '[[+ 2][- 2][* 2][inc 1][dec 1]])

(def test-terminals '[x])

(deftest test-create-tree
  (let [tree-type (type (trees/create-tree 5 test-terminals [] test-functions :grow))]
    (is (or (= tree-type clojure.lang.Symbol)
            (= tree-type clojure.lang.Cons)))))

(deftest test-module-tree
  (let [tree (trees/create-module-tree 5 test-terminals [] test-functions 1 1 0 0 :grow)]
    (do (is (list-or-cons? tree))
        (is (= (first tree) 'let))
        (is (vector? (second tree))))))

(deftest test-tree-operations
  (let [tree (trees/create-tree 5 test-terminals [] test-functions :fill)
        subtree (trees/rand-subtree tree)]
    (do (is (seq? tree)))
    (is (seq? subtree))))

(deftest test-truncate
  (let [tree (trees/create-tree 5 test-terminals [] test-functions :fill)]
    (do (is (= tree (trees/truncate tree 10)))
        (is (not (= tree (trees/truncate tree 2)))))))
