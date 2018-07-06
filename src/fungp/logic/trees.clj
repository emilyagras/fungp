(ns fungp.logic.trees
  (:require [fungp.util :as utils]
            [fungp.defined-branches :as branches]
            [fungp.logic.logic :as logic]))

;;; ### Tree manipulation
;;;
;;; **rand-subtree** and **replace-subtree** are two of the most important functions.
;;; They define how the trees are modified.
;;;
;;; The basic idea for how I implemented both of them is that recursion can be
;;; used to reduce the problem at each step: given a tree (or a subtree, all of
;;; which have the same form), recurse on a random subtree, along with a
;;; reduced value of n. The base case is when n is zero or the function hits a leaf.
;;;
;;; Additionally, **replace-subtree** uses concat to reconstruct
;;; the tree on its way back up the stack.

(defn rand-subtree
  "Return a random subtree of a list. Takes an optional second parameter that limits
   the depth to go before selecting a crossover point."
  ([tree]
   (rand-subtree tree (rand-int (inc (utils/max-tree-height tree)))))
  ([tree n]
   (if (or (zero? n) (and (seq? tree) (= (count tree) 1)) ;; don't split up (leaf)
           (not (seq? tree))) tree
       (recur (rand-nth (rest tree))
              (rand-int n)))))

(defn replace-subtree
  "Replace a random subtree with a given subtree. Takes an optional second parameter
   that limits the depth to go before selecting a crossover point."
  ([tree sub]
   (replace-subtree tree sub (rand-int (+ 1 (utils/max-tree-height tree)))))
  ([tree sub n]
   (if (or (zero? n) (and (seq? tree) (= (count tree) 1)) ;; don't split up (leaf)
           (not (seq? tree))) sub
       (let [r (+ 1 (rand-int (count (rest tree))))]
         (concat (take r tree)
                 (list (replace-subtree
                        (nth tree r) sub
                        (rand-int n)))
                 (nthrest tree (inc r)))))))

(defn truncate
  "Prevent trees from growing too big by lifting a subtree if the tree height is
   greater than the max tree height."
  [tree height]
  (if (> (utils/max-tree-height tree) height)
    (recur (rand-subtree tree) height)
    tree))

(defn truncate-module
  "A version of truncate that handles branches."
  [tree height]
  (list (first tree)
        (branches/truncate-branch tree truncate height)
        (truncate (nth tree 2) height)))

;;; ### Mutation, crossover, and selection
;;;
;;; With rand-subtree and replace-subtree out of the way, the rest of the
;;; single-generation pass is pretty simple. Mutation and crossover both
;;; can easily be written in terms of rand-subtree and replace-subtree.
;;;
;;; **Mutation** takes a tree and (occasionally) randomly changes part of it.
;;; The idea, like the rest of the fundamental aspects of genetic algorithms,
;;; is taken from nature; when DNA is copied, there is a slight chance of
;;; "mistakes" being introduced in the copy. This can lead to beneficial
;;; changes and increases genetic diversity.


;;; ### Tree creation
;;;
;;; My method of random tree generation is a combination of the "grow" and "fill"
;;; methods of tree building, similar to Koza's "ramped half-and-half" method.


(defn create-tree
  "Build a tree of source code, given a mutation depth, terminal sequence,
   function sequence, and type keyword. The type can be either :grow or :fill.
   The terminal sequence should consist of symbols or quoted code, while elements in the
   function sequence should contain both the function and a number representing
   its arity, in this form: [function arity]."
  [mutation-depth terminals numbers functions gtype]
  ;; conditions: either return terminal or create function and recurse
  (cond (zero? mutation-depth) (logic/terminal terminals numbers)
        (and (= gtype :grow)
             (utils/flip 0.5))       (logic/terminal terminals numbers)
        :else (let [[func arity] (rand-nth functions)]
                (cons func (repeatedly arity
                                       #(create-tree (dec mutation-depth)
                                                     terminals
                                                     numbers
                                                     functions
                                                     gtype))))))


(defn mutate-tree
  "Mutate a tree. Mutation takes one of three forms, chosen randomly: replace a random
   subtree with a newly generated tree, replace a random subtree with a terminal, or
   \"lift\" a random subtree to replace the root. The function takes a tree, mutation rate,
   a mutation depth (max size of new subtrees), terminals, and functions."
  [tree mutation-probability mutation-depth terminals numbers functions]
  (if (utils/flip mutation-probability)
    (let [coin (rand)] ;; random number between 0 and 1
      (cond (< coin 0.33)
            (replace-subtree tree (create-tree mutation-depth terminals numbers functions :grow))
            (< coin 0.66)
            (replace-subtree tree (rand-nth terminals))
            :else (rand-subtree tree)))
    tree))

(defn mutate-module-tree
  "A version of mutate-tree that handles branches. It applies the mutation operation not only to the
   result defining branch but to the automatically defined branches in the let statement, and it preserve
   the overall structure of the tree (the let form)."
  [module-tree mutation-probability mutation-depth terminals numbers functions]
  (if (or (utils/flip 0.5)
          (zero? (count (second module-tree))))
    ;; mutate the main branch
    (concat (take 2 module-tree)
            (list (mutate-tree (nth module-tree 2) mutation-probability
                               mutation-depth terminals numbers functions)))
    (concat (list (nth module-tree 0))
            (list (vec (map (fn [letf]
                              ;; mutate the branches
                              (branches/mutate-branch letf mutate-tree
                                                      mutation-probability mutation-depth
                                                      terminals numbers functions))
                            (nth module-tree 1))))
            (list (nth module-tree 2)))))

(defn mutate-population
  "Apply mutation to every tree in a population. Similar arguments to mutate-tree."
  [population mutation-probability mutation-depth terminals numbers functions]
  (map #(mutate-module-tree % mutation-probability mutation-depth terminals
                            numbers functions) population))

;;; **Crossover** is the process of combining two parents to make a child.
;;; It involves copying the genetic material (in this case, lisp code) from
;;; the two parents, combining them, and returning the result of the combination.

(defn crossover
  "The crossover function is simple to define in terms of replace-subtree
   and rand-subtree. Basically, crossing over two trees involves selecting a
   random subtree from one tree, and placing it randomly in the other tree."
  [tree1 tree2] (replace-subtree tree1 (rand-subtree tree2)))

(defn crossover-module
  "Crossover that preserves the let form and branches."
  [tree1 tree2]
  (if (or (utils/flip 0.5)
          (zero? (count (second tree1))))
    (let [new-subtree (crossover (nth tree1 2) (nth tree2 2))]
      (list (first tree1) (vec (second tree1)) new-subtree))
    (let [cross-branch (+ 1 (* 2 (rand-int (/ (count (second tree1))))))]
      (list (first tree1)
            (vec (branches/crossover-branch cross-branch crossover
                                            (second tree1) (second tree2)))
            (nth tree1 2)))))


;;; #### Branches
;;;
;;; This is a good time to introduce one of the defining features of the code evolved
;;; in fungp: it consists of multiple ordered branches, the last of which is the
;;; Result Defining Branch, or the branch that actually returns the final result.
;;; Other branches in the evolved code are stored in a let statement, which in
;;; Clojure binds and evaluates sequentially.
;;;
;;; Most likely the type of branch you'll find most useful are Automatically Defined
;;; Functions. These are branches that are separate functions. They are exposed to
;;; the main branch, so the main branch can call the separately evolved functions,
;;; passing arguments to them.
;;;
;;; In addition to the ADFs, I implement a couple of other types of architecture
;;; described by Koza. I don't, however, implement architecture altering operations,
;;; so whatever architecture your individuals have must be determined beforehand.

(defn create-module-tree
  "This is a version of create-tree that handles multiple branches. It builds a let form that has a (potentially empty)
   vector for automatically defined functions and loops, and it has a main branch that is the result defining
   branch."
  [mutation-depth terminals numbers functions adf-arity adf-count adl-count adl-limit type]
  (list 'let
        ;; create the vector of branches
        (branches/build-branch-vector create-tree mutation-depth terminals numbers functions
                                      adf-arity adf-count adl-count adl-limit)
        ;; create the main branch
        (create-tree mutation-depth (concat terminals (branches/gen-adl-terminals adl-count)) numbers
                     ;; add the branches to the function vector
                     (concat functions (branches/gen-adf-func adf-count adf-arity))
                     type)))
