(ns fungp.logic.populations
  (:require [fungp.logic.trees :as trees]
            [fungp.util :as utils]))

;;; #### Populations
;;;
;;; A population is a list of individuals. Creating the population involves randomly generating a list of individuals
;;; as a starting point. Islands, which will be implemented further down, are lists of populations

(defn create-population
  "Creates a population of trees given a population size, mutation depth, terminal
   sequence, and function sequence. It uses a variation of Koza's \"ramped half-and-half\"
   method: a coin flip determines whether to use the \"fill\" or \"grow\" method, and the
   mutation depth is a randomly chosen number between 1 and the specified max mutation depth."
  [population-size mutation-depth terminals numbers functions adf-arity adf-count adl-count adl-limit]
  (if (zero? population-size) []
      (conj (create-population (dec population-size) mutation-depth terminals numbers functions
                               adf-arity adf-count adl-count adl-limit)
            (trees/create-module-tree (inc (rand-int mutation-depth)) terminals numbers functions
                                      adf-arity adf-count adl-count adl-limit
                                      (if (utils/flip 0.5) :grow :fill)))))




;;; Now it's time to get into functions that operate on populations.
;;;
;;; **Selection** is the process in which more fit individuals are "selected," or
;;; more likely to breed (be involved in a crossover), while less fit individuals
;;; are less likely to breed.
;;;
;;; To carry out the selection phase, it's necessary to determine how fit the
;;; individuals are. The following functions use the fitness function to give the
;;; individual trees a grade (I sometimes refer to it as "error"). Lower grades are
;;; better. Then, in the selection phase, individuals with lower error are more
;;; likely to be selected for crossover, and thus pass on their genetic
;;; material to the next generation.
;;;
;;; Note that the fitness function is expected to return *lower* numbers for better results,
;;; with 0 being "perfect." Execution will stop once 0 is reached.

(defn truncate-population
  "Apply truncate to all individuals in a population."
  [population height]
  (map #(trees/truncate-module % height) population))

(defn fitness-zip
  "Compute the fitness of all the trees in the population, and map the trees to their population in a
   seq of a zipmap."
  [population fitness]
  (seq (zipmap population (map fitness population))))

(defn tournament-selection
  "Use tournament selection to create a new generation. In each tournament the two best individuals
   in the randomly chosen group will reproduce to form a child tree. A larger tournament size
   will lead to more selective pressure. The function takes a population, tournament size,
   \"fitness-zip\" or sequence of the zip of trees and fitness scores, and the max depth,
   and it returns a new population."
  [population tournament-size fitness-zip]
  (let [child
        (fn []
          (let [selected (map first (sort-by second
                                             (repeatedly tournament-size
                                                         #(rand-nth fitness-zip))))]
            (trees/crossover-module (first selected)
                                    (second selected))))]
    (repeatedly (count population) child)))

(defn get-best-fitness
  "Takes the fitness zip and finds the best in the population."
  [fitness-zip]
  (first (sort-by second fitness-zip)))

(defn elitism
  "Put the best-seen individual back in the population."
  [population best]
  (conj (rest population) best))

(defn generations
  "Runs n generations of a population, and returns the population and the best tree in the form [population best-tree fitness].
   Takes a long list of parameters. This function is meant to be called by island-generations, which in turn is
   called by run-genetic-programming."
  [n population tournament-size mutation-probability mutation-depth max-depth terminals
   numbers functions fitness]
  (loop [n (int n) population population] ;; optimize inner loop
    (let [computed-fitness (fitness-zip population fitness)
          [best-tree best-fit] (get-best-fitness computed-fitness)]
      (if (or (zero? n) (zero? best-fit)) ;; terminating condition
        [population best-tree best-fit]   ;; return
        (recur (- n 1)                    ;; else recurse
               (-> population
                   (tournament-selection tournament-size computed-fitness)
                   (trees/mutate-population mutation-probability mutation-depth terminals numbers functions)
                   (truncate-population max-depth)
                   (elitism best-tree)))))))
