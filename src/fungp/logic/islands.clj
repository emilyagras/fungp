(ns fungp.logic.islands
  (:require [fungp.logic.populations :as populations]))

;;; ### Islands
;;;
;;; The above code works for running generations of a single population. The concept of islands is
;;; to have multiple separated populations evolving in parallel, and cross over between them.
;;;
;;; Thanks to clojure's parallelism features, the islands actually do run in parallel. To an extent,
;;; anyway: they're processed with a thread pool of reasonable size ("reasonable size" being the key
;;; phrase -- clojure decides based on your availabe resources).
;;;
;;; So, islands do two things.
;;;
;;;  * Better results: by separating individuals for part of their evolution, and recombining them
;;;    occasionally, we get (hopefully) more diversity.
;;;
;;;  * Better performance: by exploiting multiple CPU cores with native threads, the program can
;;;    process more individuals quickly.

(defn create-islands
  "Create a list of populations (islands)."
  [num-islands population-size mutation-depth terminals numbers functions adf-arity adf-count
   adl-count adl-limit]
  (repeatedly num-islands #(populations/create-population population-size mutation-depth terminals numbers
                                                          functions adf-arity adf-count adl-count adl-limit)))

(defn island-crossover
  "Individuals migrate between islands."
  [islands]
  (let [cross (map rand-nth islands)]
    (map (fn [[island selected]] (conj (rest (shuffle island)) selected))
         (zipmap islands cross))))

;;; And... *drum roll*

(defn island-generations
  "Run generations on all the islands and cross over between them. See the documentation for the generations function.
   Returns with the form [island best-tree best-fit]."
  [n1 n2 islands tournament-size mutation-probability mutation-depth max-depth terminals
   numbers functions fitness report]
  (loop [n (int n1) islands islands] ;; optimize inner loop
    (let [islands-fit (pmap #(populations/generations n2 % tournament-size mutation-probability
                                                      mutation-depth max-depth terminals numbers
                                                      functions fitness)
                            (if (> (count islands) 1) (island-crossover islands) islands))
          islands (map first islands-fit)
          [_ best-tree best-fit] (first (sort-by #(nth % 2) islands-fit))]
      (if (or (zero? n) (zero? best-fit))
        [islands best-tree best-fit]
        (do (report best-tree best-fit)
            (recur (- n 1) islands))))))

;;; ### Wrap it up
;;;
;;; This is the final function, the one to be called from outside. It takes a key->value hash as a parameter, so the
;;; options can be provided in any order and some have default values. See the top of this file for a complete
;;; explanation of each of the keyword parameters.
