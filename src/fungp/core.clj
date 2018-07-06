(ns fungp.core
  (:use fungp.util)
  (:use fungp.defined-branches)
  (:require [fungp.logic.logic :as logic]
            [fungp.logic.populations :as populations]
            [fungp.logic.trees :as trees]
            [fungp.logic.islands :as islands]))

;;; Call the **run-genetic-programming** function with a map containing these keyword parameters:
;;;
;;; * iterations : number of iterations *between migrations*
;;; * migrations : number of migrations
;;; * num-islands : number of islands
;;; * population-size : size of the populations
;;; * tournament-size : size of the tournaments (default 5)
;;; * mutation-probability : probability of mutation (default 0.2)
;;; * mutation-depth : depth of mutated trees (default 6)
;;; * max-depth : maximum depth of trees
;;; * terminals : terminals used in tree building
;;; * numbers : number literals to use as terminals (default [])
;;; * functions : functions used in tree building, in the form [function arity]
;;; * fitness : a fitness function that takes a tree and returns an error number, lower is better
;;; * report : a reporting function passed [best-tree best-fit] at each migration
;;; * adf-count : number of automatically defined functions (default 0)
;;; * adf-arity : number of arguments for automatically defined functions (default 1)
;;;
;;; Most fitness functions will *eval* the trees of code they are passed.
;;; In Clojure, eval'ing a list of code will compile it to JVM
;;; bytecode.
;;;
;;; If you're interested primarily in how to *use* fungp, you can skip to the example files.
;;;
;;; Notes on mutable variables and loops
;;; ------------------------------------
;;;
;;; *fungp* is fully capable of evolving code that uses mutable variables, side-effects, and
;;; loops. Currently it does not support Koza's architecture-altering operations so you have
;;; to determine some of the architecture beforehand (like, the names of the variables, whether
;;; you need a loop, etc).
;;;
;;; There also is no built-in mechanism for managing mutable variables --- I would like there
;;; to be, but there are a few obstacles. Clojure doesn't really support mutable local variables.
;;; That's certainly not a bad thing, but it makes it slightly more complicated to create
;;; local variables (or something that acts like them) at runtime. Because of this, I decided to leave
;;; the management of local variables up to the user, which (thanks to dynamic Vars) is fairly
;;; straightforard. See the compile-ants sample for an example of this.
;;;
;;; By itself *fungp* has no "main" method, so running it won't do anything. The samples can be
;;; run individually, and each has a "test-*" function that launches it. I recommend you run
;;; them from the REPL, as they have some settable parameters.
;;;
;;; Here's how you would run the cart example, starting from the fungp directory:
;;;
;;;     > lein repl
;;;     nREPL server started on port 54110
;;;     user=> (use 'fungp.sample.cart)
;;;     nil
;;;     user=> (test-cart 3 6)
;;;     ...
;;;
;;; At that point the cart example will run.
;;;
;;; The code
;;; --------
;;;
;;; The code here in the core.clj file is rather brief (but dense), and it should be readable
;;; in one sitting. To actually use it, you'll likely have to at least read the sample code, and
;;; probably read most of this code as well.
;;;




;;; ### Putting it together
;;;
;;; This takes care of all the steps necessary to complete one generation of the algorithm.
;;; The process can be extended to multiple generations with a simple tail recursive
;;; function.
;;;
;;; There are some extra considerations here. The function should:
;;;
;;;  * stop when a perfect individual has been found, meaning fitness is zero
;;;
;;;  * be resumable, meaning the search can halt, returning information, and that information
;;;    can be passed back in to start the search at the same place





(defn run-genetic-programming
  "This is the entry function. Call this with a map of the parameters to run the genetic programming algorithm."
  [{:keys [iterations migrations num-islands population-size tournament-size mutation-probability
           mutation-depth max-depth terminals functions numbers fitness report adf-arity adf-count
           adl-count adl-limit]
    ;; the :or block here specifies default values for some of the arguments
    :or {tournament-size 3 mutation-probability 0.1 mutation-depth 6 adf-arity 1 adf-count 0
         adl-count 0 adl-limit 25 numbers []}}]
  ;; some basic type checking: most of the parameters must be integers
  (map #(assert (integer? %)) [iterations migrations num-islands population-size tournament-size mutation-probability
                               mutation-depth max-depth adf-arity adf-count adl-count adl-limit])
  ;; report and fitness should be functions
  (map #(assert (fn? %)) [report fitness])
  ;; call island generations
  (islands/island-generations migrations iterations
                              (islands/create-islands num-islands population-size mutation-depth terminals
                                                      numbers functions adf-arity adf-count adl-count adl-limit)
                              tournament-size mutation-probability mutation-depth max-depth terminals
                              numbers functions fitness report))
