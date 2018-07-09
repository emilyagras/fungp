(ns fungp.sample.quadratic
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.pprint))

(defn target-function [x]
  (+ (+ (* x x) (* 2 x)) 1))

(def function-set
  "Functions and their arities"
  '[[+ 2][- 2][* 2][fungp.util/abs 1]
    [inc 1][dec 1]])

(def parameters
  '[x])

(def terminals
  "Lots of number literals to use as terminals"
  '[-1 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 10])

(def test-range
  "The input test cases"
  (map #(float (* (- % 7) 3)) (range 14)))

(def expected-output
  "The output test cases"
  (map target-function test-range))

(defn fitness-function
  "Takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (eval (list 'fn 'testfunc '[x] tree))
          results (map f test-range)]
      (reduce + (map off-by-sq expected-output results)))
    (catch Exception e (println e) (println tree))))

(defn report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (pprint (list 'fn '[a] tree))
  (print (str "Error:\t" fitness "\n\n"))
  (flush))

(defn test-quadratic-function
  [n1 n2]
  (println (str "Test inputs: " (vec test-range)))
  (println (str "Test outputs: " (vec expected-output)))
  (println (str "Max generations: " (* n1 n2)))
  (println)
  (let [options {:iterations n1 :migrations n2 :num-islands 6 
                 :population-size 30
                 :max-depth 5 :terminals parameters 
                 :numbers terminals :fitness fitness-function
                 :functions function-set :report report}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (report tree score))))

