(ns fungp.logics.logic
  (:require [fungp.util :as utils]))

(defn terminal
  "Return a random terminal or number."
  [terminals numbers]
  (if (and (utils/flip 0.5)
           (not (empty? numbers)))
    (rand-nth numbers)
    (rand-nth terminals)))
