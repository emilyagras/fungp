(ns fungp.sample.prisoners-dilemma
  (:require [fungp.core :as fungp]
            [fungp.util :as utils]))

(def TOTAL_TURNS 2)

(def game (atom {}))

(defn update-game [player decision]
   (swap! game assoc player decision))

(defn cooperate [player]
  (update-game {player "cooperate"}))
