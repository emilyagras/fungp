(ns fungp.test.prisoners-dilemma-test
  (:require [fungp.core :as fungp.core]
            [fungp.sample.prisoners-dilemma :as pd])
  (:use [clojure.test]))


(deftest cooperate-test
  (testing "should make the cooperate choice for given player"
    
    (is (= {:player1 "cooperate"} (pd/update-game :player1 "cooperate")))
    (is (= {:player1 "cooperate" :player2 "cooperate"} (pd/update-game :player2 "cooperate") ))))
