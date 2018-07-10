(ns fungp.test.robot-test
  (:require [fungp.core :as fungp.core]
            [fungp.sample.robot :as robots])
  (:use [clojure.test]))


(deftest generate-coins-list-test
  (testing "should make coin count many coins"
    (let [output (robots/generate-coins-list)]
      (is (= robots/COIN_COUNT (count output))))))
