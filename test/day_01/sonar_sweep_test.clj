(ns day-01.sonar-sweep-test
  (:require [clojure.test :refer :all]
            [day-01.sonar-sweep :as sonar-sweep]))

(def example-vector [199 200 208 210 200 207 240 269 260 263])

(deftest should-match-examples-on-exercise-subject
  (is (= (sonar-sweep/increased? example-vector 0) 7)
      "First challenge. Should return 7")

  (is (= (sonar-sweep/increased? (sonar-sweep/reduce-input-noise example-vector) 0) 5)
      "Second challenge. Should return 5"))
