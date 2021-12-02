(ns day-02.dive-test
  (:require [clojure.test :refer :all]
            [day-02.dive :as dive]))

(def example-path "test/day_02/example_input.txt")

(def parsed-vector-path (dive/parse-input-into-vector-of-maps example-path))

(deftest should-match-examples-on-exercise-subject
  (is (= (dive/multiply-distance-per-depth example-path) 150)
      "First challenge. Should return 150")

  (is (= (dive/calculate-values-with-aim parsed-vector-path 0 0 0) 900)
      "Second challenge. Should return 900"))
