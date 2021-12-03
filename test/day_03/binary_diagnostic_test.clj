(ns day-03.binary-diagnostic-test
  (:require [clojure.test :refer :all]
            [day-03.binary-diagnostic :as bin-diag]))

(def example-path "test/day_03/example_input.txt")

(deftest should-match-examples-on-exercise-subject
  (is (= (bin-diag/diagnose-power-consumption example-path) 198)
      "First challenge. Should return 198")

  (is (= (bin-diag/diagnose-life-support example-path) 230)
      "First challenge. Should return 230"))
