(ns advent-of-code-2021.01.sonar-sweep
  (:require [advent-of-code-2021.01.input :refer [input-data]]))

(defn increased? [depths hits]
  (cond
    (empty? (rest depths))
    hits

    (> (second depths) (first depths))
    (increased? (rest depths) (inc hits))

    :else
    (increased? (rest depths) hits)))

(defn reduce-input-noise [input-data]
  (mapv #(reduce + %) (partition 3 1 input-data)))

(println (increased? (reduce-input-noise input-data) 0))
