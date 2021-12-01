(ns day-01.sonar-sweep
  (:require [day-01.input :refer [input-data]]))

(defn increased? [depths hits]
  (cond
    (empty? (rest depths))
    hits

    (> (second depths) (first depths))
    (increased? (rest depths) (inc hits))

    :else
    (increased? (rest depths) hits)))

(defn reduce-input-noise [input]
  (mapv #(reduce + %) (partition 3 1 input)))

;;; This solves the challenge: (println (increased? (reduce-input-noise input-data) 0))
