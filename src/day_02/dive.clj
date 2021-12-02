(ns day-02.dive
  (:require [clojure.string :as str]))

(defn parse-input-into-vector-of-maps [path]
  (let [input-text (str/split-lines (slurp path))]
    (->> input-text
         (map #(clojure.string/split % #" "))
         (map (fn [[k v]] [(keyword k) (Integer/parseInt v)]))
         (mapv #(apply hash-map %)))))

(defn multiply-distance-per-depth [path]
  (let [data-map (->> path
                      parse-input-into-vector-of-maps
                      (apply merge-with +))
        distance (:forward data-map)
        depth (- (:down data-map) (:up data-map))]
    (* distance depth)))

(defn calculate-values-with-aim [input-vector position depth aim]
  (let [instruction (first input-vector)]
    (cond
      (contains? instruction :forward)
      (calculate-values-with-aim (rest input-vector) (+ position (:forward instruction)) (+ (* aim (:forward instruction)) depth) aim)

      (contains? instruction :up)
      (calculate-values-with-aim (rest input-vector) position depth (- aim (:up instruction)))

      (contains? instruction :down)
      (calculate-values-with-aim (rest input-vector) position depth (+ aim (:down instruction)))

      :else
      (* position depth))))

;;; This solves the challenge: (println (calculate-values-with-aim (parse-input-into-vector-of-maps "src/day_02/input.txt") 0 0 0))
