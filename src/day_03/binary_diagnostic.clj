(ns day-03.binary-diagnostic
  (:require [clojure.string :as str]))

(defn parse-input-to-vectors [path]
  (let [input (slurp path)
        length (count (first (str/split-lines input)))
        clean-string (-> input
                         (str/replace #"\n" "")
                         (str/split #""))]
    (mapv #(into [] %) (partition length clean-string))))

(defn reorganize-vectors-by-column [input-vectors result-vector index]
  (if (< index (count (first input-vectors)))
    (let [conjoined-vector (conj result-vector (mapv #(nth % index) input-vectors))]
      (reorganize-vectors-by-column input-vectors conjoined-vector (inc index)))
    result-vector))

(defn select-relevant-bit [collection-by-column type & index]
  (let [length (count (first collection-by-column))
        relevant-column (if index
                          (apply nth collection-by-column index)
                          (first collection-by-column))
        number-of-zeros (->> relevant-column
                             (mapv #(when (= "0" %) pop %))
                             (remove nil?)
                             count)
        relevant-bit (cond
                       (and (> number-of-zeros (/ length 2)) (or (= type :epsilon) (= type :co2-scrubber))) "1"
                       (and (< number-of-zeros (/ length 2)) (or (= type :epsilon) (= type :co2-scrubber))) "0"
                       (and (> number-of-zeros (/ length 2)) (or (= type :gamma) (= type :o2-generator))) "0"
                       (and (< number-of-zeros (/ length 2)) (or (= type :gamma) (= type :o2-generator))) "1"
                       (and (= number-of-zeros (/ length 2)) (or (= type :epsilon) (= type :co2-scrubber))) "0"
                       (and (= number-of-zeros (/ length 2)) (or (= type :gamma) (= type :o2-generator))) "1")]
    relevant-bit))

(defn convert-binary-number-vector-to-integer [binary-number-vector]
  (Integer/parseInt (str/replace (apply str binary-number-vector) #"[^0-9]" "") 2))

(defn assemble-power-rate [collection-by-column result-vector type]
  (if (not (empty? collection-by-column))
    (let [relevant-bit (select-relevant-bit collection-by-column type)]
      (assemble-power-rate (rest collection-by-column) (conj result-vector relevant-bit) type))
    (convert-binary-number-vector-to-integer result-vector)))

(defn diagnose-power-consumption [path]
  (let [input-vectors (parse-input-to-vectors path)
        collection-by-column (reorganize-vectors-by-column input-vectors [] 0)
        gamma-power-rate (assemble-power-rate collection-by-column [] :gamma)
        epsilon-power-rate (assemble-power-rate collection-by-column [] :epsilon)]
    (* gamma-power-rate epsilon-power-rate)))

;;; This solves part 1: (diagnose-power-consumption "src/day_03/input.txt")

(defn filter-by-relevant-bit [input-vectors collection-by-column index type]
  (let [relevant-bit (select-relevant-bit collection-by-column type index)
        selected-vectors (mapv (fn [entry]
                                 (if (= relevant-bit (nth entry index)) entry nil))
                               input-vectors)
        nil-entries-removed (remove nil? selected-vectors)
        columns-from-remainder (reorganize-vectors-by-column nil-entries-removed [] 0)]
    (if (= 1 (count nil-entries-removed))
      nil-entries-removed
      (filter-by-relevant-bit nil-entries-removed columns-from-remainder (inc index) type))))

(defn diagnose-life-support [path]
  (let [input-vectors (parse-input-to-vectors path)
        collection-by-column (reorganize-vectors-by-column input-vectors [] 0)
        o2-generator (convert-binary-number-vector-to-integer
                       (filter-by-relevant-bit input-vectors collection-by-column 0 :o2-generator))
        co2-scrubber (convert-binary-number-vector-to-integer
                       (filter-by-relevant-bit input-vectors collection-by-column 0 :co2-scrubber))]
    (* o2-generator co2-scrubber)))

;;; This solves part 2: (diagnose-life-support "src/day_03/input.txt")
