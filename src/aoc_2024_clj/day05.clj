(ns aoc-2024-clj.day05
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day05 :as in]))

(defn parse-rules [rules-input]
  (->> (str/split-lines rules-input)
       (map #(str/split % #"\|"))
       (map #(vector (Long/parseLong (first %)) (Long/parseLong (second %))))))

(defn parse-updates [updates-input]
  (->> (str/split-lines updates-input)
       (map #(str/split % #","))
       (map (fn [pages] (map #(Long/parseLong %) pages)))))

(defn updates-valid? [rules pages]
  (loop [pages pages]
    (if (empty? pages)
      true
      (let [current (first pages)
            relevant-rules (filter #(= (second %) current) rules)
            not-allowed-after (set (map first relevant-rules))
            violators (filter #(contains? not-allowed-after %) (rest pages))]
        (if (= 0 (count violators))
          (recur (rest pages))
          false)))))

(defn- insert-nth [coll n value]
  (let [[left right] (split-at n coll)]
    (concat left [value] right)))

(defn correct-order [rules pages]
  (loop [ans '()
         pages pages]
    (if (empty? pages)
      ans
      (let [current (first pages)
            possibilities (for [i (range (+ 1 (count ans)))]
                            (insert-nth ans i current))
            valid (some #(when (updates-valid? rules %) %) possibilities)]
        (assert (some? valid) "There is no way to abide by the rules!")
        (recur valid (rest pages))))))

;; Part 1
(let [split (str/split in/input #"\n\n")
      rules (parse-rules (first split))
      updates (parse-updates (second split))]
  (->> (filter #(updates-valid? rules %) updates)
       (map #(nth % (/ (count %) 2)))
       (reduce +)))

;; Part 2
(let [split (str/split in/input #"\n\n")
      rules (parse-rules (first split))
      updates (parse-updates (second split))]
  (->> (filter #(not (updates-valid? rules %)) updates)
       (map #(correct-order rules %))
       (map #(nth % (/ (count %) 2)))
       (reduce +)))
