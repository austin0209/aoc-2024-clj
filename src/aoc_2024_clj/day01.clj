(ns aoc-2024-clj.day01
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day01 :as in]))

(defn process-input [input]
  (let [lines (str/split-lines input)
        pairs (->> lines
                   (map #(str/split % #"\s+"))
                   (map #(list (Integer/parseInt (first %)) (Integer/parseInt (second %)))))]
    {:left (map first pairs)
     :right (map last pairs)}))

(defn get-frequency [value coll]
  (->> coll
       (filter #(== % value))
       (count)))

(defn solve-day-1-pt1 [input]
  (let [lists (process-input input)
        left-sorted (sort (:left lists))
        right-sorted (sort (:right lists))
        dists (->> (map - left-sorted right-sorted)
                   (map abs))]
    (reduce + dists)))

(defn solve-day-1-pt2 [input]
  (let [lists (process-input input)
        sims (map #(* % (get-frequency % (:right lists))) (:left lists))]
    (reduce + sims)))

(solve-day-1-pt2 in/input)
