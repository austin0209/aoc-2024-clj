(ns aoc-2024-clj.day04
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day04 :as in]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map vec)))

(defn get-letter
  "Returns nil if out of bounds"
  [letters pos]
  (nth (nth letters (second pos) nil) (first pos) nil))

(defn get-next-letter [current]
  (case current
    \X \M
    \M \A
    \A \S
    :else (throw (Exception.
                  "We should not be calling this function for S!"))))

(defn find-xmas-from-dir [letters pos dir]
  (loop [next-letter \X
         pos pos
         answer 0]
    (let [current (get-letter letters pos)]
      (cond
        (nil? current) 0
        (not= current next-letter) 0
        (and (= next-letter \S) (= current \S)) 1
        (= current next-letter) (recur
                                 (get-next-letter current)
                                 (map + pos dir)
                                 answer)))))

(defn find-all-xmas [letters]
  (let [all-pos (for [x (range (count (first letters)))
                      y (range (count letters))]
                  [x y])]
    (reduce + (map #(+ (find-xmas-from-dir letters % [1 -1])
                       (find-xmas-from-dir letters % [1 0])
                       (find-xmas-from-dir letters % [1 1])
                       (find-xmas-from-dir letters % [-1 -1])
                       (find-xmas-from-dir letters % [-1 0])
                       (find-xmas-from-dir letters % [-1 1])
                       (find-xmas-from-dir letters % [0 -1])
                       (find-xmas-from-dir letters % [0 1])) all-pos))))

(defn find-x-mas-from-pos [letters pos]
  (cond
    (not= \A (get-letter letters pos)) 0
    (and (or (and (= \M (get-letter letters (map + pos [1 1])))
                  (= \S (get-letter letters (map + pos [-1 -1]))))
             (and (= \S (get-letter letters (map + pos [1 1])))
                  (= \M (get-letter letters (map + pos [-1 -1])))))
         (or (and (= \M (get-letter letters (map + pos [1 -1])))
                  (= \S (get-letter letters (map + pos [-1 1]))))
             (and (= \S (get-letter letters (map + pos [1 -1])))
                  (= \M (get-letter letters (map + pos [-1 1])))))) 1
    :else 0))

(defn find-all-x-mas [letters]
  (let [all-pos (for [x (range (count (first letters)))
                      y (range (count letters))]
                  [x y])]
    (reduce + (map #(find-x-mas-from-pos letters %) all-pos))))

;; Part 1
(find-all-xmas (parse-input in/input))

;; Part 2
(find-all-x-mas (parse-input in/input))
