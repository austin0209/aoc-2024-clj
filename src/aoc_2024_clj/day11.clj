(ns aoc-2024-clj.day11
  (:require [aoc-2024-clj.input.day11 :as in]))

(defn parse-input [input]
  (read-string (str "(" input ")")))

(defn split-stone [stone]
  (let [stone-str (str stone)
        len (count stone-str)]
    (map #(Long/parseLong %)
         [(subs stone-str 0 (/ len 2))
          (subs stone-str (/ len 2))])))

(defn process-stone [stone]
  (cond
    (= 0 stone) 1
    (even? (count (str stone))) (split-stone stone)
    :else (* 2024 stone)))

(defn blink [stones]
  (->> (map process-stone stones)
       (flatten)
       (vec)))

(defn solve-p1 [input]
  (count (last (take 26 (iterate blink (parse-input input))))))

(comment
  (solve-p1 in/input))
