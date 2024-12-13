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
    (= 0 stone) [1]
    (even? (count (str stone))) (split-stone stone)
    :else [(* 2024 stone)]))

(defn blink [stones]
  (mapcat process-stone stones))

(def nth-count
  (memoize
    (fn [stone n]
      (cond
        (= n 0) 1
        (= 0 stone) (nth-count 1 (dec n))
        (even? (count (str stone)))
        (let [split (split-stone stone)]
          (+ (nth-count (first split) (dec n))
             (nth-count (second split) (dec n))))
        :else (nth-count (* 2024 stone) (dec n))))))

(defn solve-p1 [input]
  (count (last (take 26 (iterate blink (parse-input input))))))

(defn solve-p2 [input]
  (->> (parse-input input)
       (map #(nth-count % 75))
       (apply +)))

(comment
  (solve-p1 in/input)
  (solve-p2 in/input)
  (nth-count 125 6))
