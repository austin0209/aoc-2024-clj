(ns aoc-2024-clj.day03
  (:require [aoc-2024-clj.input.day03 :as in]))

(defn do-mul [instruction]
  (let [matcher (re-matcher #"\d+" instruction)
        num1 (Long/parseLong (re-find matcher))
        num2 (Long/parseLong (re-find matcher))]
    (* num1 num2)))

;; Part 1
(->> (re-seq #"mul\(\d+,\d+\)" in/input)
     (map do-mul)
     (reduce +))

;; Part 2
(->> (re-seq #"do\(\)|don't\(\)|mul\(\d+,\d+\)" in/input)
     (#(loop [instructions %
              enabled true
              answer 0]
         (if (empty? instructions)
           answer
           (let [current (first instructions)]
             (cond
               (= "do()" current) (recur (rest instructions) true answer)
               (= "don't()" current) (recur (rest instructions) false answer)
               (not enabled) (recur (rest instructions) enabled answer)
               :else (recur (rest instructions) enabled (+ answer (do-mul current)))))))))
