(ns aoc-2024-clj.day02
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day02 :as in]))

(defn parse-input [input]
  (as-> input i
        (str/split-lines i)
        (map #(read-string (str "[" % "]")) i)))

(defn is-safe-pt1? [levels]
  (let [diffs (->> (partition 2 1 levels)
                   (map #(- (first %) (second %))))
        sign (if (zero? (first diffs))
               1
               (/ (first diffs) (abs (first diffs))))]
    (->> diffs
         (filter #(or (zero? %) (= sign (/ % (abs %)))))
         (filter #(and (>= (abs %) 1) (<= (abs %) 3)))
         (count)
         (= (count diffs)))))

(defn is-safe-pt2? [levels]
  (if (is-safe-pt1? levels)
    true
    (loop [i 0]
      (if (= i (count levels))
        false
        (let [removed (keep-indexed #(when (not= i %1) %2) levels)]
          (if (is-safe-pt1? removed)
            true
            (recur (+ 1 i))))))))

;; Part 1
(count (filter true? (map is-safe-pt1? (parse-input in/input))))

;; Part 2
(count (filter true? (map is-safe-pt2? (parse-input in/input))))
