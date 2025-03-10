(ns aoc-2024-clj.day12
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day12 :as in]))

(defn get-pos [world x y]
  (cond
    (>= x (:width world)) nil
    (< x 0) nil
    (>= y (:height world)) nil
    (< y 0) nil
    :else (nth (nth (:contents world) y) x)))

(defn get-world [input]
  (let [split (str/split-lines input)]
    {:width (count split)
     :height (count (first split))
     :area (* (count split) (count (first split)))
     :contents split}))

(defn edge-count [world x y]
  (let [label (get-pos world x y)
        neighbors [(get-pos world x (inc y))
                   (get-pos world (inc x) y)
                   (get-pos world x (dec y))
                   (get-pos world (dec x) y)]]
    (count (filter #(or (nil? %) (not= % label)) neighbors))))

(defn get-data-help [world perimeters areas plot-num]
  (if (>= plot-num (:area world))
    {:perimeters perimeters
     :areas areas}
    (let [x (mod plot-num (:width world))
          y (int (/ plot-num (:width world)))
          label (get-pos world x y)
          new-areas (assoc areas label (inc (get areas label 0)))
          new-perms (assoc perimeters label (+ (get perimeters label 0)
                                               (edge-count world x y)))]
      (get-data-help world new-perms new-areas (inc plot-num)))))

(defn get-data [world]
  (get-data-help world {} {} 0))

; TODO: need to take adjacency into account! see second sample input.
(defn solve-p1 [input]
  (let [world (get-world input)
        data (get-data world)
        {perms :perimeters
         areas :areas} data
        plots (keys perms)]
    (->> plots
         (map #(* (get perms %) (get areas %)))
         (reduce +))))

(comment
  (get-data (get-world in/sample-input2))
  (solve-p1 in/sample-input2))


