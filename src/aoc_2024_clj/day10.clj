(ns aoc-2024-clj.day10
  (:require [aoc-2024-clj.input.day10 :as in]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [split (str/split-lines input)
        width (count (first split))
        height (count split)
        flat (apply str split)]
    {:map flat :width width
     :height height}))

(defn in-bounds? [x y world]
  (let [width (:width world)
        height (:height world)]
    (and (>= x 0) (< x width) (>= y 0) (< y height))))

(defn elevation [x y world]
  (if (in-bounds? x y world)
    (-> (nth (:map world) (+ (* y (:width world)) x))
        (str)
        (Long/parseLong))
    nil))

(defn can-continue? [pos next-pos world]
  (if (in-bounds? (first next-pos) (second next-pos) world)
    (let [current (elevation (first pos) (second pos) world)
          next (elevation (first next-pos) (second next-pos) world)
          diff (- next current)]
      (= 1 diff))
    false))

(defn next-positions [x y world]
  (let [pos [x y]
        top (map + pos [0 -1])
        bot (map + pos [0 1])
        left (map + pos [-1 0])
        right (map + pos [1 0])]
    (->> [top bot left right]
         (filter #(can-continue? [x y] % world)))))

(defn nines-reached [x y nines world]
  (let [next-positions (next-positions x y world)]
    (cond
      (= (elevation x y world) 9) (conj nines [x y])
      (empty? next-positions) nines
      :else (->> next-positions
                 (map #(nines-reached
                        (first %) (second %)
                        nines
                        world))
                 (reduce set/union nines)))))

(def distinct-trails
  (memoize
   (fn [x y world]
     (let [next-positions (next-positions x y world)]
       (cond
         (= (elevation x y world) 9) 1
         (empty? next-positions) 0
         :else (->> next-positions
                    (map #(distinct-trails
                           (first %) (second %)
                           world))
                    (reduce +)))))))

;; Part 1
(let [world (parse-input in/input)
      {w :width h :height} world
      trail-starts (for [x (range w)
                         y (range h)
                         :when (= 0 (elevation x y world))] [x y])]
  (->> trail-starts
       (map #(nines-reached (first %) (second %) #{} world))
       (map count)
       (reduce +)))

;; Part 2
(let [world (parse-input in/input)
      {w :width h :height} world
      trail-starts (for [x (range w)
                         y (range h)
                         :when (= 0 (elevation x y world))] [x y])]
  (->> (map #(distinct-trails (first %) (second %) world) trail-starts)
       (reduce +)))
