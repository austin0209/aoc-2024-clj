(ns aoc-2024-clj.day08
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day08 :as in]))

(defn- get-antennae [flat width]
  (loop [idx 0
         antennae []]
    (if (= idx (count flat))
      antennae
      (if (= \. (nth flat idx))
        (recur (+ 1 idx) antennae)
        (recur (+ 1 idx)
               (conj antennae {:freq (nth flat idx)
                               :pos [(mod idx width)
                                     (long (Math/floor (/ idx width)))]}))))))

(defn parse-input [input]
  (let [split (str/split-lines input)
        flat (apply str split)
        width (count (first split))
        height (count split)]
    {:antennae (get-antennae flat width)
     :width width
     :height height}))

(defn in-bounds? [pos width height]
  (let [x (first pos)
        y (second pos)]
    (and (>= x 0) (< x width) (>= y 0) (< y height))))

(defn get-antinodes [pos1 pos2]
  (let [x1 (first pos1) y1 (second pos1)
        x2 (first pos2) y2 (second pos2)
        dx (- x1 x2)
        dy (- y1 y2)]
    [[(+ x1 dx) (+ y1 dy)] [(- x2 dx) (- y2 dy)]]))

(defn get-antinodes2 [pos1 pos2 w h]
  (let [x1 (first pos1) y1 (second pos1)
        x2 (first pos2) y2 (second pos2)
        dx (- x1 x2)
        dy (- y1 y2)]
    (loop [anode1 [(+ x1 dx) (+ y1 dy)]
           anode2 [(- x2 dx) (- y2 dy)]
           result (transient [pos1 pos2])]
      (if (every? #(not (in-bounds? % w h)) [anode1 anode2])
        (persistent! result)
        (recur [(+ (first anode1) dx) (+ (second anode1) dy)]
               [(- (first anode2) dx) (- (second anode2) dy)]
               (reduce conj! result [anode1 anode2]))))))

(defn get-all-antinodes [antennae get-antinode-fn]
  (loop [freqs (transient (vec (set (map :freq antennae))))
         antinodes (transient #{})]
    (if (= 0 (count freqs))
      (persistent! antinodes)
      (let [curr-freq (get freqs (- (count freqs) 1))
            relevant-pos (map :pos (filter #(= (:freq %) curr-freq) antennae))
            pairs (for [idx1 (range (- (count relevant-pos) 1))
                        idx2 (range (+ 1 idx1) (count relevant-pos))]
                    [idx1 idx2])
            new-antinodes (apply concat (map #(get-antinode-fn
                                               (nth relevant-pos (first %))
                                               (nth relevant-pos (second %))) pairs))]
        (recur (pop! freqs) (reduce conj! antinodes new-antinodes))))))

;; Part 1
(let [{ants :antennae
       w :width
       h :height} (parse-input in/input)]
  (->> (get-all-antinodes ants get-antinodes)
       (filter #(in-bounds? % w h))
       (count)))

;; Part 2
(let [{ants :antennae
       w :width
       h :height} (parse-input in/input)]
  (->> (get-all-antinodes ants #(get-antinodes2 %1 %2 w h))
       (filter #(in-bounds? % w h))
       (count)))
