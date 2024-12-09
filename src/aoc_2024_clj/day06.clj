(ns aoc-2024-clj.day06
  (:require [clojure.string :as str]
            [aoc-2024-clj.input.day06 :as in]))

(defn- get-obstructions [flat width]
  (loop [last-idx 0
         ans (transient #{})]
    (let [idx (str/index-of flat \# (+ 1 last-idx))]
      (if (nil? idx)
        (persistent! ans)
        (let [x (mod idx width)
              y (long (Math/floor (/ idx width)))]
          (recur idx (conj! ans [x y])))))))

(defn parse-input [input]
  (let [split (str/split-lines input)
        flat (apply str split)
        width (count (first split))
        height (count split)
        guard-idx (str/index-of flat \^)
        guard-x (mod guard-idx width)
        guard-y (long (Math/floor (/ guard-idx width)))]
    {:guard-pos [guard-x guard-y]
     :guard-facing :up ;; We are assuming the guard always starts facing up
     :obstructions (get-obstructions flat width)
     :width width
     :height height}))

(defn- get-front-pos [current facing]
  (case facing
    :up (map + current [0 -1])
    :down (map + current [0 1])
    :left (map + current [-1 0])
    :right (map + current [1 0])))

(defn- rotate-90 [facing]
  (case facing
    :up :right
    :down :left
    :left :up
    :right :down))

(defn in-bounds? [state pos]
  (let [x (first pos)
        y (second pos)
        {width :width height :height} state]
    (and (>= x 0) (< x width) (>= y 0) (< y height))))

(defn tick [state]
  (let [{facing :guard-facing pos :guard-pos} state
        front-pos (get-front-pos pos facing)
        obstacle-in-front (contains? (:obstructions state) front-pos)
        new-facing (if obstacle-in-front (rotate-90 facing) facing)
        new-pos (if obstacle-in-front pos front-pos)]
    (-> state
        (assoc :guard-pos (if (in-bounds? state new-pos) new-pos nil))
        (assoc :guard-facing new-facing))))

(defn unique-positions [state]
  (loop [positions (transient #{})
         curr-state state]
    (if (nil? (:guard-pos curr-state))
      (persistent! positions)
      (recur (conj! positions (:guard-pos curr-state)) (tick curr-state)))))

(defn is-loop? [state]
  (loop [guard-states (transient #{})
         curr-state (tick state)]
    (let [{pos :guard-pos facing :guard-facing} curr-state
          new-guard-state {:guard-pos pos
                           :guard-facing facing}]
      (cond
        (nil? pos) false ; Guard left map, this is not a loop.
        (contains? guard-states new-guard-state) true
        :else (recur (conj! guard-states new-guard-state) (tick curr-state))))))

(defn all-loop-states [state]
  (let [obs (:obstructions state)
        tries (for [t (unique-positions state)
                    :when (not= t (:guard-pos state))]
                (assoc state :obstructions (conj obs t)))
        states (pmap #(when (is-loop? %) %) tries)]
    (filter some? states)))

;; Part 1
(count (unique-positions (parse-input in/input)))

;; Part 2
(count (all-loop-states (parse-input in/input)))
