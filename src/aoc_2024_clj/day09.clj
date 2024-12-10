(ns aoc-2024-clj.day09
  (:require [aoc-2024-clj.input.day09 :as in]))

(defn parse-input [input]
  (loop [res ""
         idx 0]
    (cond
      (= idx (count input)) (vec res)
      (even? idx) (let [block-count (Long/parseLong (str (nth input idx)))
                        id (long (/ idx 2))
                        blocks (for [_ (range block-count)] id)]
                    (recur (vec (concat res blocks)) (inc idx)))
      (odd? idx) (let [block-count (Long/parseLong (str (nth input idx)))
                       block-str (for [_ (range block-count)] nil)]
                   (recur (vec (concat res block-str)) (inc idx))))))

(defn has-gaps? [blocks]
  (loop [idx 0
         start-found false]
    (cond
      (= idx (dec (count blocks)))
      false

      (and (not start-found)
           (number? (nth blocks idx))
           (nil? (nth blocks (inc idx))))
      (recur (inc idx) true)

      (and start-found
           (nil? (nth blocks idx))
           (number? (nth blocks (inc idx))))
      true

      :else (recur (inc idx) start-found))))

(defn next-free-idx [blocks]
  (first (keep-indexed #(when (nil? %2) %1) blocks)))

(defn compact [init-blocks]
  (loop [move-queue (-> (keep-indexed
                         #(if (nil? %2) nil {:pos %1 :id %2})
                         init-blocks)
                        (reverse))
         res init-blocks]
    (if (not (has-gaps? res))
      res
      (let [to-move (first move-queue)
            id (:id to-move)
            free-idx (next-free-idx res)]
        (recur (rest move-queue) (-> (assoc res free-idx id)
                                     (assoc (:pos to-move) nil)))))))

;; Part 1
(->> (parse-input in/input)
     (compact)
     (filter some?)
     (map-indexed vector)
     (reduce #(+ %1 (apply * %2)) 0))
