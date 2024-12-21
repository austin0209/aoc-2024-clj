(ns aoc-2024-clj.day09
  (:require [aoc-2024-clj.input.day09 :as in]))

(defn parse-input [input]
  (loop [res []
         idx 0]
    (if (= idx (count input))
      res
      (let [block-count (Long/parseLong (str (nth input idx)))
            id (if (even? idx) (long (/ idx 2)) nil)
            blocks (for [_ (range block-count)] id)]
        (recur (apply conj res blocks) (inc idx))))))

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
  (loop [idx 0]
    (let [id (nth blocks idx)]
      (if (nil? id)
        idx
        (recur (inc idx))))))

(defn compact [init-blocks]
  (loop [move-queue (-> (keep-indexed
                         #(if (nil? %2) nil {:pos %1 :id %2})
                         init-blocks)
                        (reverse))
         res (transient init-blocks)]
    (if (not (has-gaps? res))
      (persistent! res)
      (let [to-move (first move-queue)
            id (:id to-move)
            free-idx (next-free-idx res)]
        (recur (rest move-queue) (-> (assoc! res free-idx id)
                                     (assoc! (:pos to-move) nil)))))))

(require '[com.clojure-goes-fast/clj-async-profiler :as prof])
(prof/profile (compact (parse-input in/input)))

;; Part 1
(comment (->> (parse-input in/input)
              (compact)
              (filter some?)
              (map-indexed vector)
              (reduce #(+ %1 (apply * %2)) 0)))
