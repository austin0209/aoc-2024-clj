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
       (flatten)))

(defn blink-windowed [window-size stones]
  (->> (partition window-size window-size '() stones)
       (pmap blink)
       (flatten)))

(def blink-cache (atom {}))

(defn nth-stone-blink [stone n]
  (if (contains? @blink-cache [stone n])
    (get @blink-cache [stone n])
    (let [ns-cached (->> (keys @blink-cache)
                         (filter #(= stone (first %)))
                         (map second))
          last-cached-n (if (empty? ns-cached) 0 (apply max ns-cached))
          cached-stones (get @blink-cache [stone last-cached-n] [stone])]
      (loop [curr-stones cached-stones
             blinks-done last-cached-n]
        (if (= blinks-done n)
          curr-stones
          (let [next-blink (blink curr-stones)
                blinks-done (inc blinks-done)]
            (swap! blink-cache assoc [stone blinks-done] next-blink)
            (recur next-blink blinks-done)))))))

(comment
  (time (last (nth-stone-blink 125 75)))
  @blink-cache
  (reset! blink-cache {}))

(defn solve-p1 [input]
  (count (last (take 26 (iterate blink (parse-input input))))))

(defn solve-p2 [input]
  (let [stones (parse-input input)]
    (loop [curr-stones stones
           n 1]
      (println n)
      (if (= n 26)
        (count curr-stones)
        (let [next-blink (->> (mapv #(nth-stone-blink % n) curr-stones)
                              (flatten))]
          (recur next-blink (inc n)))))))

(comment
  (solve-p1 in/input)
  (solve-p2 in/input)
  (time (count (last (take 26 (iterate (partial blink-windowed 100) (parse-input in/input)))))))

;; Need to memoize against stone # and distance taken!
