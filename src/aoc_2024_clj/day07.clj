(ns aoc-2024-clj.day07
  (:require [aoc-2024-clj.input.day07 :as in]
            [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #": "))
       (map #(vector (Long/parseLong (first %)) (read-string (str "[" (second %) "]"))))))

(defn- || [a b]
  (Long/parseLong (str a b)))

(defn- op-str [op]
  (cond
    (= op +) "+"
    (= op *) "*"
    (= op ||) "||"))

(defn- valid-ops-help [answer operands curr-ans operator curr-ops]
  (cond
    (> curr-ans answer) nil
    (empty? operands) (if (= curr-ans answer) curr-ops nil)
    :else (let [next-ans (operator curr-ans (first operands))
                next-ops (conj curr-ops (op-str operator))]
            (or (valid-ops-help answer (rest operands) next-ans + next-ops)
                (valid-ops-help answer (rest operands) next-ans * next-ops)
                (valid-ops-help answer (rest operands) next-ans || next-ops)))))

(defn valid-ops [answer operands]
  (or (valid-ops-help answer (rest operands) (first operands) + [])
      (valid-ops-help answer (rest operands) (first operands) * [])
      (valid-ops-help answer (rest operands) (first operands) || [])))

;; Both parts (comment out code involving || for part 1).
(def sample-equations (parse-input in/sample-input))
(def equations (parse-input in/input))
(->> (filter #(some? (valid-ops (first %) (second %))) equations)
     (map first)
     (reduce +))
