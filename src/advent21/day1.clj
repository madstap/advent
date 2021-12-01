(ns advent21.day1
  (:require [clojure.string :as str]))

(def input
  (map parse-long (str/split-lines (slurp "inputs/day1-0.txt"))))

(defn answer1 [input]
  (->> (partition 2 1 input) (filter (partial apply <)) count))

(defn answer2 [input]
  (->> (partition 3 1 input) (map (partial apply +)) answer1))

(comment
  (answer1 input)
  (answer2 input)
  )
