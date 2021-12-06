(ns advent21.day6
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (->> (re-seq #"\d+" s) (map parse-long) frequencies))

(def input
  (parse-input (slurp "inputs/day6.txt")))

(def ex
  (parse-input "3,4,3,1,2"))

(defn decrement-age [fish-pop]
  (->> (range 8)
       (map (juxt identity #(get fish-pop (inc %) 0)))
       (into {})))

(defn fish-population-next-day [{spawning 0, :as fish-pop}]
  (-> (decrement-age fish-pop)
      (update 6 (fnil + 0) (or spawning 0))
      (assoc 8 (or spawning 0))))

(defn fish-population-days [fish-pop]
  (iterate fish-population-next-day fish-pop))

(defn total-pop [fish-pop]
  (apply + (vals fish-pop)))

(defn answer1 [input]
  (-> (fish-population-days input) (nth 80) total-pop))

(defn answer2 [input]
  (-> (fish-population-days input) (nth 256) total-pop))

(comment
  (= 5934 (answer1 ex))
  (= 26984457539  (answer2 ex))

  (= 379114 (answer1 input))
  (= 1702631502303 (answer2 input))
  )
