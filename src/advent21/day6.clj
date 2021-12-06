(ns advent21.day6
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (->> (re-seq #"\d+" s) (map parse-long)))

(def input
  (parse-input (slurp "inputs/day6.txt")))

(def ex
  (parse-input "3,4,3,1,2"))

(defn fish-next-day [fish]
  (if (zero? fish)
    [6 8]
    [(dec fish)]))

(defn fish-population-next-day [fish-pop]
  (mapcat fish-next-day fish-pop))

(defn fish-population-days [fish-pop]
  (iterate fish-population-next-day fish-pop))

(defn answer1 [input]
  (-> (fish-population-days input) (nth 80) count))

(defn answer2 [input]
  (-> (fish-population-days input) (nth 256) count))

(comment


  (= 5934 (answer1 ex))

  ;; Hangs forever, which makes sense since it's eventually gonna try to make
  ;; a sequence with 26 billion numbers
  (= 26984457539  (answer2 ex))


  "Elapsed time: 1558.196777 msecs"
  (time (answer1 input))

  )
