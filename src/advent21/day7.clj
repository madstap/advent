(ns advent21.day7
  (:require [clojure.java.math :as math]))

(defn parse-input [s]
  (->> (re-seq #"\d+" s) (mapv parse-long)))

(def input
  (parse-input (slurp "inputs/day7.txt")))

(def ex
  (parse-input "16,1,2,0,4,2,7,1,2,14"))

(defn cost [sub-pos goal-pos]
  (math/abs (- sub-pos goal-pos)))

(defn total-cost [subs goal-pos]
  (transduce (map #(cost % goal-pos)) + subs))

(defn cost2 [sub-pos goal-pos]
  (apply + (range (inc (cost sub-pos goal-pos)))))

(defn total-cost2 [subs goal-pos]
  (transduce (map #(cost2 % goal-pos)) + subs))

(defn avg [xs]
  (/ (apply + xs) (count xs)))

(defn median [xs]
  (let [sorted (sort xs)]
    (if (odd? (count xs))
      (nth sorted (math/floor (/ (count xs) 2)))
      (avg (take 2 (drop (dec (/ (count xs) 2)) sorted))))))

(defn answer1 [input]
  (total-cost input (median input)))

(defn answer2 [input]
  (min (total-cost2 input (math/ceil (avg input)))
       (total-cost2 input (math/floor (avg input)))))

(comment

  (= 37 (answer1 ex))
  (= 168 (answer2 ex))

  "Elapsed time: 29869.995311 msecs"
  "Elapsed time: 17.828081 msecs"
  (= 328187 (time (answer1 input)))

  "Elapsed time: 19865.854694 msecs"
  "Elapsed time: 25.577696 msecs"
  (= 91257582 (time (answer2 input)))
  )
