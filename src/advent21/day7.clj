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

(defn all-costs [input]
  (reduce (fn [acc goal-pos]
            (assoc acc goal-pos (total-cost input goal-pos)))
          {}
          (range (apply max input))))


(defn answer1 [input]
  (val (apply min-key val (all-costs input))))

(comment

  (= 37 (answer1 ex))

  "Elapsed time: 29869.995311 msecs"
  (time (answer1 input))

  )
