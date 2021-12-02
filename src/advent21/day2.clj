(ns advent21.day2
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(let [[cmd n] (str/split % #"\s+")]
               [(keyword cmd) (parse-long n)]))))

(def ex
  (parse-input
   "forward 5
down 5
forward 8
up 3
down 8
forward 2
"))

(def input
  (parse-input (slurp "inputs/day2.txt")))

(defn move [commands]
  (reduce (fn [coords [cmd n]]
            (case cmd
              :forward (update coords :pos + n)
              :up (update coords :depth - n)
              :down (update coords :depth + n)))
          {:depth 0
           :pos 0}
          commands))

(defn depth*pos [{:keys [depth pos]}]
  (* depth pos))

(defn answer1 [input]
  (depth*pos (move input)))

(defn move2 [commands]
  (reduce (fn [{:keys [aim] :as coords} [cmd n]]
            (case cmd
              :forward (-> coords
                           (update :pos + n)
                           (update :depth + (* aim n)))
              :up (update coords :aim - n)
              :down (update coords :aim + n)))
          {:depth 0
           :pos 0
           :aim 0}
          commands))

(defn answer2 [input]
  (depth*pos (move2 input)))

(comment
  (= 150 (answer1 ex))
  (= 900 (answer2 ex))

  (answer1 input)
  (answer2 input)
  )
