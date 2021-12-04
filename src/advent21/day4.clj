(ns advent21.day4
  (:require [clojure.string :as str]
            [medley.core :as medley]))

(defn parse-board [s]
  (mapv #(->> (str/split (str/trim %) #"\s+")
              (mapv parse-long))
        (str/split-lines s)))

(defn parse-numbers [s]
  (->> (str/split s #",") (map parse-long)))

(defn parse-input [s]
  (let [[numbers & boards] (str/split s #"\n\n")]
    {:numbers (parse-numbers numbers)
     :boards (map parse-board boards)}))

(def ex
  (parse-input
   "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"))

(def input
  (parse-input (slurp "inputs/day4.txt")))

(defn unmarked [board]
  (mapv #(mapv (fn [n] {:n n :marked? false}) %) board))

(defn mark [board n]
  (reduce (fn [board row-idx]
            (reduce (fn [board idx]
                      (cond-> board
                        (= n (get-in board [row-idx idx :n]))
                        (-> (assoc-in [row-idx idx :marked?] true) reduced)))
                    board
                    (range (count (board row-idx)))))
          board
          (range (count board))))

(defn transpose [coll]
  (apply mapv vector coll))

(defn win? [board]
  (let [w? #(some (partial every? :marked?) %)]
    (or (w? board) (w? (transpose board)))))

(defn score [board n]
  (let [unmarked (->> board (mapcat seq) (remove :marked?) (map :n))]
    (* n (apply + unmarked))))

(defn answer1 [{:keys [numbers boards]}]
  (reduce (fn [bs n]
            (let [new-bs (map #(mark % n) bs)
                  winner (medley/find-first win? new-bs)]
              (if (some? winner)
                (reduced (score winner n))
                new-bs)))
          (map unmarked boards)
          numbers))

(defn answer2 [{:keys [numbers boards]}]
  (reduce (fn [{:keys [bs loser-idx] :as acc} n]
            (let [new-bs (map #(mark % n) bs)]
              (cond
                (every? win? new-bs)
                (let [loser (nth new-bs loser-idx)]
                  (reduced (score loser n)))

                (and (nil? loser-idx)
                     (= 1 (count (remove win? new-bs))))
                (let [loser-idx (some (fn [[idx b]]
                                        (and (not (win? b)) idx))
                                      (medley/indexed new-bs))]
                  (assoc acc :bs new-bs :loser-idx loser-idx))

                :else (assoc acc :bs new-bs))))
          {:bs (mapv unmarked boards)
           :loser-idx nil}
          numbers))

(comment
  (= 4512 (answer1 ex))
  (= 1924 (answer2 ex))

  (answer1 input)
  (time (answer2 input))
  )
