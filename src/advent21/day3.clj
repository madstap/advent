(ns advent21.day3
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as xfs]
            [net.cgrand.xforms.rfs :as rfs]
            [medley.core :as medley]))

(defn parse-input [s]
  (->> (str/split-lines s) (map vec)))

(def ex
  (parse-input
   "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(def input
  (parse-input (slurp "inputs/day3.txt")))

(defn update-freqs [freqs number]
  (mapv (fn [fs n]
          (update fs n (fnil inc 0)))
        (concat freqs (repeat {}))
        number))

(defn freqs-xf [rf]
  (let [*freqs (volatile! [])]
    (fn
      ([] (rf))
      ([res] (-> res (rf @*freqs) unreduced rf))
      ([res x]
       (vswap! *freqs update-freqs x)
       res))))

(defn number-freqs [input]
  (transduce freqs-xf rfs/last input))

(defn max-n [freq]
  (key (apply max-key val freq)))

(defn min-n [freq]
  (key (apply max-key (comp - val) freq)))

(defn gamma [freqs]
  (mapv max-n freqs))

(defn epsilon [freqs]
  (mapv min-n freqs))

(defn bin->dec [number]
  (Integer/parseInt (apply str number) 2))

(defn answer1 [input]
  (let [freqs (number-freqs input)]
    (* (bin->dec (gamma freqs))
       (bin->dec (epsilon freqs)))))


(defn transpose [coll]
  (apply mapv vector coll))

(defn oxygen [{ones \1, zeroes \0}]
  (cond (= ones zeroes) \1, (< ones zeroes) \0, (> ones zeroes) \1))

(defn co2 [{ones \1, zeroes \0}]
  (cond (= ones zeroes) \0, (< ones zeroes) \1, (> ones zeroes) \0))

(defn rating [f input]
  (reduce (fn [remaining idx]
            (if (= 1 (count remaining))
              (reduced (first remaining))
              (let [{col idx} (transpose remaining)
                    choice (f (merge {\1 0, \0 0} (frequencies col)))]
                (filter #(= choice (% idx)) remaining))))
          input
          (range)))

(defn answer2 [input]
  (* (bin->dec (rating oxygen input))
     (bin->dec (rating co2 input))))

(comment
  (= 198 (answer1 ex))
  (= 230 (answer2 ex))

  (answer1 input)
  (answer2 input)
  )
