(ns advent21.day3
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as xfs]
            [net.cgrand.xforms.rfs :as rfs]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map (partial map str))
       (map (partial mapv parse-long))))

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

(comment
  (= 198 (answer1 ex))

  (answer1 input)
  )
