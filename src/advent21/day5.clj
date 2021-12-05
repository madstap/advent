(ns advent21.day5
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as xfs]))

(defn parse-input [s]
  (->> (re-seq #"(\d+),(\d+) -> (\d+),(\d+)" s)
       (map rest)
       (map (partial map parse-long))
       (map (fn [[start-x start-y end-x end-y]]
              [[start-x start-y] [end-x end-y]]))))

(def input
  (parse-input (slurp "inputs/day5.txt")))

(def ex
  (parse-input
   "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(defn diagonal? [[[start-x start-y] [end-x end-y]]]
  (not (or (= start-x end-x) (= start-y end-y))))

(defn line->coords [[[start-x start-y] [end-x end-y]]]
  (for [x (cond->> (range (min start-x end-x) (inc (max start-x end-x)))
            (> start-x end-x) reverse)
        y (cond->> (range (min start-y end-y) (inc (max start-y end-y)))
            (> start-y end-y) reverse)]
    [x y]))

(comment
  (= [[1 1] [1 2] [1 3]]
     (line->coords [[1 1] [1 3]]))

  (= [[9 7] [8 7] [7 7]]
     (line->coords [[9 7] [7 7]]))
  )

(defn expand-ocean-floor [ocean-floor [x y]]
  (let [missing-x (- (inc x) (count ocean-floor))
        expanded-x (vec (concat ocean-floor (repeat missing-x [])))
        missing-y (- (inc y) (count (get expanded-x x)))]
    (update expanded-x x #(vec (concat % (repeat missing-y 0))))))

(defn mark-vent [ocean-floor coords]
  (-> ocean-floor
      (expand-ocean-floor coords)
      (update-in coords (fnil inc 0))))

(defn answer1-ocean-floor [input]
  (transduce (comp (remove diagonal?) (mapcat line->coords))
             (completing mark-vent)
             []
             input))

(defn answer1 [input]
  (xfs/count (comp (mapcat seq) (filter #(< 1 %)))
             (answer1-ocean-floor input)))

(defn render-ocean-floor [ocean-floor]
  (let [width (or (some->> ocean-floor (map count) not-empty (apply max))
                  0)]
    (->> ocean-floor
         (map (fn [row]
                (->> (concat row (repeat 0))
                     (take width)
                     (map #(if (zero? %) "." %))
                     (apply str))))
         (str/join "\n"))))

(defn render-ocean-floor [ocean-floor]
  (->> ocean-floor
       (map (fn [row]
              (->> (concat row (repeat 0))
                   (take width)
                   (map #(if (zero? %) "." %))
                   (apply str))))
       (str/join "\n")))

(defn clean-console []
  (print "\033[H\033[2J")
  (flush))

(comment

  (println (render-ocean-floor (answer1 ex)))

  (= 5 (answer1 ex))

  "Elapsed time: 8444.524804 msecs"
  (time (answer1 input))
  )
