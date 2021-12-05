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

(defn line->coords [[[start-x start-y] [end-x end-y] :as line]]
  (if (diagonal? line)
    () ;; TODO:
    (for [x (cond->> (range (min start-x end-x) (inc (max start-x end-x)))
              (> start-x end-x) reverse)
          y (cond->> (range (min start-y end-y) (inc (max start-y end-y)))
              (> start-y end-y) reverse)]
      [x y])))

(comment
  (= [[1 1] [1 2] [1 3]]
     (line->coords [[1 1] [1 3]]))

  (= [[9 7] [8 7] [7 7]]
     (line->coords [[9 7] [7 7]]))

  (= [[1 1] [2 2] [3 3]]
     (line->coords [[1 1] [3 3]]))

  (= [[9 7] [8 8] [7 9]]
     (line->coords [[9 7] [7 9]]))

  )

(defn expand-ocean-floor
  ([ocean-floor]
   (let [width (or (some->> ocean-floor (map count) not-empty (apply max))
                   0)]
     (reduce (fn [o idx]
               (update o idx #(into % (repeat (- width (count %)) 0))))
             ocean-floor
             (range (count ocean-floor)))))

  ([ocean-floor [x y]]
   (let [missing-x (- (inc x) (count ocean-floor))
         expanded-x (into ocean-floor (repeat missing-x []))
         missing-y (- (inc y) (count (get expanded-x x)))]
     (update expanded-x x into (repeat missing-y 0)))))

(defn mark-vent [ocean-floor coords]
  (-> ocean-floor
      (expand-ocean-floor coords)
      (update-in coords (fnil inc 0))))

(defn answer1-ocean-floor [input]
  (transduce (comp (remove diagonal?) (mapcat line->coords))
             (completing mark-vent)
             []
             input))

(defn vent-risk-count [ocean-floor]
  (xfs/count (comp (mapcat seq) (filter #(< 1 %))) ocean-floor))

(defn answer1 [input]
  (vent-risk-count (answer1-ocean-floor input)))

(defn answer2-ocean-floor [input]
  (transduce (mapcat line->coords) (completing mark-vent) [] input))

(defn answer2 [input]
  (vent-risk-count (answer2-ocean-floor input)))

(defn transpose [coll]
  (apply mapv vector coll))

(defn render-ocean-floor [ocean-floor]
  (let [width (or (some->> ocean-floor (map count) not-empty (apply max))
                  0)]
    (->> ocean-floor
         expand-ocean-floor
         transpose
         (map (fn [row]
                (->> row (map #(if (zero? %) "." %)) str/join)))
         (str/join "\n"))))

(defn clean-console []
  (print "\033[H\033[2J")
  (flush))

(defn print-floor [ocean-floor]
  (clean-console)
  (println (render-ocean-floor ocean-floor))
  (flush))

(defn animate [lines]
  (reduce (fn [o line]
            (Thread/sleep 300)
            (reduce (fn [o coords]
                      (Thread/sleep 150)
                      (doto (mark-vent o coords)
                        print-floor))
                    o
                    (line->coords line)))
          []
          lines))

;; clojure -X advent21.day5/animate1 :in :ex
(defn animate1 [{:keys [in] :or {in :ex}}]
  (animate (remove diagonal? (in {:input input :ex ex}))))

;; clojure -X advent21.day5/animate2 :in :ex
(defn animate2 [{:keys [in] :or {in :ex}}]
  (animate (in {:input input :ex ex})))

(comment

  (=
   ".......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111...."
     (render-ocean-floor (answer1-ocean-floor ex)))

  (= 5 (answer1 ex))

  "Elapsed time: 8444.524804 msecs"
  "Elapsed time: 267.005436 msecs"
  (= 6841 (time (answer1 input)))
  )
