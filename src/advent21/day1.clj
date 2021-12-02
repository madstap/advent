(ns advent21.day1
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as xfs]))

(def input
  (map parse-long (str/split-lines (slurp "inputs/day1-0.txt"))))

(defn partition-xf
  ([n] (partition-xf n n))
  ([n step]
   (fn [rf]
     (let [*part (volatile! [])]
       (fn
         ([res] (rf res))
         ([res x]
          (let [part (vswap! *part conj x)]
            (if (= n (count part))
              (do (vswap! *part subvec step)
                  (rf res part))
              res))))))))

(defn answer1 [input]
  (->> (partition 2 1 input) (filter (partial apply <)) count))

(defn answer2 [input]
  (->> (partition 3 1 input) (map (partial apply +)) answer1))

(def answer1-xf
  (comp (partition-xf 2 1) (filter (partial apply <))))

(defn answer1* [input]
  (xfs/count answer1-xf input))

(def answer2-xf
  (comp (partition-xf 3 1) (map (partial apply +)) answer1-xf))

(defn answer2* [input]
  (xfs/count answer2-xf input))


(comment
  (answer1 input)
  (answer2 input)
  )
