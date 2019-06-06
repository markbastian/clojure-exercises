(ns codingame.ascii-art
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

;https://www.codingame.com/ide/puzzle/ascii-art

(def m
  (->> "codingame/ascii_art_example1.txt"
       io/resource
       slurp
       cs/split-lines
       (drop 3)
       (map #(map cs/join (partition-all 4 (concat % (repeat \space)))))
       (apply map vector)
       (zipmap (conj (mapv #(char (+ (int \A) %)) (range 26)) \?))))

(->> "RUSH"
     (map m)
     (apply map vector)
     (map cs/join)
     (cs/join "\n")
     println)

{ 1 2
  2 3
  3 4
  3 7
  4 5
  4 6
  7 8 }