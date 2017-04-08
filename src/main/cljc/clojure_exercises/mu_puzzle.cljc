(ns clojure-exercises.mu-puzzle
  (:require [clojure.string :as cs]))

(defn r0 [s] (cs/replace s #"(.*I)" (fn [[s]] (str s "U"))))
(defn r1 [s] (cs/replace s #"M(.+)" (fn [[_ s]] (str "M" s s))))
(defn r2 [s] (cs/replace s #"III" "U"))
(defn r3 [s] (cs/replace s #"UU" ""))
(def rules (juxt r0 r1 r2 r3))

(take 7 (iterate #(into % (mapcat rules %)) #{"MI"}))