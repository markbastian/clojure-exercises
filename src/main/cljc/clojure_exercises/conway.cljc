(ns clojure-exercises.conway
  (:require [clojure.string :as str]))

(defn neighbors [[x y]]
  (let [dx (juxt inc inc identity dec dec dec identity inc)
        dy (juxt identity inc inc inc identity dec dec dec)]
    (map vector (dx x) (dy y))))

(def grid
  ["      "
   " ##   "
   " ##   "
   "   ## "
   "   ## "
   "      "])

(defn grid->coords [grid]
  (for [y (range (count grid))
        x (range (count (vec (grid y))))
        :when (#{\#} (get-in grid [y x]))]
    [x y]))

(defn coords->grid [{:keys [coords bounds]}]
  (let [[_ _ maxx maxy] bounds
        vx (vec (repeat (inc maxx) \space))
        grid (vec (repeat (inc maxy) vx))]
    (reduce (fn [acc coord] (assoc-in acc coord \#)) grid coords)))

(defn grid->str [grid]
  (str/join "\n" (mapv str/join grid)))

(defn step-coords [coords]
  (letfn [(alive [[coord freq]]
            (or
              (= 3 freq)
              (and (= 2 freq) (coords coord))))]
    (->> coords
         (mapcat neighbors)
         frequencies
         (filter alive)
         keys
         set)))

(defn step [{:keys [coords bounds] :as state}]
  (let [[minx miny maxx maxy] bounds
        c (->> (step-coords (set coords))
               (filter (fn [[x y]]
                         (and
                           (<= minx x maxx)
                           (<= miny y maxy)))))]
    (assoc state :coords (set c))))

(comment
  (neighbors [0 0])
  (grid->coords grid)

  (let [grid ["      "
              " ##   "
              " ##   "
              "   ## "
              "   ## "
              "      "]
        maxy (dec (count grid))
        maxx (dec (count (first grid)))
        coords (grid->coords grid)]
    (->> {:coords coords
          :bounds [0 0 maxx maxy]}
         (iterate step)
         (take 3)
         (map (comp println grid->str coords->grid))))

  (let [grid ["     "
              "     "
              " ### "
              "     "
              "     "]
        maxy (dec (count grid))
        maxx (dec (count (first grid)))
        coords (grid->coords grid)]
    (->> {:coords coords
          :bounds [0 0 maxx maxy]}
         (iterate step)
         (take 3)
         (map (comp println grid->str coords->grid))))
  )
