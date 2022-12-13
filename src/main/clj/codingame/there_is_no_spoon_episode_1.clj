(ns codingame.there-is-no-spoon-episode-1
  (:require
    [clojure.java.io :as io]
    [clojure.string :as s]))

; https://www.codingame.com/ide/puzzle/there-is-no-spoon-episode-1
; Don't let the machines win. You are humanity's last hope...

;; BEGIN Codinggame utils, since using the web IDE will lead to madness
(defn printerr [s]
  (binding [*out* *err*] (println s)))

(defn read-long [& [suppress]]
  (let [l (parse-long (read-line))]
    (when-not suppress
      (printerr
        (format
          "#################\nReading long:\n%s\n#################"
          l)))
    l))

(defn read-n-lines [n & [suppress]]
  (let [input-lines (->> (repeatedly read-line)
                         (take n)
                         vec)]
    (when-not suppress
      (printerr
        (format
          "#################\nInput lines:\n%s\n#################"
          (s/join "\n" input-lines))))
    input-lines))
;; END Codinggame utils, since using the web IDE will lead to madness

(defn index-grid [grid row-axis column-axis]
  (->> grid
       (group-by row-axis)
       vals
       (map (fn [v] (->> v
                         (sort-by column-axis)
                         (partition 2 1)
                         (reduce (fn [acc [k v]] (assoc acc k v)) {}))))
       (apply merge)))

(defn compute-solutions [cells rows cols]
  (for [cell cells
        :let [r (get rows cell [-1 -1])
              c (get cols cell [-1 -1])]]
    (reduce into cell [r c])))

(defn -main [& _args]
  (let [suppress? false
        width (read-long suppress?)
        height (read-long suppress?)
        rows (read-n-lines height suppress?)
        grid (for [y (range height)
                   :let [row (vec (rows y))]
                   x (range width)
                   :when (= \0 (row x))]
               [x y])
        rows (index-grid grid second first)
        cols (index-grid grid first second)
        cells (->> grid (sort-by (juxt second first)))
        solutions (compute-solutions cells rows cols)]
    (doseq [solution solutions]
      (println (s/join " " solution)))))

(comment
  (with-open [is (io/reader (io/resource "codingame/there_is_no_spoon_episode_1/example.txt"))]
    (binding [*in* is]
      (-main)))
  )