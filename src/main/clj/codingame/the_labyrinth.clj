(ns codingame.the-labyrinth)

(defn neighbors [[i j]]
  (let [xs ((juxt inc identity dec identity) i)
        ys ((juxt identity inc identity dec) j)]
    (mapv vector xs ys)))

(neighbors [1 1])

(defn -main [& args]
  (let [; R: number of rows.
        ; C: number of columns.
        ; A: number of rounds between the time the alarm countdown is activated and the time the alarm goes off.
        [R C A] (map #(Integer/parseInt %) (filter #(not-empty %) (str/split (read-line) #" ")))]
    (loop [; KR: row where Rick is located.
           ; KC: column where Rick is located.
           [KR KC :as c] (map #(Integer/parseInt %) (filter #(not-empty %) (str/split (read-line) #" ")))
           visited #{c}
           grid (vec (for [i R
                           :let [row (read-line)]]
                       (vec row)))]
      (debug grid)
      ; Rick's next move (UP DOWN LEFT or RIGHT).
      (output "RIGHT")
      (recur
        (map #(Integer/parseInt %) (filter #(not-empty %) (str/split (read-line) #" ")))
        visited
        (vec (for [i R
                   :let [row (read-line)]]
               (vec row))))
      )))


