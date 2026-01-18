(require '[clojure.string :as str])

;; Import utilities (assuming they're on classpath)
;; For now, we'll use inline file reading for simplicity
(defn read-input [path]
  (->> path
       slurp
       str/trim
       str/split-lines))

(defn count-neighbors [grid i j rows cols]
  (reduce (fn [cnt [di dj]]
            (if (and (= di 0) (= dj 0))
              cnt
              (let [ni (+ i di)
                    nj (+ j dj)]
                (if (and (>= ni 0) (< ni rows) (>= nj 0) (< nj cols) 
                         (= (get-in grid [ni nj]) \@))
                  (inc cnt)
                  cnt))))
          0
          (for [di [-1 0 1] dj [-1 0 1]] [di dj])))

(defn solve [lines]
  (let [rows (count lines)
        cols (if (> rows 0) (count (first lines)) 0)
        
        ;; Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
        part1-count (reduce (fn [cnt [i j]]
                              (if (= (get-in lines [i j]) \@)
                                (let [neighbors (count-neighbors lines i j rows cols)]
                                  (if (< neighbors 4)
                                    (inc cnt)
                                    cnt))
                                cnt))
                            0
                            (for [i (range rows) j (range cols)] [i j]))
        
        ;; Part 2: Iteratively remove accessible rolls until none can be removed
        grid (atom (mapv vec lines))
        part2-count (loop [count 0]
                      (let [to-remove (reduce (fn [acc [i j]]
                                                (if (= (get-in @grid [i j]) \@)
                                                  (let [neighbors (count-neighbors @grid i j rows cols)]
                                                    (if (< neighbors 4)
                                                      (conj acc [i j])
                                                      acc))
                                                  acc))
                                              []
                                              (for [i (range rows) j (range cols)] [i j]))]
                        (if (empty? to-remove)
                          count
                          (do
                            (doseq [[i j] to-remove]
                              (swap! grid assoc-in [i j] \.))
                            (recur (+ count (count to-remove)))))))]
    [part1-count part2-count]))

(let [lines (read-input "../data/input.txt")
      [part1 part2] (solve lines)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
