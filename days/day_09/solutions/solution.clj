(require '[clojure.string :as str])

(defn parse-coordinates [lines]
  (->> lines
       (map str/trim)
       (filter #(not (str/blank? %)))
       (filter #(str/includes? % ","))
       (map #(let [[x y] (str/split % #",")]
               (try
                 [(Long/parseLong (str/trim x)) (Long/parseLong (str/trim y))]
                 (catch Exception e nil))))
       (filter some?)))

(defn point-in-polygon [px py polygon]
  (let [n (count polygon)
        edges (map-indexed (fn [i _] 
                            [(nth polygon i) 
                             (nth polygon (mod (inc i) n))])
                          polygon)]
    (odd? (count (filter (fn [[[x1 y1] [x2 y2]]]
                          (and (not= (> y1 py) (> y2 py))
                               (< px (+ x1 (* (- py y1) (/ (- x2 x1) (if (= y2 y1) 1.0 (- y2 y1))))))))
                        edges)))))

(defn solve [input-data]
  (let [lines (str/split-lines (str/trim input-data))
        red-tiles (parse-coordinates lines)]
    (if (< (count red-tiles) 2)
      ["0" "0"]
      (let [;; Part 1
            max-area-part1 (apply max
                                  (for [[x1 y1] red-tiles
                                        [x2 y2] red-tiles
                                        :when (< [x1 y1] [x2 y2])]
                                    (* (+ (Math/abs (- x1 x2)) 1)
                                       (+ (Math/abs (- y1 y2)) 1))))
            
            ;; Part 2: Use coordinate compression approach
            all-x-set (set (mapcat (fn [[x _]] [x (inc x)]) red-tiles))
            all-y-set (set (mapcat (fn [[_ y]] [y (inc y)]) red-tiles))
            all-x (sort all-x-set)
            all-y (sort all-y-set)
            
            x-to-cx (zipmap all-x (range))
            y-to-cy (zipmap all-y (range))
            
            width (count all-x)
            height (count all-y)
            
            ;; Build valid tiles set using coordinate compression
            valid-tiles (atom #{})
            
            ;; Mark boundary
            _ (doseq [[x y] red-tiles]
                (swap! valid-tiles conj [x y]))
            
            ;; Connect consecutive tiles
            _ (doseq [i (range (count red-tiles))]
                (let [[x1 y1] (nth red-tiles i)
                      [x2 y2] (nth red-tiles (mod (inc i) (count red-tiles)))]
                  (if (= x1 x2)
                    (doseq [y (range (min y1 y2) (inc (max y1 y2)))]
                      (swap! valid-tiles conj [x1 y]))
                    (when (= y1 y2)
                      (doseq [x (range (min x1 x2) (inc (max x1 x2)))]
                        (swap! valid-tiles conj [x y1])))))
            
            ;; Fill interior - check compressed grid points
            _ (doseq [cx (range width)
                     cy (range height)]
                (let [orig-x (nth all-x cx)
                      orig-y (nth all-y cy)]
                  (when (and (not (@valid-tiles [orig-x orig-y]))
                           (point-in-polygon orig-x orig-y red-tiles))
                    (swap! valid-tiles conj [orig-x orig-y]))))
            
            valid-tiles-set @valid-tiles
            
            ;; Generate candidates sorted by area descending
            candidates (sort-by (fn [[_ _ _ _ area]] (- area))
                               (for [[x1 y1] red-tiles
                                     [x2 y2] red-tiles
                                     :when (< [x1 y1] [x2 y2])]
                                 (let [min-x (min x1 x2)
                                       max-x (max x1 x2)
                                       min-y (min y1 y2)
                                       max-y (max y1 y2)
                                       area (* (+ (Math/abs (- x1 x2)) 1)
                                               (+ (Math/abs (- y1 y2)) 1))]
                                   [min-x max-x min-y max-y area])))
            
            ;; Check candidates
            is-valid-rect? (fn [[min-x max-x min-y max-y]]
                            (every? #(contains? valid-tiles-set %)
                                   (for [x (range min-x (inc max-x))
                                         y (range min-y (inc max-y))]
                                     [x y])))
            
            max-area-part2 (loop [[[min-x max-x min-y max-y area] & rest] candidates
                                 best 0]
                            (if (or (nil? min-x) (<= area best))
                              best
                              (if (is-valid-rect? [min-x max-x min-y max-y])
                                area
                                (recur rest best))))]
        [(str max-area-part1) (str max-area-part2)])))

(defn -main []
  (let [data (slurp "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
