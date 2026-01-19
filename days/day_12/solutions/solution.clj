(require '[clojure.string :as str])

(defn read-input-raw [path]
  (->> path slurp str/trim))

(defn parse-shape [lines i]
  (when (< (+ i 3) (count lines))
    (let [line (str/trim (nth lines i ""))
          shape-num-str (subs line 0 (dec (count line)))]
      (try
        (let [shape-num (Integer/parseInt shape-num-str)
              shape-grid (map #(str/trim (nth lines (+ i 1 %) "")) (range 3))
              area (reduce + (map #(count (filter (fn [c] (= c \#)) %)) shape-grid))]
          [shape-num area])
        (catch Exception _ nil)))))

(defn solve [input-data]
  (let [lines (str/split-lines input-data)]
    (if (empty? lines)
      ["0" "Final star"]
      (let [;; Parse shapes (first 6 shapes, numbered 0-5)
            shape-areas (loop [i 0 shape-idx 0 acc []]
                          (cond
                            (>= shape-idx 6) (vec acc)
                            (>= i (count lines)) (vec acc)
                            :else
                            (let [line (str/trim (nth lines i ""))]
                              (if (and (not (empty? line)) (str/ends-with? line ":"))
                                (let [parsed (parse-shape lines i)]
                                  (if (and parsed (= (first parsed) shape-idx))
                                    (recur (+ i 4) (inc shape-idx) (conj acc (second parsed)))
                                    (recur (inc i) shape-idx acc)))
                                (recur (inc i) shape-idx acc)))))
            
            ;; Find query start (after shapes - shapes are numbered 0-5, each takes 4 lines)
            query-start (loop [i 0]
                          (if (>= i (count lines))
                            0
                            (let [line (str/trim (nth lines i ""))]
                              (if (and (not (empty? line)) (str/includes? line "x") (str/includes? line ":"))
                                i
                                (recur (inc i))))))
            
            ;; Parse queries
            possible-count (loop [remaining-lines (drop query-start lines) count 0]
                             (if (empty? remaining-lines)
                               count
                               (let [line (str/trim (first remaining-lines))]
                                 (if (empty? line)
                                   (recur (rest remaining-lines) count)
                                   (if (str/includes? line ":")
                                     (let [[dims counts-str] (str/split line #":" 2)
                                           dims (str/trim dims)
                                           counts-str (str/trim counts-str)]
                                       (if (str/includes? dims "x")
                                         (let [[width-str height-str] (str/split dims #"x")
                                               width (try (Integer/parseInt width-str) (catch Exception _ nil))
                                               height (try (Integer/parseInt height-str) (catch Exception _ nil))]
                                           (if (and width height)
                                             (let [count-parts (str/split counts-str #"\s+")]
                                               (if (= (count count-parts) 6)
                                                 (let [counts (map #(Integer/parseInt %) count-parts)
                                                       region-area (* width height)
                                                       required-area (reduce + (map * shape-areas counts))]
                                                   (recur (rest remaining-lines) 
                                                          (+ count (if (<= required-area region-area) 1 0))))
                                                 (recur (rest remaining-lines) count)))
                                             (recur (rest remaining-lines) count)))
                                         (recur (rest remaining-lines) count)))
                                     (recur (rest remaining-lines) count))))))
            
            ;; Part 2: Final star (no computation needed)
            part2 "Final star"]
        [(str possible-count) part2]))))

(let [data (read-input-raw "../data/input.txt")
      [part1 part2] (solve data)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
