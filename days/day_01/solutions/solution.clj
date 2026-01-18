(require '[clojure.string :as str])

;; Import utilities (assuming they're on classpath)
;; For now, we'll use inline file reading for simplicity
(defn read-input [path]
  (->> path
       slurp
       str/trim
       str/split-lines))

(defn solve [lines]
  ;; Part 1: Count times dial ends at 0 after a rotation
  (let [result1 (loop [position 50
                       count-part1 0
                       remaining lines]
                  (if (empty? remaining)
                    count-part1
                    (let [line (first remaining)
                          direction (first line)
                          distance (Integer/parseInt (subs line 1))
                          new-pos (if (= direction \L)
                                    (mod (+ (- position distance) 100) 100)
                                    (mod (+ position distance) 100))]
                      (recur new-pos
                             (if (= new-pos 0) (inc count-part1) count-part1)
                             (rest remaining)))))
        
        ;; Part 2: Count times dial is at 0 during entire process
        result2 (loop [position 50
                       count-part2 0
                       remaining lines]
                  (if (empty? remaining)
                    count-part2
                    (let [line (first remaining)
                          direction (first line)
                          distance (Integer/parseInt (subs line 1))
                          start-pos position
                          ;; Count zeros during rotation
                          zeros-in-rotation (reduce (fn [cnt click]
                                                      (let [click-pos (if (= direction \L)
                                                                        (mod (+ (- start-pos click) 100) 100)
                                                                        (mod (+ start-pos click) 100))]
                                                        (if (= click-pos 0) (inc cnt) cnt)))
                                                    0
                                                    (range 1 (inc distance)))
                          new-pos (if (= direction \L)
                                    (mod (+ (- position distance) 100) 100)
                                    (mod (+ position distance) 100))]
                      (recur new-pos
                             (+ count-part2 zeros-in-rotation)
                             (rest remaining)))))]
    [result1 result2]))

(let [lines (read-input "../data/input.txt")
      [part1 part2] (solve lines)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
