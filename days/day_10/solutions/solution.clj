(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn read-input-raw [path]
  (->> path slurp str/trim))

(defn parse-line [line]
  (let [pattern-re #"\[([.#]+)\]"
        pattern-match (re-find pattern-re line)
        target-pattern (if pattern-match
                         (mapv #(= % \#) (second pattern-match))
                         [])
        
        button-re #"\(([^)]*)\)"
        button-matches (re-seq button-re line)
        buttons (mapv (fn [[_ btn-str]]
                       (let [trimmed (str/trim btn-str)]
                         (if (str/blank? trimmed)
                           []
                           (mapv #(Long/parseLong (str/trim %))
                                 (str/split trimmed #",")))))
                     button-matches)
        
        joltage-re #"\{([^}]+)\}"
        joltage-match (re-find joltage-re line)
        joltages (if joltage-match
                   (mapv #(Long/parseLong (str/trim %))
                         (str/split (second joltage-match) #","))
                   [])]
    {:target-pattern target-pattern
     :buttons buttons
     :joltages joltages}))

(defn gaussian-elimination-gf2 [matrix target]
  (let [num-buttons (count matrix)
        num-lights (count target)
        ;; Create augmented matrix [A | b]
        aug (vec (for [i (range num-lights)]
                  (vec (concat (for [j (range num-buttons)]
                                (if (and (< j (count matrix))
                                         (< i (count (nth matrix j))))
                                  (nth (nth matrix j) i)
                                  false))
                              [(nth target i)]))))]
    (loop [aug aug
           pivot-row 0
           pivot-col 0]
      (if (or (>= pivot-row num-lights) (>= pivot-col num-buttons))
        ;; Check for inconsistency and back substitution
        (let [inconsistent? (some (fn [i]
                                   (and (nth (nth aug i) num-buttons)
                                        (not (some identity (take num-buttons (nth aug i))))))
                                 (range pivot-row num-lights))]
          (if inconsistent?
            nil
            (let [solution (vec (repeat num-buttons false))
                  used-rows (atom #{})]
              (loop [i (dec num-lights)
                     sol solution]
                (if (< i 0)
                  (count (filter identity sol))
                  (let [row (nth aug i)
                        pivot-col-idx (first (filter #(and (nth row %)
                                                          (not (@used-rows %)))
                                                   (range num-buttons)))]
                    (if pivot-col-idx
                      (do (swap! used-rows conj pivot-col-idx)
                          (let [val (loop [val (nth row num-buttons)
                                          j (inc pivot-col-idx)]
                                     (if (>= j num-buttons)
                                       val
                                       (if (and (nth row j) (nth sol j))
                                         (recur (not val) (inc j))
                                         (recur val (inc j)))))]
                            (recur (dec i) (assoc sol pivot-col-idx val))))
                      (recur (dec i) sol)))))))
        ;; Find pivot
        (let [pivot-idx (first (filter #(nth (nth aug %) pivot-col)
                                      (range pivot-row num-lights)))]
          (if (nil? pivot-idx)
            (recur aug pivot-row (inc pivot-col))
            ;; Swap rows and eliminate
            (let [aug-swapped (if (= pivot-idx pivot-row)
                               aug
                               (assoc aug pivot-row (nth aug pivot-idx)
                                     pivot-idx (nth aug pivot-row)))
                  aug-eliminated (vec (for [i (range num-lights)]
                                       (if (and (> i pivot-row)
                                                (nth (nth aug-swapped i) pivot-col))
                                         (vec (map #(bit-xor %1 %2)
                                                  (nth aug-swapped i)
                                                  (nth aug-swapped pivot-row)))
                                         (nth aug-swapped i))))]
              (recur aug-eliminated (inc pivot-row) (inc pivot-col)))))))))

(defn solve-part2-ilp [buttons joltages]
  (let [num-buttons (count buttons)
        num-lights (count joltages)
        max-joltage (if (seq joltages) (apply max joltages) 0)]
    (letfn [(dfs [button-idx current-joltages presses-so-far best]
              (if (>= button-idx num-buttons)
                (if (every? true? (map = current-joltages joltages))
                  (if (nil? best) presses-so-far (min best presses-so-far))
                  best)
                (if (and best (>= presses-so-far best))
                  best
                  (let [remaining-needs (map #(max 0 (- %2 %1))
                                           current-joltages joltages)
                        sum-remaining (reduce + remaining-needs)]
                    (if (and (> sum-remaining 0)
                             (let [max-lights-per-button (apply max (map count (drop button-idx buttons)))]
                               (and (not= max-lights-per-button 0)
                                    (let [min-additional (int (Math/ceil (/ sum-remaining max-lights-per-button)))]
                                      (and best (>= (+ presses-so-far min-additional) best))))))
                      best
                      (loop [presses 0
                             best-result best]
                        (if (or (> presses max-joltage)
                                (and best (>= (+ presses-so-far presses) best)))
                          best-result
                          (let [new-joltages (vec (map-indexed (fn [i curr]
                                                                 (+ curr (if (some #(= i %) (nth buttons button-idx))
                                                                           presses 0)))
                                                               current-joltages))]
                            (if (some (fn [[a b]] (> a b)) (map vector new-joltages joltages))
                              (recur (inc presses) best-result)
                              (let [result (dfs (inc button-idx) new-joltages
                                               (+ presses-so-far presses) best-result)]
                                (recur (inc presses) (if (and result (or (nil? best-result) (< result best-result)))
                                                      result best-result)))))))))))]
      (dfs 0 (vec (repeat num-lights 0)) 0 nil))))

(defn solve [input-data]
  (let [lines (str/split-lines input-data)
        lines (filter (comp not str/blank?) lines)]
    (if (empty? lines)
      ["0" "0"]
      (let [[part1-total part2-total]
            (reduce (fn [[p1 p2] line]
                     (let [parsed (parse-line line)]
                       (if (empty? (:target-pattern parsed))
                         [p1 p2]
                         (let [num-lights (count (:target-pattern parsed))
                               ;; Part 1: Build incidence matrix
                               button-matrix (vec (for [i (range (count (:buttons parsed)))]
                                                   (vec (for [j (range num-lights)]
                                                         (some #(= j %) (nth (:buttons parsed) i))))))
                               required-toggles (:target-pattern parsed)
                               result1 (gaussian-elimination-gf2 button-matrix required-toggles)
                               new-p1 (+ p1 (if result1 result1 0))
                               ;; Part 2: ILP
                               result2 (if (= (count (:joltages parsed)) num-lights)
                                        (solve-part2-ilp (:buttons parsed) (:joltages parsed))
                                        nil)
                               new-p2 (+ p2 (if result2 result2 0))]
                           [new-p1 new-p2]))))
                   [0 0]
                   lines)]
        [(str part1-total) (str part2-total)]))))

(defn -main []
  (let [data (read-input-raw "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
