(require '[clojure.string :as str])

(defn read-input-raw [path]
  (->> path slurp str/trim))

(defn solve [input-data]
  (let [lines (str/split-lines input-data)
        max-len (apply max (map count lines))
        padded-lines (map #(let [pad-count (- max-len (count %))]
                            (if (> pad-count 0)
                              (str % (apply str (repeat pad-count " ")))
                              %))
                          lines)
        op-row-idx (dec (count padded-lines))
        op-row (nth padded-lines op-row-idx)
        num-rows (take op-row-idx padded-lines)
        
        ;; Find problem boundaries (columns that are all spaces)
        is-space-col (vec (for [col (range max-len)]
                            (every? #(or (>= col (count %))
                                         (= \space (nth % col)))
                                    padded-lines)))
        
        ;; Group columns into problems
        problems (loop [i 0
                        acc []]
                   (if (>= i max-len)
                     acc
                     (if (not (nth is-space-col i))
                       (let [start-col i
                             end-col (loop [j (inc i)]
                                       (if (and (< j max-len)
                                                (not (nth is-space-col j)))
                                         (recur (inc j))
                                         j))
                             op (first (filter #(or (= % \+) (= % \*))
                                               (subs op-row start-col end-col)))]
                         (if op
                           (recur end-col (conj acc {:start-col start-col
                                                      :end-col end-col
                                                      :op op}))
                           (recur end-col acc)))
                       (recur (inc i) acc))))
        
        ;; Part 1: Extract numbers horizontally
        part1-total (reduce +
                            (for [{:keys [start-col end-col op]} problems]
                              (let [numbers (reduce concat
                                                    (for [row num-rows]
                                                      (let [problem-str (str/trim (subs row start-col end-col))
                                                            parts (str/split problem-str #"\s+")]
                                                        (map #(Long/parseLong %)
                                                             (filter (complement str/blank?) parts)))))]
                                (if (seq numbers)
                                  (let [result (if (= op \+)
                                                 (reduce + numbers)
                                                 (reduce * numbers))]
                                    result)
                                  0))))
        
        ;; Part 2: Parse vertically (columns, right-to-left)
        ;; Approach: Use Clojure's functional transformations with threading
        part2-total (reduce +
                            (for [{:keys [start-col end-col op]} problems]
                              (let [col-strings (->> (range start-col end-col)
                                                    (remove #(nth is-space-col %))
                                                    (map (fn [col]
                                                           (->> num-rows
                                                                (map #(get % col \space))
                                                                (filter #(or (Character/isDigit %)
                                                                            (= \space %)))
                                                                (apply str)
                                                                str/trim)))
                                                    (filter (complement str/blank?))
                                                    reverse)  ; Right-to-left
                                    numbers (->> col-strings
                                                (map #(str/replace % #"\s" ""))
                                                (filter (complement str/blank?))
                                                (map #(Long/parseLong %)))]
                                (if (seq numbers)
                                  (let [result (if (= op \+)
                                                 (reduce + numbers)
                                                 (reduce * numbers))]
                                    result)
                                  0))))]
    [part1-total part2-total]))

(let [content (read-input-raw "../data/input.txt")
      [part1 part2] (solve content)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
