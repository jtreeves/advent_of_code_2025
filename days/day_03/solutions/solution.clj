(require '[clojure.java.io :as io])

;; Find the largest N-digit number by selecting N digits in order from bank
(defn find-largest-subsequence [bank n]
  (let [bank-len (count bank)]
    (if (< bank-len n)
      0
      (loop [result []
             start 0
             i 0]
        (if (= i n)
          (Long/parseLong (apply str result))
          (let [remaining-needed (- n i 1)
                end (- bank-len remaining-needed)
                candidates (subs bank start end)
                candidates-chars (seq candidates)
                max-char (reduce #(if (> (int %2) (int %1)) %2 %1) candidates-chars)
                max-pos (+ start (count (take-while #(not= % max-char) candidates-chars)))]
            (recur (conj result max-char)
                   (inc max-pos)
                   (inc i))))))))

(defn solve [input-data]
  (let [lines (filter #(not (empty? %)) 
                      (clojure.string/split-lines (clojure.string/trim input-data)))]
    (let [part1-sum (reduce + (map #(find-largest-subsequence % 2) lines))
          part2-sum (reduce + (map #(find-largest-subsequence % 12) 
                                   (filter #(>= (count %) 12) lines)))]
      [(str part1-sum) (str part2-sum)])))

(defn -main []
  (let [data (slurp "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
