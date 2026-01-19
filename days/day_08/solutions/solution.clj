(require '[clojure.java.io :as io])

(defrecord Coord [x y z])

(defrecord Pair [i j dist-sq])

(defn parse-coordinates [lines]
  (->> lines
       (map clojure.string/trim)
       (filter (complement clojure.string/blank?))
       (map (fn [line]
              (let [parts (clojure.string/split line #",")]
                (->Coord
                 (Long/parseLong (nth parts 0))
                 (Long/parseLong (nth parts 1))
                 (Long/parseLong (nth parts 2))))))))

(defn squared-distance [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))
        dz (- (:z p2) (:z p1))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn union-find [n]
  (let [parent (atom (vec (range n)))
        size (atom (vec (repeat n 1)))
        component-count (atom n)]
    {:parent parent
     :size size
     :component-count component-count
     :n n}))

(defn find-root [uf x]
  (let [parent @(:parent uf)
        p (get parent x)]
    (if (= p x)
      x
      (let [root (find-root uf p)]
        (swap! (:parent uf) assoc x root)
        root))))

(defn union [uf x y]
  (let [root-x (find-root uf x)
        root-y (find-root uf y)]
    (if (= root-x root-y)
      false
      (let [size @(:size uf)
            size-x (get size root-x)
            size-y (get size root-y)
            [rx ry] (if (< size-x size-y) [root-y root-x] [root-x root-y])]
        (swap! (:parent uf) assoc ry rx)
        (swap! (:size uf) assoc rx (+ (get @(:size uf) rx) (get @(:size uf) ry)))
        (swap! (:component-count uf) dec)
        true))))

(defn solve [input-data]
  (let [lines (clojure.string/split-lines (clojure.string/trim input-data))
        coords (parse-coordinates lines)
        n (count coords)]
    (if (zero? n)
      ["0" "0"]
      (let [;; Generate all pairs with squared distances
            pairs (for [i (range n)
                        j (range (inc i) n)]
                    (let [dist-sq (squared-distance (nth coords i) (nth coords j))]
                      (->Pair i j dist-sq)))
            ;; Sort by distance
            sorted-pairs (sort-by :dist-sq pairs)
            
            ;; Part 1: Connect first 1000 pairs
            uf1 (union-find n)
            _ (doseq [[idx pair] (map-indexed vector sorted-pairs)]
                (when (< idx 1000)
                  (union uf1 (:i pair) (:j pair))))
            
            ;; Get component sizes
            component-sizes (loop [i 0
                                   sizes {}]
                             (if (= i n)
                               sizes
                               (let [root (find-root uf1 i)]
                                 (recur (inc i)
                                        (assoc sizes root (get @(:size uf1) root))))))
            sizes (sort > (vals component-sizes))
            part1 (if (>= (count sizes) 3)
                    (* (nth sizes 0) (nth sizes 1) (nth sizes 2))
                    0)
            
            ;; Part 2: Connect until all in one circuit
            uf2 (union-find n)
            final-pair (loop [pairs sorted-pairs]
                        (if (zero? @(:component-count uf2))
                          nil
                          (if (empty? pairs)
                            nil
                            (let [pair (first pairs)]
                              (if (union uf2 (:i pair) (:j pair))
                                (if (= @(:component-count uf2) 1)
                                  pair
                                  (recur (rest pairs)))
                                (recur (rest pairs)))))))
            part2 (if final-pair
                    (let [i (:i final-pair)
                          j (:j final-pair)]
                      (* (:x (nth coords i)) (:x (nth coords j))))
                    0)]
        [(str part1) (str part2)]))))

(defn -main []
  (let [data (slurp "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
