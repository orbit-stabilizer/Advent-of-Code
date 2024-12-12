(require '[clojure.string :as str])

(defn main [file-name solve]
  (->> (parse file-name)
       (solve)))

(defn parse [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (mapv #(mapv char %))))

(defn part1 [m]
  (->> m
       (build-search-space)
       (count-values)))

(defn part2 [m]
  (loop [sum 0 indices (for [x (range (count m)) y (range (count m))] [x y])]
    (if (empty? indices)
      sum
      (recur (+ sum (x-shape? m (first indices))) (rest indices)))))

(defn x-shape? [grid [x y]]
  (let [upper-left  [(- x 1) (- y 1)]
        lower-left  [(- x 1) (+ y 1)]
        upper-right [(+ x 1) (- y 1)]
        lower-right [(+ x 1) (+ y 1)]
        s \S
        m \M
        get-g (partial get-in grid)
        =? #(= (get-g %1) %2)
        bool->int {false 0 true 1}]
    (bool->int
      (and 
        (=? [x y] \A)
        (or
           (and (=? lower-left m)
                (=? upper-left m)
                (=? upper-right s)
                (=? lower-right s))
           (and (=? upper-left m)
                (=? upper-right m)
                (=? lower-right s)
                (=? lower-left s))
           (and (=? upper-right m)
                (=? lower-right m)
                (=? lower-left s)
                (=? upper-left s))
           (and (=? lower-right m)
                (=? lower-left m)
                (=? upper-left s)
                (=? upper-right s)))))))

(defn build-search-space [lines]
  (let [forward-horizontal lines
        reversed-horizontal (mapv #(into [] (reverse %)) forward-horizontal)
        forward-vertical (transpose lines)
        reversed-vertical (mapv #(into [] (reverse %)) forward-vertical)
        left-diags (diags lines)
        reversed-left-diags (mapv #(into [] (reverse %)) left-diags)
        right-diags (diags reversed-horizontal)
        reversed-right-diags (mapv #(into [] (reverse %)) right-diags)]
    (->> (vector
           forward-horizontal
           reversed-horizontal
           forward-vertical
           reversed-vertical
           left-diags
           reversed-left-diags
           right-diags
           reversed-right-diags)
         (mapcat identity)
         (map #(apply str %)))))

(defn count-values [search-space]
  (->> search-space
       (map #(re-seq #"XMAS" %))
       (map count)
       (flatten)
       (apply +)))

(defn transpose [m]
  (apply mapv vector m))

(defn upper-diags [m]
  (loop [i 0 acc []]
    (if
      (= i (count m)) acc
      (recur
        (+ i 1)
        (conj
          acc
          (vec (for [j (range (+ i 1))] (get-in m [(- i j) j]))))))))

(defn lower-diags [m]
  (let [n (count m)]
      (loop [k n acc []]
       (if (= k (+ n (- n 1)))
         acc
         (recur
          (+ k 1)
          (conj acc
                (vec (for [i (reverse (range n)) j (range 1 n) :when (= (+ i j) k)]
                       (get-in m [i j])))))))))

(defn diags [m]
  (vec (concat (upper-diags m) (lower-diags m))))

(comment
  (main "sample.txt" part1)
  (main "input.txt" part1)
  (main "sample.txt" part2)
  (main "input.txt" part2))
