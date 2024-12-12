(require '[clojure.string :as str])

(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn growth-within-limits? [report]
  (let [lower-limit 0
        upper-limit 4
        adjacent-level-pairs (map vector report (drop 1 report))
        within-limits? (fn [[x y]]
                         (and 
                           (< (abs (- x y)) upper-limit)
                           (> (abs (- x y)) lower-limit)))]
    (every? within-limits? adjacent-level-pairs)))

(defn monotonic? [report]
  (let [adjacent-level-pairs (map vector report (drop 1 report))]
    (or
      (every? (fn [[x y]] (<= x y)) adjacent-level-pairs)
      (every? (fn [[x y]] (>= x y)) adjacent-level-pairs))))

(defn compute-num-fully-safe-reports [reports]
  (->> reports
    (filter monotonic?)
    (filter growth-within-limits?)
    (count)))

(defn dampened-safe-report? [report]
  (let [n (count report)
        fully-safe? #(and (monotonic? %) (growth-within-limits? %))]
    (loop [i 0]
      (cond
        (= i n) false
        (fully-safe? (vec-remove i report)) true
        :else (recur (+ i 1))))))

(defn compute-num-dampened-safe-reports [reports]
  (let [fully-safe? #(and (monotonic? %) (growth-within-limits? %))
        [fully-safe-reports potentially-unsafe-reports] ((juxt filter remove) fully-safe? reports)]
    (->> potentially-unsafe-reports
      (map dampened-safe-report?)
      (filter true?)
      (count)
      (+ (count fully-safe-reports)))))

(defn parse-text [text]
  (->> text
    (str/split-lines)
    (map #(re-seq #"\d+" %))
    (map #(map parse-long %))
    (map #(into [] %))))

(defn main [file-name compute-num-safe-reports]
  (->> (slurp file-name)
       (parse-text)
       (compute-num-safe-reports)))

(comment
  (parse-text (slurp "sample.txt"))
  (monotonic? [1 2 3 4])
  (growth-within-limits? [2 9 3 4])
  (compute-num-fully-safe-reports [[1 2 3 4]])
  (map list '(1 2 3) (pop '(1 2 3)))
  (map list (pop '(1 2 3)) '(1 2 3))
  (partition 2 1 [1 2 3 4 5])
  (main "sample.txt" compute-num-fully-safe-reports)
  (main "input.txt" compute-num-fully-safe-reports)
  (main "sample.txt" compute-num-dampened-safe-reports)
  (main "input.txt" compute-num-dampened-safe-reports))
