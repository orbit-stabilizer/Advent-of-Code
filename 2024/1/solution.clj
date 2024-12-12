(defn partition-cycle [n list-or-vec]
  (loop [partitions (vec (repeat n []))
         i 0]
    (cond
      (= i (count list-or-vec))
      partitions
      :else
      (let [k (mod i n)
            p (nth partitions k)
            item (nth list-or-vec i)]
        (recur
          (assoc partitions k (conj p item))
          (+ i 1))))))

(defn compute-similarity-score [[left-list right-list]]
  (let [counts (frequencies right-list)]
    (->> left-list
         (map #(* % (get counts % 0)))
         (apply +))))

(defn compute-distance [[left-list right-list]]
  (->> (map (comp abs -) left-list right-list)
       (apply +)))

(defn make-lists [document]
  (->> (re-seq #"\d+" document)
       (map parse-long)
       (partition-cycle 2)
       (map sort)))

(defn main [file-name compute-result]
  (->> (slurp file-name)
       (make-lists)
       (compute-result)))

(comment
  (print (slurp "sample.txt"))
  (partition-cycle 2 [0 1 2 3 4 5 6 7 8 9])
  (partition-cycle 3 [0 1 2 3 4 5 6 7 8 9])
  (vec (repeat 5 []))
  (main "sample.txt" compute-distance)
  (main "input.txt" compute-distance)
  (main "sample.txt" compute-similarity-score)
  (main "input.txt" compute-similarity-score))
