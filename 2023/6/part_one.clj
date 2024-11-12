(require '[clojure.string :as str])

(defn read-file [file-name]
  (->> (slurp file-name)
       (str/split-lines)))

(defn make-races [document]
  (->> document
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))
       (apply map vector)))

(defn compute-ways-to-win [[time distance]]
  (->> (range time)
       (map #(- (* time %) (* % %)))
       (filter #(> % distance))
       (count)))

(defn calculate-result [nums]
  (apply * nums))

(defn main [file-name]
  (->> (read-file file-name)
       (make-races)
       (map compute-ways-to-win)
       (calculate-result)))

(main "z_input.txt")
