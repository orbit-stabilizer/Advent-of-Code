(require '[clojure.string :as str])

(defn read-file [file-name]
  (->> (slurp file-name)
       (str/split-lines)))

(defn make-race [lines]
  (->> lines
       (map #(re-seq #"\d+" %))
       (map #(str/join "" %))
       (map parse-long)))

(defn compute-ways-to-win [[t d]]
  (let [x (/ (- t (Math/sqrt (- (* t t) (* 4 d)))) 2)
        y (/ (+ t (Math/sqrt (- (* t t) (* 4 d)))) 2)]
    (+ (- (- (Math/ceil y) 1) (+ (Math/floor x) 1)) 1)))

(defn main [file-name]
  (->> (read-file file-name)
       (make-race)
       (compute-ways-to-win)))

(main "z_input.txt")
