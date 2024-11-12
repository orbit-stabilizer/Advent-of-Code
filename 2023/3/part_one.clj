(require '[clojure.string :as str])

(defn main [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (map (partial re-seq #"\d+"))))

(main "sample.txt")

(def a (to-array-2d [[1 2 3][4 5 6]]))

(aget a 1 0)
