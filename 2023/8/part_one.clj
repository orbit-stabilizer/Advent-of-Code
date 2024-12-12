(require '[clojure.string :as str])

(defn cycle-str [coll]
  (lazy-cat coll (cycle-str coll)))

(defn build-directions [document]
  (->> document
       (first)
       (cycle-str)))

(defn build-network [document]
  (->> document
       ((comp next next))
       (map #(str/split % #"="))))

(defn main [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (build-network)))


(main "sample.txt")

