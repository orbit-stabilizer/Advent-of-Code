(require '[clojure.string :as str])

(defn possible-game? [game]
  (and (<= (:red game) 12)
       (<= (:green game) 13)
       (<= (:blue game) 14)))

(defn get-max-colour [line colour]
  (let [pattern (re-pattern (str "(\\d+) " colour))]
    (as-> (re-seq pattern line) x
       (apply max (map #(parse-long (nth %1 1)) x)))))

(defn make-max-game [line]
 (let [id (parse-long (re-find #"\d+" line))
       [red green blue] (map (partial get-max-colour line) ["red" "green" "blue"])]
   {:id id :red red :green green :blue blue}))

(defn part-one-transformer [games]
  (->> games
       (filter possible-game?)
       (map :id)))

(defn part-two-transformer [games]
  (->> games
       (map #(apply * ((juxt :red :green :blue) %1)))))
  
(defn main [file-name transformer]
  (->> (slurp file-name) 
    (str/split-lines)
    (map make-max-game)
    (transformer)
    (apply +)))

(main "z_input.txt" part-one-transformer)
(main "z_input.txt" part-two-transformer)
