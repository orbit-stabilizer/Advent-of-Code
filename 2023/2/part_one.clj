(require '[clojure.string :as str])

(def max-red-cubes 12)
(def max-green-cubes 13)
(def max-blue-cubes 14)

(defn possible-game? [game]
  (and (<= (get game :red) max-red-cubes)
       (<= (get game :green) max-green-cubes)
       (<= (get game :blue) max-blue-cubes)))

(defn get-max-colour [line colour]
  (let [pattern (re-pattern (str "(\\d+) " colour))]
    (as-> (re-seq pattern line) x
       (apply max (map #(Integer/parseInt (nth %1 1)) x)))))

(defn make-max-game [line]
 (let [id (Integer/parseInt (re-find #"\d+" line))
       red (get-max-colour line "red")
       green (get-max-colour line "green")
       blue (get-max-colour line "blue")]
  {:id id :red red :green green :blue blue}))
  
(defn main [file-name]
  (as-> (slurp file-name) x
    (str/split x #"\n")
    (map make-max-game x)
    (filter possible-game? x)
    (map :id x)
    (apply + x)))
    

(main "z_input.txt")
