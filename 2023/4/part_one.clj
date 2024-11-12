(require '[clojure.string :as str])
(require '[clojure.set :as c-set])
(require '[clojure.math :as math])

(defn helper [line]
  (as-> line x
       (str/split x #":")
       (nth x 1)
       (str/split x #"\|")
       (map str/trim x)
       (map #(str/split %1 #" ") x)
       (map (fn [x] (filter #(not= "" %1) x)) x)
       (map set x)
       (apply c-set/intersection x)
       (count x)
       (if (> x 0) (math/pow 2 (- x 1)) 0)))

(defn main [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (map helper)
       (apply +)))

(main "z_input.txt")
