(require '[clojure.string :as str])
(require '[clojure.set :as c-set])

(defn update-cards-helper [cards c n k final]
  (if (or (= k c) (= n final))
      cards
      (update-cards-helper
        (assoc cards n (+ (get cards n) c))
        c
        (+ n 1)
        (+ k 1)
        final)))

(defn update-cards [cards counts n final]
  (if (= n final)
      cards
      (let [new-cards (update-cards-helper cards (nth counts n) (+ n 1) 0 final)]
        (update-cards new-cards counts (+ n 1) final))))

(defn build-cards [n]
  (into {} (map #(-> [%1 1]) (range n))))

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
       (count x)))

(defn main [file-name]
  (let [lines (->> (slurp file-name) (str/split-lines))
        cards (build-cards (count lines))
        counts (map helper lines)]
    ;(->> 
      (update-cards cards counts 0 (count lines))))
         
         ;(vals)
         ;(apply +))))

(main "sample.txt")
