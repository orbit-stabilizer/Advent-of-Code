(require '[clojure.string :as str])

(defn get-num [string]
  (let [[first-num last-num] ((juxt first last) (re-seq #"\d" string))]
        (parse-long (str first-num last-num))))

(defn transform-line [line]
  (-> line 
    (str/replace #"one" "o1ne")
    (str/replace #"two" "t2wo")
    (str/replace #"three" "t3hree")
    (str/replace #"four" "f4our")
    (str/replace #"five" "f5ive")
    (str/replace #"six" "s6ix")
    (str/replace #"seven" "s7even")
    (str/replace #"eight" "e8ight")
    (str/replace #"nine" "n9ine")))

(defn main [file-name transformer]
  (->> (slurp file-name)
       (str/split-lines)
       (map transformer)
       (map get-num)
       (reduce +)))

(main "document.txt" identity)
(main "document.txt" transform-line)
