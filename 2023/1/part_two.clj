(require '[clojure.string :as str])

(defn get-number [string]
  (let [first-number (re-find #"[\d+]" string)
        last-number (re-find #"[\d+](?!.*\d)" string)]
    (Integer/parseInt (str first-number last-number))))

(defn transform-line [line]
  (as-> line l
    (str/replace l #"one" "o1ne")
    (str/replace l #"two" "t2wo")
    (str/replace l #"three" "t3hree")
    (str/replace l #"four" "f4our")
    (str/replace l #"five" "f5ive")
    (str/replace l #"six" "s6ix")
    (str/replace l #"seven" "s7even")
    (str/replace l #"eight" "e8ight")
    (str/replace l #"nine" "n9ine")))

(defn main [file-name]
  (as-> (slurp file-name) x
      (str/split x #"\n")
      (map transform-line x)
      (map get-number x)
      (reduce + x)))

(main "document.txt")
