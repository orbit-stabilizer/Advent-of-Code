(require '[clojure.string :as str])

(defn get-number [string]
  (let [first-number (re-find #"[\d+]" string)
        last-number (re-find #"[\d+](?!.*\d)" string)]
    (Integer/parseInt (str first-number last-number))))

(defn main [file-name]
  (as-> (slurp file-name) x
      (str/split x "\n")
      (map get-number x)
      (reduce + x)))

(main "document.txt")
