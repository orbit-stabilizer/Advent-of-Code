(require '[clojure.core.match :refer [match]])

(defn main [file-name solve]
  (->> (slurp file-name)
       (solve)))

(defn part-one [text]
  (as-> text t
       (re-seq #"mul\((\d+),(\d+)\)" t)
       (for [[_ n m] t]  (* (parse-long n) (parse-long m)))
       (apply + t)))

(defn part-two [text]
  (loop [matches (re-seq #"mul\((\d+),(\d+)\)|don't|do" text)
         sum 0
         skip? false]
     (if (empty? matches)
       sum
       (match (first matches)
         ["don't" nil nil] (recur (rest matches) sum true)
         ["do" nil nil] (recur (rest matches) sum false)
         [_ n m] (recur (rest matches) (if skip? sum (+ sum (* (parse-long n) (parse-long m)))) skip?)))))

(comment
  (empty? ())
  (part-one "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5)")
  (part-two "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (main "sample.txt" part-one)
  (main "input.txt" part-one)
  (main "sample2.txt" part-two)
  (class (cons 1 '(2)))
  (parse-long nil)
  (str 5)
  (defn divisible-by? [n m] (= (mod m n) 0))
  (defn say [n]
       (cond-> nil
         (divisible-by? 3 n) (str "Fizz")
         (divisible-by? 5 n) (str "Buzz")
         :always             (or (str n))))
  (say 15)
  (or "15" "FizzBuzz")
  (print "hi")
  (main "input.txt" part-two))

