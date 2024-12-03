(ns aoc24.3
  (:require [clojure.string :as str]))

(def test-input
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(slurp "input/day3.txt")

(defn solve [input]
  (let [matches (re-seq #"mul\(\d+,\d+\)" input)
        mults (map (fn [s]
                     (let [first-num (-> s
                                         (str/split #"mul\(")
                                         last
                                         (str/split #",")
                                         first)
                           second-num (-> s
                                          (str/split #",")
                                          last
                                          (str/split #"\)")
                                          first)]
                       (* (Integer/parseInt first-num) (Integer/parseInt second-num)))) matches)]
    (reduce + mults)))

(solve test-input)
(solve (slurp "input/day3.txt"))

(def test-input2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn solve2 [input]
  (let [dont-sections (str/split input #"don't\(\)")
        get-mults (fn [s]
                    (when-let [matches (re-seq #"mul\(\d+,\d+\)" s)]
                      (map (fn [match]
                             (let [first-num (-> match
                                                 (str/split #"mul\(")
                                                 last
                                                 (str/split #",")
                                                 first)
                                   second-num (-> match
                                                  (str/split #",")
                                                  last
                                                  (str/split #"\)")
                                                  first)]
                               (* (Integer/parseInt first-num) (Integer/parseInt second-num)))) matches)))
        first-mults (get-mults (first dont-sections))
        rest-mults (map (fn [s]
                          (when-let [do-sections (-> (str/split s #"do\(\)")
                                                     rest)]
                            (map #(get-mults %) do-sections))) (rest dont-sections))
        all-mults (remove nil? (concat first-mults (flatten rest-mults)))]
    (reduce + all-mults)))

(solve2 test-input2)
(solve2 (slurp "input/day3.txt"))
