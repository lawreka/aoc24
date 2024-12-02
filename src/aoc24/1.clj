(ns aoc24.1
  (:require [clojure.string :as str]))

(def test-input
  "3   4
4   3
2   5
1   3
3   9
3   3")

(slurp "input/day1.txt")

(defn solve [input]
  (let [input-lines (str/split-lines input)
        lists (map #(str/split % #"   ") input-lines)
        left-list (-> (map #(-> %
                                first
                                Integer/parseInt) lists)
                      sort)
        right-list (-> (map #(-> %
                                 last
                                 Integer/parseInt) lists)
                       sort)
        diffs (map (fn [n1 n2]
                     (-> (- n1 n2)
                         abs)) left-list right-list)]
    (reduce + diffs)))

(solve test-input)
(solve (slurp "input/day1.txt"))

(defn solve2 [input]
  (let [input-lines (str/split-lines input)
        lists (map #(str/split % #"   ") input-lines)
        left-list (map #(first %) lists)
        right-list (->> (map #(-> %
                                  last
                                  (str " ")) lists)
                        (apply str))
        scores (map (fn [n1]
                      (-> (re-pattern n1)
                          (re-seq  right-list)
                          count
                          (* (Integer/parseInt n1)))) left-list)]
    (reduce + scores)))

(solve2 test-input)
(solve2 (slurp "input/day1.txt"))
