(ns aoc24.5
  (:require [clojure.string :as str]))

(def test-input
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(slurp "input/day5.txt")

(defn get-applicable-rules [rules update]
  (loop [rules rules
         applicable []]
    (if (nil? (first rules))
      applicable
      (let [rule (first rules)
            [before after] rule]
        (if (and (contains? (set update) before)
                 (contains? (set update) after))
          (recur (rest rules) (into applicable [rule]))
          (recur (rest rules) applicable))))))

(defn test-rules [rules update]
  (every? true? (map (fn [rule]
                       (let [[before after] rule]
                         (< (.indexOf update before) (.indexOf update after)))) rules)))

(defn solve [input]
  (let [[rules lists] (str/split input #"\n\n")
        updates (->> (str/split-lines lists)
                     (map #(str/split % #",")))
        page-nums (->> (str/split-lines rules)
                       (map #(str/split % #"\|")))
        in-the-right-order (loop [updates updates
                                  kept []]
                             (if (nil? (first updates))
                               kept
                               (let [update (first updates)
                                     applicable-rules (get-applicable-rules page-nums update)]
                                 (if (test-rules applicable-rules update)
                                   (recur (rest updates) (into kept [update]))
                                   (recur (rest updates) kept)))))
        middle-numbers (map (fn [update]
                              (let [middle-index (quot (count update) 2)]
                                (-> (nth update middle-index)
                                    Integer/parseInt))) in-the-right-order)]
    (reduce + middle-numbers)))

(solve test-input)
;; => 143
(solve (slurp "input/day5.txt"))
;; => 4185

(defn fix-sorting [{:keys [update rules]}]
  (loop [update update
         rules rules]
    (let [failed-rules (remove (fn [rule]
                                 (let [[before after] rule]
                                   (< (.indexOf update before) (.indexOf update after)))) rules)]
      (if (nil? (first failed-rules))
        update
        (let [[_before after] (first failed-rules)
              re-sorted (-> (remove #(= after %) update)
                            vec
                            (conj after))]
          (recur re-sorted rules))))))

(defn solve2 [input]
  (let [[rules lists] (str/split input #"\n\n")
        updates (->> (str/split-lines lists)
                     (map #(str/split % #",")))
        page-nums (->> (str/split-lines rules)
                       (map #(str/split % #"\|")))
        in-the-wrong-order (loop [updates updates
                                  kept []]
                             (if (nil? (first updates))
                               kept
                               (let [update (first updates)
                                     applicable-rules (get-applicable-rules page-nums update)]
                                 (if (not (test-rules applicable-rules update))
                                   (recur (rest updates) (into kept [{:update update
                                                                      :rules applicable-rules}]))
                                   (recur (rest updates) kept)))))
        fixed-lists (map (fn [unsorted-update]
                           (fix-sorting unsorted-update))
                         in-the-wrong-order)
        middle-numbers (map (fn [update]
                              (let [middle-index (quot (count update) 2)]
                                (-> (nth update middle-index)
                                    Integer/parseInt))) fixed-lists)]
    (reduce + middle-numbers)))

(solve2 test-input)
;; => 123
(solve2 (slurp "input/day5.txt"))
;; => 4480
