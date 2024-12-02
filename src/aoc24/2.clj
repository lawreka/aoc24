(ns aoc24.2
  (:require [clojure.string :as str]))

(def test-input
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(slurp "input/day2.txt")

(defn solve [input]
  (let [lists (->>
               (str/split-lines input)
               (map #(str/split % #" ")))
        asc? (fn [list]
               (every? true? (map #(< (-> % first Integer/parseInt) (-> % last Integer/parseInt)) (partition 2 1 list))))
        desc? (fn [list]
                (every? true? (map #(> (-> % first Integer/parseInt) (-> % last Integer/parseInt)) (partition 2 1 list))))
        safe? (fn [list]
                (every? true? (map (fn [pair]
                                     (let [diff (abs (- (-> pair first Integer/parseInt) (-> pair last Integer/parseInt)))]
                                       (and (>= diff 1)
                                            (<= diff 3)))) (partition 2 1 list))))]
    (-> (filter (fn [list]
                  (and (or (asc? list)
                           (desc? list))
                       (safe? list))) lists)
        count)))

(solve test-input)
(solve (slurp "input/day2.txt"))

(defn solve2 [input]
  (let [lists (->>
               (str/split-lines input)
               (map #(str/split % #" ")))
        asc? (fn [list]
               (every? true? (map #(< (-> % first Integer/parseInt) (-> % last Integer/parseInt)) (partition 2 1 list))))
        desc? (fn [list]
                (every? true? (map #(> (-> % first Integer/parseInt) (-> % last Integer/parseInt)) (partition 2 1 list))))
        safe? (fn [list]
                (every? true? (map (fn [pair]
                                     (let [diff (abs (- (-> pair first Integer/parseInt) (-> pair last Integer/parseInt)))]
                                       (and (>= diff 1)
                                            (<= diff 3)))) (partition 2 1 list))))
        test-modified-list (fn [list]
                             (map-indexed (fn [idx _x]
                                            (let [[split1 split2] (split-at idx list)
                                                  modified-list (concat split1 (rest split2))]
                                              (and (or (asc? modified-list)
                                                       (desc? modified-list))
                                                   (safe? modified-list))))
                                          list))]
    (-> (filter (fn [list]
                  (or (and (or (asc? list)
                               (desc? list))
                           (safe? list))
                      (some true? (test-modified-list list)))) lists)
        count)))

(solve2 test-input)
(solve2 (slurp "input/day2.txt"))
