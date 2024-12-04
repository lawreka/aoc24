(ns aoc24.4
  (:require [clojure.string :as str]))

(def test-input
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(slurp "input/day4.txt")

(defn loop-thru [lines test-condition]
  (let [line-length (count (first lines))
        number-of-lines (count lines)]
    (loop [line 0
           char 0
           max-char (dec line-length)
           max-line (dec number-of-lines)
           acc 0]
      (if (> line max-line)
        ;; done
        acc
        (if (> char max-char)
          ;; next line
          (recur (inc line) 0 max-char max-line acc)
          ;; test at char
          (if (test-condition line char lines max-char max-line)
            (recur line (inc char) max-char max-line (inc acc))
            (recur line (inc char) max-char max-line acc)))))))

(defn solve [input]
  (let [lines (str/split-lines input)
        across-or-backwards (loop-thru lines (fn [line char lines max-char _max-line]
                                               (or (and (= "X" (str (nth (nth lines line) char)))
                                                        (<= (+ 3 char) max-char)
                                                        (= "M" (str (nth (nth lines line) (inc char))))
                                                        (= "A" (str (nth (nth lines line) (+ 2 char))))
                                                        (= "S" (str (nth (nth lines line) (+ 3 char)))))
                                                   (and (= "S" (str (nth (nth lines line) char)))
                                                        (<= (+ 3 char) max-char)
                                                        (= "A" (str (nth (nth lines line) (inc char))))
                                                        (= "M" (str (nth (nth lines line) (+ 2 char))))
                                                        (= "X" (str (nth (nth lines line) (+ 3 char))))))))
        down-or-up (loop-thru lines (fn [line char lines _max-char max-line]
                                      (or (and (= "X" (str (nth (nth lines line) char)))
                                               (<= (+ 3 line) max-line)
                                               (= "M" (str (nth (nth lines (inc line)) char)))
                                               (= "A" (str (nth (nth lines (+ 2 line)) char)))
                                               (= "S" (str (nth (nth lines (+ 3 line)) char))))
                                          (and (= "S" (str (nth (nth lines line) char)))
                                               (<= (+ 3 line) max-line)
                                               (= "A" (str (nth (nth lines (inc line)) char)))
                                               (= "M" (str (nth (nth lines (+ 2 line)) char)))
                                               (= "X" (str (nth (nth lines (+ 3 line)) char)))))))
        diag-right (loop-thru lines (fn [line char lines max-char max-line]
                                      (or (and (= "X" (str (nth (nth lines line) char)))
                                               (<= (+ 3 line) max-line)
                                               (<= (+ 3 char) max-char)
                                               (= "M" (str (nth (nth lines (inc line)) (inc char))))
                                               (= "A" (str (nth (nth lines (+ 2 line)) (+ 2 char))))
                                               (= "S" (str (nth (nth lines (+ 3 line)) (+ 3 char)))))
                                          (and (= "S" (str (nth (nth lines line) char)))
                                               (<= (+ 3 line) max-line)
                                               (<= (+ 3 char) max-char)
                                               (= "A" (str (nth (nth lines (inc line)) (inc char))))
                                               (= "M" (str (nth (nth lines (+ 2 line)) (+ 2 char))))
                                               (= "X" (str (nth (nth lines (+ 3 line)) (+ 3 char))))))))
        diag-left (loop-thru lines (fn [line char lines _max-char max-line]
                                     (or (and (= "X" (str (nth (nth lines line) char)))
                                              (<= (+ 3 line) max-line)
                                              (>= (- char 3) 0)
                                              (= "M" (str (nth (nth lines (inc line)) (dec char))))
                                              (= "A" (str (nth (nth lines (+ 2 line)) (- char 2))))
                                              (= "S" (str (nth (nth lines (+ 3 line)) (- char 3)))))
                                         (and (= "S" (str (nth (nth lines line) char)))
                                              (<= (+ 3 line) max-line)
                                              (>= (- char 3) 0)
                                              (= "A" (str (nth (nth lines (inc line)) (dec char))))
                                              (= "M" (str (nth (nth lines (+ 2 line)) (- char 2))))
                                              (= "X" (str (nth (nth lines (+ 3 line)) (- char 3))))))))]
    (+ across-or-backwards down-or-up diag-right diag-left)))

(solve test-input)
;; => 18
(solve (slurp "input/day4.txt"))
;; => 2554

(defn solve2 [input]
  (let [lines (str/split-lines input)
        ;; s . s
        ;; . a .
        ;; m . m
        samsam (loop-thru lines (fn [line char lines max-char max-line]
                                  (and (= "S" (str (nth (nth lines line) char)))
                                       (<= (+ 2 line) max-line)
                                       (<= (+ 2 char) max-char)
                                       (= "S" (str (nth (nth lines line) (+ 2 char))))
                                       (= "A" (str (nth (nth lines (inc line)) (inc char))))
                                       (= "M" (str (nth (nth lines (+ 2 line)) char)))
                                       (= "M" (str (nth (nth lines (+ 2 line)) (+ 2 char)))))))
        ;; m . m
        ;; . a .
        ;; s . s
        masmas (loop-thru lines (fn [line char lines max-char max-line]
                                  (and (= "M" (str (nth (nth lines line) char)))
                                       (<= (+ 2 line) max-line)
                                       (<= (+ 2 char) max-char)
                                       (= "M" (str (nth (nth lines line) (+ 2 char))))
                                       (= "A" (str (nth (nth lines (inc line)) (inc char))))
                                       (= "S" (str (nth (nth lines (+ 2 line)) char)))
                                       (= "S" (str (nth (nth lines (+ 2 line)) (+ 2 char)))))))
        ;; s . m
        ;; . a .
        ;; s . m
        sammas (loop-thru lines (fn [line char lines max-char max-line]
                                  (and (= "S" (str (nth (nth lines line) char)))
                                       (<= (+ 2 line) max-line)
                                       (<= (+ 2 char) max-char)
                                       (= "M" (str (nth (nth lines line) (+ 2 char))))
                                       (= "A" (str (nth (nth lines (inc line)) (inc char))))
                                       (= "S" (str (nth (nth lines (+ 2 line)) char)))
                                       (= "M" (str (nth (nth lines (+ 2 line)) (+ 2 char)))))))
        ;; m . s
        ;; . a .
        ;; m . s
        massam (loop-thru lines (fn [line char lines max-char max-line]
                                  (and (= "M" (str (nth (nth lines line) char)))
                                       (<= (+ 2 line) max-line)
                                       (<= (+ 2 char) max-char)
                                       (= "S" (str (nth (nth lines line) (+ 2 char))))
                                       (= "A" (str (nth (nth lines (inc line)) (inc char))))
                                       (= "M" (str (nth (nth lines (+ 2 line)) char)))
                                       (= "S" (str (nth (nth lines (+ 2 line)) (+ 2 char)))))))]
    (+ samsam masmas sammas massam)))

(solve2 test-input)
;; => 9
(solve2 (slurp "input/day4.txt"))
;; => 1916
