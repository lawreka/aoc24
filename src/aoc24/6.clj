(ns aoc24.6
  (:require [clojure.string :as str]))

(def test-input
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(slurp "input/day6.txt")

(defn grid [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line]
                 (keep-indexed (fn [x char]
                                 {:x x :y y :char (str char)})
                               line))
               (range))))

(defn grid-pos-key [{:keys [x y]}]
  (keyword (str x "-" y)))

(defn grid-map [grid]
  (into {} (map (fn [{:keys [x y char] :as pos}]
                  {(grid-pos-key pos) {:x x :y y :char char}}) grid)))

(defn get-start-pos [grid-map]
  (-> (filter (fn [[_pos {:keys [_x _y char]}]]
                (= "^" char))
              grid-map)
      first
      val))

(defn get-next-pos [{:keys [x y]} dir]
  (cond
    (= dir :north) {:x x :y (dec y)}
    (= dir :east) {:x (inc x) :y y}
    (= dir :south) {:x x :y (inc y)}
    (= dir :west) {:x (dec x) :y y}))

(defn get-char-at-pos [grid pos]
  (->> (grid-pos-key pos)
       (get grid)
       :char))

(defn turn-right [dir]
  (cond
    (= dir :north) :east
    (= dir :east) :south
    (= dir :south) :west
    (= dir :west) :north))

(defn avoid-objects [char-at-next-pos dir]
  (if (= "#" char-at-next-pos)
    (turn-right dir)
    dir))

(defn update-grid-with-X [grid pos]
  (let [updated-grid (assoc-in grid [(grid-pos-key pos) :char] "X")]
    updated-grid))

(defn solve [input]
  (let [grid-map (grid-map (grid input))
        start-pos (get-start-pos grid-map)
        patrol (loop [grid grid-map
                      pos start-pos
                      dir :north]
                 (let [next-pos (get-next-pos pos dir)
                       char-at-next-pos (get-char-at-pos grid next-pos)
                       object-avoiding-dir (avoid-objects char-at-next-pos dir)
                       real-next-pos (get-next-pos pos object-avoiding-dir)
                       updated-grid (update-grid-with-X grid pos)]
                   (if (nil? char-at-next-pos)
                     updated-grid
                     (recur updated-grid real-next-pos object-avoiding-dir))))
        Xs (filter (fn [[_grid-pos-key {:keys [_x _y char]}]]
                     (= char "X")) patrol)]
    (count Xs)))

(solve test-input)
;; => 41
(solve (slurp "input/day6.txt"))
;; => 5516

(defn move [grid pos dir]
  (loop [pos pos
         dir dir]
    (let [next-pos (get-next-pos pos dir)
          char-at-next-pos (get-char-at-pos grid next-pos)
          turn? (= "#" char-at-next-pos)]
      (if (nil? char-at-next-pos)
        [nil nil]
        (if turn?
          (recur pos (turn-right dir))
          [next-pos dir])))))

(defn grid-pos-dir-key [{:keys [x y]} dir]
  (keyword (str x "-" y "-" (name dir))))

(defn part1-contd [input]
  (let [grid-map (grid-map (grid input))
        start-pos (get-start-pos grid-map)
        patrol (loop [grid grid-map
                      pos start-pos
                      dir :north
                      visited []]
                 (let [[next-pos next-dir] (move grid-map pos dir)]
                   (if (nil? next-pos)
                     visited
                     (recur grid next-pos next-dir (into visited {(grid-pos-key next-pos)
                                                                  {:pos next-pos
                                                                   :dir next-dir}})))))]
    patrol))

(defn update-grid-with-# [grid pos]
  (let [updated-grid (assoc-in grid [(grid-pos-key pos) :char] "#")]
    updated-grid))

(defn test-loop [grid start-key start-pos start-dir]
  (loop [grid grid
         pos start-pos
         dir start-dir
         visited [start-key]]
    (let [[next-pos next-dir] (move grid pos dir)]
      (if (nil? next-pos)
        false
        (let [visit-key (grid-pos-dir-key next-pos next-dir)
              looped? (contains? (set visited) visit-key)]
          (if looped?
            true
            (recur grid next-pos next-dir (into visited [visit-key]))))))))

(defn solve2 [input]
  (let [visited (part1-contd input)
        grid-map (grid-map (grid input))
        start-pos (get-start-pos grid-map)
        start-key (grid-pos-dir-key start-pos :north)
        obstacles-that-make-loops (map (fn [[_k {:keys [pos dir]}]]
                                         (let [[next-pos _next-dir] (move grid-map pos dir)]
                                           (when next-pos
                                             (let [test-grid (update-grid-with-# grid-map next-pos)
                                                   obstacle-key (grid-pos-key next-pos)
                                                   loop? (test-loop test-grid start-key start-pos :north)]
                                               (when loop?
                                                 obstacle-key)))))
                                       visited)]
    (count (remove nil? (set obstacles-that-make-loops)))))

(solve2 test-input)
;; obstacles should be at 3-6, 6-7, 7-7, 1-8, 3-8, and 7-9
;; => 6
(time (solve2 (slurp "input/day6.txt")))
;; => 1488 (too low!)
;; => 1500 (too low!)
;; => 2258 (too high!)
;; => 2008 (after running for 2379249.502667 msecs x_x)
