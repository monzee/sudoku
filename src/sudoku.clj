(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [rstart (* 3 (int (/ row 3)))
        cstart (* 3 (int (/ col 3)))
        pairs  (coord-pairs [0 1 2])]
    (set (map #(get-in board %) 
              (map (fn [[r c]] [(+ rstart r) (+ cstart c)]) pairs)))))

(defn valid-values-for [board coord]
  (if (not= 0 (value-at board coord))
    #{}
    (apply set/difference (cons all-values
                                (map #(% board coord)
                                     [row-values col-values block-values])))))

(defn filled? [board]
  (every? #(not (some #{0} %)) board))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (->> (coord-pairs [0 1 2])
       (map #(map (partial * 3) %))
       (map #(block-values board %))))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (every? #(% board) [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0, col 0]
    (cond
      (> row 8) nil
      (> col 8) (recur (inc row) 0)
      (= 0 (get-in board [row col])) [row col]
      :else (recur row (inc col)))))

(declare search)
(defn solve [board]
  (cond
    (not (filled? board)) (let [slot (find-empty-point board)
                                xs   (valid-values-for board slot)]
                            (search (map #(set-value-at board slot %) xs)))
    (valid-solution? board) board
    :else []))

(defn- search [[b & bs :as boards]]
  (if (empty? boards)
    []
    (let [solution (solve b)]
      (if (empty? solution)
        (recur bs)
        solution))))

(comment defn solve [board]
  (letfn [(aux [board]
            (if (filled? board)
              (if (valid-solution? board)
                [board]
                [])
              (let [slot (find-empty-point board)]
                (for [n (valid-values-for board slot)
                      :let [new-board (set-value-at board slot n)]
                      solution (aux new-board)]
                  solution))))]
    (first (aux board))))
