(ns advent-of-code-2020.day-05.binary-boarding
  (:require [clojure.string :as str]))

(def rows (into [] (range 1 (inc 127))))
(def columns (into [] (range 1 (inc 7))))
(def seats {:rows rows :cols columns})

(defn to-instructions [text]
  (->> (seq text)
       (map #(cond (= \L %) :left
                   (= \R %) :right
                   (= \F %) :front
                   (= \B %) :back))))

(defn narrow-down [x {:keys [rows cols] :as seats}]
  (cond (= x :front) (assoc seats :rows (take (quot (count rows) 2) rows))
        (= x :back) (assoc seats :rows (drop (quot (count rows) 2) rows))
        (= x :left) (assoc seats :cols (take (quot (inc (count cols)) 2) cols))
        (= x :right) (assoc seats :cols (drop (quot (count cols) 2) cols))))

(defn row-n-seat [text]
  (->> (to-instructions text)
       (reduce #(narrow-down %2 %1) seats)))

(defn seat-id [text]
  (->> (row-n-seat text)
       (#(let [[row] (:rows %) [col] (:cols %)]
           (-> (* row 8)
               (+ col))))))

(row-n-seat "FBFBBFFRLR")

(= (seat-id "BFFFBBFRRR") 567)
(= (seat-id "FFFBBBFRRR") 119)
(= (seat-id "BBFFBBFRLL") 820)

(def input (slurp "src/advent-of-code-2020/day-05/day-05.txt"))

(apply max (map seat-id (str/split input #"\n")))

(apply max (map #((frequencies (seq %)) \B) (str/split input #"\n"))