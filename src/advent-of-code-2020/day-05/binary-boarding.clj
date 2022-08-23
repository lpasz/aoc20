(ns advent-of-code-2020.day-05.binary-boarding
  (:require [clojure.string :as str]))

(defn to-binary-str [text]
  (->> (seq text) (map {\F 0 \L 0 \B 1 \R 1}) (str/join)))

(defn to-row-n-col [text]
  (let [[row col] (into [] (map (comp str/join to-binary-str) (partition 7 7 [] text)))]
    {:row (Integer/parseInt row 2) :col (Integer/parseInt col 2)}))

(defn to-seat-id [text]
  (Integer/parseInt (to-binary-str text) 2))

(def input (slurp "src/advent-of-code-2020/day-05/day-05.txt"))

(defn ex1 [] (->> (str/split input #"\n")
                  (map to-seat-id)
                  (apply max)))

(ex1)

(defn missing-one
  ([ids] (missing-one (first ids) (rest ids)))
  ([prev ids]
   (if (= (inc prev) (first ids))
     (recur (first ids) (rest ids))
     (first ids))))

(defn ex2 [] (->> (str/split input #"\n")
                  (map to-seat-id)
                  (sort)
                  (partition 2 1)
                  (filter #(= (inc (first %)) (dec (second %))))
                  ((comp inc first first))))

(ex2)

