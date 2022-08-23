(ns advent-of-code-2020.day-06.custom-customs
  (:require [clojure.string :as str]))

(def example (slurp "src/advent-of-code-2020/day-06/day-06-ex.txt"))
(def input (slurp "src/advent-of-code-2020/day-06/day-06.txt"))

(defn anyone-yes [text]
  (->> (str/split text #"\n\n")
       (map #(str/replace % #"\n" ""))
       (map (comp count set seq))
       (apply +)))

(= 11 (anyone-yes example))

(anyone-yes input)

(defn contained-in-every [coll]
  (filter (fn [c]
            (every? #((set (seq %1)) c) (rest coll)))
          (first coll)))

(defn every-yes [text]
  (->> (str/split text #"\n\n")
       (map #(str/split % #"\n"))
       (map (comp count contained-in-every))
       (apply +)))

(= 6 (every-yes example))

(every-yes input)
