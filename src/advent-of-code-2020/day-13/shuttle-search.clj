(ns advent-of-code-2020.day-13.shuttle-search
  (:require [clojure.string :as str]))

(def ex (slurp "./src/advent-of-code-2020/day-13/day-13-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-13/day-13.txt"))

(defn to-inf [text]
  (let [[start-at buses] (str/split text #"\n")]
    [(Integer/parseInt start-at)
     (->> (str/split buses #",")
          (filter #(not= % "x"))
          (map #(Integer/parseInt %)))]))

(defn min-of-wait [text]
  (let [[start-at buses] (to-inf text)]
    (->> (iterate inc start-at)
         (some (fn [from-start-at]
                 (if-let [bus (first (filter #(= 0 (rem from-start-at %)) buses))]
                   (* (- from-start-at start-at) bus)))))))

(min-of-wait ex) ;; 295
(min-of-wait inp) ;; 161

(defn get-idx-buses [text]
  (->> (-> (str/split text #"\n")
           (second)
           (str/split #","))
       (keep-indexed #(when-not (= %2 "x") [%1 (Integer/parseInt %2)]))))

(get-idx-buses ex)

(defn next-offset [step offset idx bus]
  (->> (iterate #(+ step %) offset)
       (filter pos?)
       (filter #(zero? (mod (+ idx %) bus)))
       (first)))

(defn next-step-offset [[step offset] [idx bus]]
  [(* step bus)  (next-offset step offset idx bus)])

(defn next-moment [text]
  (->> (get-idx-buses text)
       (reduce next-step-offset [1 0])
       (second)))

(next-moment inp)