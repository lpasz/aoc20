(ns advent-of-code-2020.day-07.handy-haversacks
  (:require [clojure.string :as s]))

(def example (slurp "./src/advent-of-code-2020/day-07/day-07-ex.txt"))
(def example2 (slurp "./src/advent-of-code-2020/day-07/day-07-ex2.txt"))

(def input (slurp "./src/advent-of-code-2020/day-07/day-07.txt"))

(defn to-key [text]
  (-> (s/trim text) (s/replace "bags" "bag") (s/replace " " "-") (keyword)))

(to-key "light red bags ")

(defn to-vals [text]
  (->> (-> (s/trim text) (s/replace "bags" "bag") (s/replace "." "") (s/split #", "))
       (map (fn [x]
              (if (= x "no other bag") [:no-other-bag 0]
                  (let [num (re-find #"\d+" x)]
                    [(to-key (s/replace x num "")) (read-string num)]))))
       (into {})))

(to-vals " 5 faded blue bags, 6 dotted black bags.")

(defn rule-set [text]
  (->> (s/split text #"\n")
       (map #(s/split % #"contain"))
       (map (fn [[key val]] [(to-key key) (to-vals val)]))
       (into {})))

(def bags-ex (rule-set example))
(def bags-ex2 (rule-set example2))
(def bags (rule-set input))

(defn contains-bag-color? [color bag bags]
  (some (fn [[sub-bag-color _]] (or (= color sub-bag-color)
                                    (contains-bag-color? color sub-bag-color bags)))
        (bag bags)))

(defn bags-with-shiny-gold [bags]
  (->> (keys bags)
       (filter #(contains-bag-color? :shiny-gold-bag % bags))
       (count)))

;; part 1
(= 4 (bags-with-shiny-gold bags-ex))
(= 115 (bags-with-shiny-gold bags))

(defn count-bag-in-bags [curr-bag bags]
  (->> (curr-bag bags)
       (reduce (fn [acc [next-bag n]]
                 (+ acc n
                    (* n (count-bag-in-bags next-bag bags)))) 0)))

;; part 2
(= 32 (count-bag-in-bags :shiny-gold-bag bags-ex))
(= 126 (count-bag-in-bags :shiny-gold-bag bags-ex2))
(= 1250 (count-bag-in-bags :shiny-gold-bag bags))
