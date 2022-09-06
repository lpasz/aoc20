(ns advent-of-code-2020.day-18.operation-order
  (:require [clojure.string :as s]))

(def ex1 "1 + 2 * 3 + 4 * 5 + 6")
(def ex2 "1 + (2 * 3) + (4 * (5 + 6))")
(def inp (slurp "./src/advent-of-code-2020/day-18/inp.txt"))

(defn formula [op n1 n2]
  (str (eval (read-string (str "(" op " " n1 " " n2 ")")))))

(defn do-calc [text patterns]
  (loop [text text]
    (if-let [[parens no-parens] (re-find #"\(([^()]+)\)" text)]
      (recur (s/replace-first text parens (do-calc no-parens patterns)))
      (if-let [[exp n1 op n2] (->> patterns
                                   (some (fn [pattern]
                                           (re-find
                                            (re-pattern (str #"(\d+) " pattern #" (\d+)"))
                                            text))))]
        (recur (s/replace-first text exp (formula op n1 n2)))
        text))))

(defn calc [text] (-> (do-calc text [#"(\+|\*)"])
                      (Long/parseLong)))

(calc ex2) ;; 51

(defn sum-homework-with [cal]
  (->> (s/split-lines inp)
       (reduce (fn [acc line]
                 (+ acc (cal line))) 0)))

(sum-homework-with calc) ;; 18213007238947

(defn adv-calc [text] (-> (do-calc text [#"(\+)" #"(\*)"])
                          (Long/parseLong)))

(adv-calc ex2) ;; 51

(sum-homework-with adv-calc) ;; 388966573054664
