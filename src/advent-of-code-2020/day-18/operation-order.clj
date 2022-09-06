(ns advent-of-code-2020.day-18.operation-order
  (:require [clojure.string :as s]))

(def ex1 "1 + 2 * 3 + 4 * 5 + 6")
(def ex2 "1 + (2 * 3) + (4 * (5 + 6))")
(def inp (slurp "./src/advent-of-code-2020/day-18/inp.txt"))

(defn formula [op n1 n2]
  (str (eval (read-string (str "(" op " " n1 " " n2 ")")))))

(defn calc-once [text]
  (let [[exp1 n1 op1 n2 exp2 n3 op2 n4] (re-find #"\((\d+) (\S+) (\d+)\)|((\d+) (\S+) (\d+))" text)]
    (cond
      exp2 (-> text (s/replace-first exp2 (formula op2 n3 n4)))
      exp1 (-> text (s/replace-first exp1 (formula op1 n1 n2)))
      :else text)))

(defn calc [text]
  (let [result (calc-once text)]
    (if-not (= text result)
      (calc result)
      text)))

(calc ex2) ;; 51

(defn sum-homework-with [cal]
  (->> (s/split-lines inp)
       (reduce (fn [acc line]
                 (+ acc (read-string (cal line)))) 0)))

(sum-homework-with calc) ;; 18213007238947

(defn adv-calc-once [text]
  (let [[find-p-all find-p] (re-find #"\(([^()]+)\)" text)
        find-n (re-find #"\d+ \+ \d+" text)]
    (cond
      (some? find-p) (-> text (s/replace-first find-p-all (adv-calc find-p)))
      (some? find-n) (-> text (s/replace-first find-n (calc-once find-n)))
      :else (calc-once text))))

(defn adv-calc [text]
  (let [result (adv-calc-once text)]
    (if-not (= text result)
      (adv-calc result)
      text)))

(adv-calc ex2) ;; 51

(sum-homework-with adv-calc) ;; 388966573054664

