(ns advent-of-code-2020.day-07.handy-haversacks
  (:require [clojure.string :as s]))

(def example (slurp "./src/advent-of-code-2020/day-07/day-07-ex.txt"))
(def example2 (slurp "./src/advent-of-code-2020/day-07/day-07-ex2.txt"))

(def input (slurp "./src/advent-of-code-2020/day-07/day-07.txt"))

(defn to-key [text]
  (-> (s/trim text)
      (s/replace "bags" "bag")
      (s/replace " " "-")
      (keyword)))

(to-key "light red bags ")

(defn to-vals [text]
  (->> (-> (s/trim text)
           (s/replace "bags" "bag")
           (s/replace "." "")
           (s/split #", "))
       (map (fn [x]
              (if (= x "no other bag") [:no-other-bag 0]
                  (let [num (re-find #"\d+" x)]
                    [(to-key (s/replace x num "")) (read-string num)]))))
       (into {})))

(to-vals " 5 faded blue bags, 6 dotted black bags.")

(defn rule-set [text]
  (->> (s/split text #"\n")
       (map #(s/split % #"contain"))
       (map (fn [[key val]]
              [(to-key key) (to-vals val)]))
       (into {})))

(def bags-ex (rule-set example))
(def bags-ex2 (rule-set example2))
(def bags (rule-set input))

(defn tree-v [k visited bags]
  (let [k (if (keyword? k) k (first k))]
    (map #(cond
            (visited %) false
            (= % :no-other-bag) false
            (= % :shiny-gold-bag) true
            :else (tree-v % (conj visited %) bags))
         (keys (bags k)))))

(defn bags-with-shiny-gold [bags] (->> (map #(tree-v  % #{} bags) (keys bags))
                                       (map flatten)
                                       (map #(not-every? false? %))
                                       (filter true?)
                                       (count)))

(= 4 (bags-with-shiny-gold bags-ex))
(= 115 (bags-with-shiny-gold bags))

(defn tree-vd [bags [start n]]
  (if-let [nexts (bags start)]
    (+ n (reduce #(+ %1 (* n (tree-vd bags %2))) 0 nexts))
    0))

(defn how-many-bags-inside [bag bags]
  (dec (tree-vd bags [bag 1])))

;; why is it wrong by one ???
(= 32 (how-many-bags-inside :shiny-gold-bag bags-ex))
(= 126 (how-many-bags-inside :shiny-gold-bag bags-ex2))
(= 1250 (how-many-bags-inside :shiny-gold-bag bags))
