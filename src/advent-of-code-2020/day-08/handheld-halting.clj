(ns advent-of-code-2020.day-08.handheld-halting
  (:require [clojure.string :as str]))

(def ex (slurp "./src/advent-of-code-2020/day-08/day-08-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-08/day-08.txt"))

(defn to-code [text]
  (->> (str/split text #"\n")
       (map (fn [line] (let [[op num] (str/split line #" ")]
                         [(keyword op) (read-string num)])))
       (into [])))

(count (to-code ex))
((to-code ex) 9)

(defn exec
  ([code] (exec 0 0 code #{} (count code)))
  ([i acc code prevs cnt]
   (let [[op val] (get code i)]
     (cond
       (= i cnt) [:finish acc]
       (prevs i) [:raise acc]
       (= op :nop) (recur (inc i) acc code (conj prevs i) cnt)
       (= op :acc) (recur (inc i) (+ val acc) code (conj prevs i) cnt)
       (= op :jmp) (recur (+ i val) acc code (conj prevs i) cnt)))))

(-> ex to-code exec) ;; 5
(-> inp to-code exec) ;; 2080

(defn gen-var [origin]
  (->> (map-indexed (fn [i v] [i v]) origin)
       (reduce (fn [acc [i [op v]]]
                 (if (#{:nop :jmp} op)
                   (conj acc (assoc origin i [(if (= :nop op) :jmp :nop) v]))
                   acc)) [])))

(defn success-version-acc [code]
  (->> code
       to-code
       gen-var
       (some #(let [[rslt acc] (exec %)]
                (if (= :finish rslt)
                  acc)))))

(success-version-acc ex) ;; 8
(success-version-acc inp) ;; 2477
