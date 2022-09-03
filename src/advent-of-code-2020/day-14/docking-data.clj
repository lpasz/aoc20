(ns advent-of-code-2020.day-14.docking-data
  (:require [clojure.string :as str]))

(def ex (slurp "./src/advent-of-code-2020/day-14/day-14-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-14/day-14.txt"))

(defn str-int->str-bin [s]
  (-> (Integer/parseInt s)
      (Integer/toBinaryString)))

(defn mem-address [s]
  (->> (re-find #"(\d+)" s)
       (first)
       (Integer/parseInt)))

(mem-address "mem[123]")

(defn to-cmd [[cmd value]]
  (if (= cmd "mask")
    [:mask value]
    [(mem-address cmd) (str-int->str-bin value)]))

(defn pad-to-match [value mask]
  (str
   (apply str (repeat (- (count mask) (count value)) \0))
   value))

(pad-to-match "1011" "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(defn masked-val [value mask]
  (->> (pad-to-match value mask)
       (map (fn [m v] (if (#{\1 \0} m) m v)) mask)
       (apply str)
       (#(Long/parseLong % 2))))

(Long/parseLong "11010001011100100000001001101000010" 2)


(masked-val "1011" "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(defn exec [state [cmd value]]
  (if (= cmd :mask)
    (assoc state :mask value)
    (assoc state cmd (masked-val value (:mask state)))))

(defn state-sum [text]
  (->> (str/split text #"(\n|=)")
       (map str/trim)
       (partition 2)
       (map to-cmd)
       (reduce exec {:mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"})
       (#(dissoc % :mask))
       (reduce (fn [acc [_ value]] (+ acc value)) 0)))

(state-sum ex) ;; 165
(state-sum inp) ;; 11926135976176