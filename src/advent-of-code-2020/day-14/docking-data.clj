(ns advent-of-code-2020.day-14.docking-data
  (:require [clojure.string :as str]))

(def ex (slurp "./src/advent-of-code-2020/day-14/day-14-ex.txt"))
(def ex2 (slurp "./src/advent-of-code-2020/day-14/day-14-ex2.txt"))
ex2
(def inp (slurp "./src/advent-of-code-2020/day-14/day-14.txt"))

(defn str-int->str-bin [s]
  (-> (Integer/parseInt s)
      (Integer/toBinaryString)))

(str-int->str-bin "100")

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

(defn exec-v1 [state [cmd value]]
  (if (= cmd :mask)
    (assoc state :mask value)
    (assoc state cmd (masked-val value (:mask state)))))

(defn state [text version-func]
  (->> (str/split text #"(\n|=)")
       (map str/trim)
       (partition 2)
       (map to-cmd)
       (reduce version-func {})
       (#(dissoc % :mask))))

(defn state-sum [state]
  (apply + (map second state)))

(state-sum (state ex exec-v1)) ;; 165
(state-sum (state inp exec-v1))  ;; 11926135976176

;; v2

(defn quantum-x [s]
  (loop [vs [s]]
    (if (some #(str/includes? % "X") vs)
      (recur (mapcat #(if (str/includes? % "X")
                        [(str/replace-first % "X" "0") (str/replace-first % "X" "1")]
                        [%]) vs))
      vs)))

(quantum-x "000000000000000000000000000000X1101X")

(defn get-addresses-from-mask [address mask]
  (->>   (pad-to-match (str-int->str-bin (str address)) mask)
         (map (fn [m v] (if (= \0 m) v m)) mask)
         (apply str)
         (quantum-x)
         (map #(Long/parseLong % 2))))


(get-addresses-from-mask 42 "00000000000000000000000000000000X0XX")

(defn exec-v2 [state [cmd value]]
  (if (= cmd :mask)
    (assoc state :mask value)
    (->> (get-addresses-from-mask cmd (:mask state))
         (reduce #(assoc %1 %2 (Long/parseLong value 2)) state))))

(state-sum (state inp exec-v2))



