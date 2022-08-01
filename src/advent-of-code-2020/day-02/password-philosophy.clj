(ns advent-of-code-2020.day-02.password-philosophy
  (:require [clojure.string :as str]))

(defn prepare-input [text]
  (->> (str/split text #"(\n)|(: )|(-)|( )")
       (partition 4)))

(defn count-in [val input]
  (count (filter #(= val (str %)) (seq input))))

(defn valid? [[min max val input]]
  (let [quant (count-in val input)
        min (read-string min)
        max (read-string max)]
    (and (>= quant min) (>= max quant))))

(defn count-valid-password [text]
  (->> (prepare-input text)
       (filter #(valid? %))
       (count)))

(count-in "c" "cccccccccc")

(valid? ["2" "9" "c" "cccccccccc"])

(def ex-input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: cccccccccc")
(def input (slurp "src/advent-of-code-2020/day-02/day-02.txt"))

(count-valid-password ex-input)
(count-valid-password input)

(defn new-valid? [[min max val input]]
  (let [val (first (seq val))
        inputs (vec (seq input))
        idx-1 (dec (read-string min))
        idx-2 (dec (read-string max))
        idx-1? (= (inputs idx-1) val)
        idx-2? (= (inputs idx-2) val)]
    (or (and idx-1? (not idx-2?))
        (and (not idx-1?) idx-2?))))

(defn count-new-valid-password [text]
  (->> (prepare-input text)
       (filter #(new-valid? %))
       (count)))

(prepare-input ex-input)

(map new-valid? '(("1" "3" "a" "abcde") 
                  ("1" "3" "b" "cdefg") 
                  ("2" "9" "c" "cccccccccc")))

(count-new-valid-password ex-input)
(count-new-valid-password input)