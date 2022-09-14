(ns advent-of-code-2020.day-19.monster-messages
  (:require [clojure.string :as s]))

(def inp (slurp "./src/advent-of-code-2020/day-19/inp.txt"))
(def ex1 (slurp "./src/advent-of-code-2020/day-19/ex1.txt"))
(def ex2 (slurp "./src/advent-of-code-2020/day-19/ex2.txt"))

(defn remap-val [val]
  (if (re-find #"[a-zA-Z]+" val)
    (first (read-string val))
    (->> (s/split val #"\|")
         (map #(read-string (str "[" % "]")))
         (into []))))

(defn message-validator [text]
  (->> (s/split-lines text)
       (map #(s/split % #": "))
       (map (fn [[key val]] [(read-string key) (remap-val val)]))
       (into (sorted-map))))

(def ex1-messages (s/split-lines (second (s/split ex1 #"\n\n"))))
(def ex1-rules (message-validator (first (s/split ex1 #"\n\n"))))
(def ex2-messages (s/split-lines (second (s/split ex2 #"\n\n"))))
(def ex2-rules (message-validator (first (s/split ex2 #"\n\n"))))
(def inp-messages (s/split-lines (second (s/split inp #"\n\n"))))
(def inp-rules (message-validator (first (s/split inp #"\n\n"))))

(defn consume-msgs [msgs start paths]
  (let [next-paths (paths start)]
    (cond
      ;; i have no more valid messages to attempt to parse, go back
      (empty? msgs) nil

      ;; i hitted the end of the tree nodes, attempt to pop the char from string start
      (char? next-paths) (for [msg msgs
                               :when (= next-paths (first msg))]
                           (rest msg))

      :else (mapcat (fn [next-path]
                      (reduce (fn [msgs node]
                                (consume-msgs msgs node paths))
                              msgs
                              next-path))
                    next-paths))))

(defn count-valid-msgs [msgs rules]
  (->> (consume-msgs msgs 0 rules)
       (filter empty?)
       (count)))

(count-valid-msgs ex1-messages ex1-rules) ;; 2
(count-valid-msgs ex2-messages ex2-rules) ;; 3
(count-valid-msgs inp-messages inp-rules) ;; 156

(defn update-rules [rules]
  (assoc rules
         8 [[42] [42 8]]
         11 [[42 31] [42 11 31]]))

(->> ex2-rules
     (update-rules)
     (count-valid-msgs ex2-messages)) ;; 12

(->> inp-rules
     (update-rules)
     (count-valid-msgs inp-messages)) ;;  363