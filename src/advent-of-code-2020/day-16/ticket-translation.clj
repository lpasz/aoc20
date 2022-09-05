(ns advent-of-code-2020.day-16.ticket-translation
  (:require [clojure.string :as s]))

(def inp (slurp "./src/advent-of-code-2020/day-16/inp.txt"))
(def ex1 (slurp "./src/advent-of-code-2020/day-16/ex1.txt"))
(def ex2 (slurp "./src/advent-of-code-2020/day-16/ex2.txt"))

;; parsing 
(defn to-text-range [text]
  (-> text (s/replace #".*: " "") (s/split #"(-| or )")))

(defn to-ranges [text]
  (->> (to-text-range text)
       (map #(Integer/parseInt %))
       (partition 2)
       (map (fn [[n1 n2]] (set (range n1 (inc n2)))))))

(defn to-rule [line]
  (let [[_ match] (re-find #"(.*):" line)]
    [(keyword (s/replace match #" " "-")) (to-ranges line)]))

(to-rule "bonk plane: 1-3 or 5-7")

(defn to-rules [rules]
  (into {} (map to-rule rules)))


(defn to-ticket [ticket]
  (->> (s/split (last ticket) #",")
       (map #(Integer/parseInt %))))

(defn to-tickets [tickets]
  (->> tickets
       (rest)
       (map #(to-ticket [nil %]))))

(defn parse-ticket [text]
  (let [[rules my-ticket other-tickets] (->> (s/split text #"\n")
                                             (partition-by empty?)
                                             (filter #(not= % '(""))))]
    {:rules  (to-rules rules)
     :my-ticket (to-ticket my-ticket)
     :other-tickets (to-tickets other-tickets)}))

;; part 1
(defn make-rules [ticket] (apply concat (map (fn [[_k v]] v) (:rules ticket))))

(defn invalid-rule [rule val]
  (nil? (rule val)))

(defn invalid-fields-sum [text]
  (let [ticket (parse-ticket text)
        rules (make-rules ticket)]
    (->> (:other-tickets ticket)
         (concat)
         (flatten)
         (filter (fn [val] (every? #(invalid-rule % val) rules)))
         (apply +))))

(parse-ticket inp)

(invalid-fields-sum ex1) ;; 71
(invalid-fields-sum inp) ;; 22977

;; part 2

(defn valid-ticket? [ticket rules]
  (nil? (some (fn [val] (every? #(invalid-rule % val) rules)) ticket)))


(defn tranpose [m]
  (apply map vector m))

(parse-ticket ex2)
(parse-ticket inp)

(let [[line & rest] [1]]
  rest)

(defn remove-tag [idx-groups tag-to-remove]
  (->> idx-groups
       (map (fn [[idx tags]]
              [idx (->> tags
                        (filter (fn [[tag val]]
                                  (not= tag-to-remove tag))))]))))


(def idx-group [[0 [[:class false] [:row true] [:seat false]]]
                [1 [[:class true] [:row true] [:seat false]]]
                [2 [[:class true] [:row true] [:seat true]]]])

(remove-tag idx-group :seat)

(defn i
  ([x] (do (clojure.pprint/pprint x) x))
  ([y x] (do (clojure.pprint/pprint {y x}) x)))

(defn categorize [result [[idx in-groups] & rest]]
  (if-not (and (empty? in-groups) (empty? rest))
    (let [valid-groups (filter second  in-groups)
          [[tag _]] valid-groups]
      (if (= 1 (count valid-groups))
        (recur (conj result [tag idx]) (remove-tag rest tag))
        (recur result (conj (vec rest) [idx in-groups]))))
    result))

(categorize idx-group [])

(defn discover-vals [text include]
  (let [ticket (parse-ticket text)
        my-ticket (vec (:my-ticket ticket))
        rules (make-rules ticket)]
    (->> (concat (:other-tickets ticket) [(:my-ticket ticket)])
         (filter #(valid-ticket? % rules))
         (tranpose)
         (map (fn [ticket-line]
                (map (fn [[rule-tag rule-set]]
                       [rule-tag (valid-ticket? ticket-line rule-set)]) (:rules ticket))))
         (map-indexed (fn [i v] [i v]))
         (categorize [])
         (into {})
         (filter (fn [[k _]] (re-find include (str k))))
         (map (fn [[_ i]] (my-ticket i)))
         (apply *))))

(re-find #"(class|row|seat)" "homer")

(discover-vals ex2 #"(class|row|seat)") ;; 1716

(discover-vals inp #"departure")

(def my-ticket (:my-ticket (parse-ticket inp)))





