(ns advent-of-code-2020.day-04.passport-processing
  (:require [clojure.string :as str]))

(def example
  (slurp "src/advent-of-code-2020/day-04/day-04-ex.txt"))

(def input
  (slurp "src/advent-of-code-2020/day-04/day-04.txt"))

(defn passport-entry-text->map [passport-entry]
  (->> (str/split passport-entry #"(\s|:)")
       (partition 2)
       (map (fn [[key val]] [(keyword key) val]))
       (into {})))

(def req-keys #{:ecl :pid :eyr :hcl :byr :iyr :hgt})

(defn to-passport-map [text]
  (->> (str/split text #"\n\n")
       (map passport-entry-text->map)))

(defn passport? [passport] (every? (set (keys passport)) req-keys))

(defn count-valid-passports [passports]
  (reduce (fn [acc passport] (if (passport? passport)
                               (inc acc)
                               acc)) 0 passports))


(count (to-passport-map input))

(defn between? [n this that]
  (and (>= n this) (>= that n)))

(between? 2002 1920 2002)

(defn valid-byr? [passport]
  (some-> passport :byr (Integer/parseInt) (between? 1920 2002)))

(valid-byr? {:byr "2002"})
(valid-byr? {:byr "2003"})

(defn valid-iyr? [passport]
  (some-> passport :iyr (Integer/parseInt) (between? 2010 2020)))


(valid-iyr? {:iyr "2010"})
(valid-iyr? {:iyr "2003"})

(defn valid-eyr? [passport]
  (some-> passport :eyr (Integer/parseInt) (between? 2020 2030)))

(valid-eyr? {:eyr "2020"})
(valid-eyr? {:eyr "2003"})

(defn valid-hgt? [passport]
  (cond
    (str/ends-with? (:hgt passport) "cm") (some->> passport
                                                   :hgt
                                                   (re-find #"\d+")
                                                   read-string
                                                   (#(between? % 150 193)))
    (str/ends-with? (:hgt passport) "in") (some->> passport
                                                   :hgt
                                                   (re-find #"\d+")
                                                   read-string
                                                   (#(between? % 59 76)))
    :else false))

(valid-hgt? {:hgt "60in"})
(valid-hgt? {:hgt "190in"})
(valid-hgt? {:hgt "190cm"})
(valid-hgt? {:hgt "19cm"})
(valid-hgt? {:hgt "60"})


(defn valid-hcl? [passport]
  (some->>  passport :hcl (re-matches #"#([0-9]|[a-f]){6}")))

(valid-hcl? {:hcl "#123abc"})
(valid-hcl? {:hcl "#123abz"})
(valid-hcl? {:hcl "123abc"})


(defn valid-ecl? [passport]
  (some-> passport :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))

(valid-ecl? {:ecl "brn"})
(valid-ecl? {:ecl "ben"})

(defn valid-pid? [passport]
  (some->> passport :pid (re-matches #"[0-9]{9}")))

(valid-pid? {:pid "000000001"})
(valid-pid? {:pid "0123456789"})

(def valid "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(def invalid "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
")

(defn valid? [text]
  (count (filter passport? (to-passport-map text))))

(valid? input)

(defn full-valid? [text]
  (->> (to-passport-map text)
       (filter passport?)
       (filter valid-byr?)
       (filter valid-eyr?)
       (filter valid-hgt?)
       (filter valid-iyr?)
       (filter valid-hcl?)
       (filter valid-ecl?)
       (filter valid-pid?)
       (count)))

(full-valid? input)

