(require '[clojure.string :as str]
         '[clojure.spec.alpha :as s]
         '[clojure.set :as set])

(def sample-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")


(defn quick-parsing [input]
  (let [lines (str/split input #"\n\n")
        passports (map #(set (map second (re-seq #"(\w+):" %))) lines)]
    passports))

(def MANDATORY-FIELDS #{"hgt" "pid" "byr" "eyr" "iyr" "ecl" "hcl"})

(defn valid? [passport] (empty? (set/difference MANDATORY-FIELDS passport)))

(defn solve-part-1 [input]
  (count (filter valid? (quick-parsing input))))

(solve-part-1 sample-input)

(solve-part-1 (slurp "day04/input"))
;; 230

(comment
  "byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.")

(defn safe-parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception e s)))

(defn parse-height [height]
  (let [[_ v measure] (re-find #"(\d+)(cm|in)" height)]
    (case measure
      "cm" {:unit "cm" :value (safe-parse-int v)}
      "in" {:unit "in" :value (safe-parse-int v)}
      height)))

(defn parse-field [field]
  (let [[key value] (str/split field #":")
        read-value (case key
                     "byr" safe-parse-int
                     "iyr" safe-parse-int
                     "eyr" safe-parse-int
                     "hgt" parse-height
                     identity)]
    [(keyword key) (read-value value)]))

(defn parse-passport [passport-input]
  (let [fields (str/split passport-input #"(\n| )")]
    (into {} (map parse-field) fields)))

(parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm")

(defn parse-input [input]
  (map parse-passport (str/split input #"\n\n")))

(s/def ::byr (s/int-in 1920 (inc 2002)))
(s/def ::iyr (s/int-in 2010 (inc 2020)))
(s/def ::eyr (s/int-in 2020 (inc 2030)))

(s/def ::unit #{"cm" "in"})
(s/def :inches/value (s/int-in 59 (inc 76)))
(s/def :centimeters/value (s/int-in 150 (inc 193)))
(defmulti hgt :unit)
(defmethod hgt "cm" [_]
  (s/keys :req-un [::unit :centimeters/value]))
(defmethod hgt "in" [_]
  (s/keys :req-un [::unit :inches/value]))
(s/def ::hgt (s/multi-spec hgt :unit))

(s/def ::hcl (s/and string? #(re-matches #"#[a-z0-9]{6}" %)))
(s/def ::ecl (s/and string? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))
(s/def ::pid (s/and string? #(re-matches #"\d{9}" %)))
(s/def ::cid any?)
(s/def ::password (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] :opt-un [::cid]))

(assert (s/valid? ::byr 2002))
(assert (not (s/valid? ::byr 2003)))

(assert (s/valid? ::hgt (parse-height "60in")))
(assert (s/valid? ::hgt (parse-height "190cm")))
(assert (not (s/valid? ::hgt "190in")))
(assert (not (s/valid? ::hgt "190")))

(def invalid-passports
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(map #(s/explain-data ::password %) (parse-input invalid-passports))

(def valid-passports
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(map #(s/explain-data ::password %) (parse-input valid-passports))


(defn solve-part-2 [input]
  (->>
   input
   (parse-input)
   (filter #(s/valid? ::password %))
   count))

(solve-part-2 (slurp "day04/input"))
