(require '[clojure.string :as str]
         '[clojure.spec.alpha :as s])

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

(defn parse-field [field]
  (let [[key value] (str/split field #":")]
    [(keyword key) value]))

(defn parse-passport [passport-input]
  (let [fields (str/split passport-input #"(\n| )")]
    (into {} (map parse-field) fields)))

(parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm")

(defn parse-input [input]
  (map parse-passport (str/split input #"\n\n")))

(parse-input sample-input)

(s/def ::byr string?)
(s/def ::iyr string?)
(s/def ::eyr string?)
(s/def ::hgt string?)
(s/def ::hcl string?)
(s/def ::ecl string?)
(s/def ::pid string?)
(s/def ::cid string?)
(s/def ::password (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] :opt-un [::cid]))


(defn solve-part-1 [input]
  (count (filter #(s/valid? ::password %) (parse-input input))))

(solve-part-1 sample-input)

(solve-part-1 (slurp "day04/input"))
;; 230
