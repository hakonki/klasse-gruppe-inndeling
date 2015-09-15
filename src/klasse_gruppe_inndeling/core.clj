(ns klasse-gruppe-inndeling.core
  (:require [clj-time.core :as t]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as c]
            [clojure.math.numeric-tower :as math]))

(defn parse-file
  [file-name]
  (map #(s/split % #";") (->> (-> (slurp (io/file
                                          (io/resource 
                                           file-name)))
                                (s/split #"\n")
                                )
                           (filter #(not (.startsWith % "#")))
                           )))

(defn parse-long [s]
  (Long/parseLong (re-find #"\A-?\d+" s)))

(defn get-weight-adjustment
  [n]
  (/ 1 (/ (+ (math/expt 0.85 n) (math/expt 1.15 n)) 2)))

(defn make-weights
  [n]
  (->> (range 1 (inc n))
    (map #(get-weight-adjustment %))))


(defn random-groups
  [odd-number-of-pupils]
  (let [pupils (shuffle (range 1 (inc odd-number-of-pupils)))]
    (->> (concat
          (list (take 3 pupils))
          (partition 2 (drop 3 pupils)))
;;      (map set)
      )))

(defn generate-data
  [num-days odd-number-of-pupils]
  (->> (make-weights num-days)
    (reverse)
    (map #(concat (list %) (random-groups odd-number-of-pupils)))
    ))

(defn make-map
  [data]
  (reduce #(assoc % (first %2) (rest %2)) {} data))

;;(make-map (generate-data 10 15))
;; (map #(do (count (c/combinations (range 15) 3))
;;                                                 %))

(defn ncr
  [n r] (c/count-combinations (range n) r))

(defn get-subtract-to-even-minuend
  [n]
  (if (odd? n)
    3
    2))

(defn subtract-to-even
  [n]
  (if (odd? n)
    (- n 3)
    (- n 2)))

;; (defn positive-numbers 
;; 	([] (positive-numbers 1))
;; 	([n] (cons n (lazy-seq (positive-numbers (inc n))))))

(defn subtract-to-zero
  [n]
  (cond
    (< n 0) '()
    (= n 0) '(0)
    :else (cons n (subtract-to-zero (subtract-to-even n)))))

(defn get-subtract-to-zero-minuends
  ([n] (cond
         (< n 0) '()
         (= n 0) '(0)
         :else (get-subtract-to-zero-minuends n '())))
  ([n acc] (cond
             (< n 1) (reverse acc)
             :else (let [minuend (get-subtract-to-even-minuend n)]
                     (get-subtract-to-zero-minuends (- n  minuend) (cons minuend acc))))))

(defn update-vals [map vals f]
  (reduce #(update-in % [%2] f) map vals))

;; (defn fib [n] 
;;   (take n 
;;     (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))

;; (defn make-weights
;;   [n]
;;   (rest (rest (fib (inc (inc n))))))

;; (defn make-weights
;;   [n]
;;   )


(defn make-empty-map-for-student
  ([student-no num-students] (make-empty-map-for-student student-no num-students {:triples 0}))
  ([student-no num-students acc]
   (cond (< num-students 1) acc
         (= student-no num-students) (make-empty-map-for-student student-no (dec num-students) acc)
         :else (make-empty-map-for-student student-no (dec num-students) (assoc acc num-students 0))
         )))

(defn make-empty-maps-for-all-students
  [num-students]
  (->> (range 1 (inc num-students))
    (reduce #(assoc % %2 (make-empty-map-for-student %2 num-students)) {})))

;;FIXME: Uncomment this later
(def dummy-weights (make-empty-maps-for-all-students 15))

(defn increase-value-in-row
  [row key]
  (assoc row key (inc (get row key))))

(defn increase-with-adjustement
  [old-val adjustment]
  (* adjustment (inc old-val)))

(defn analyze-pair
  [pair weights adjustment]
  (let [one (first pair)
        two (second pair)
        old-val (get-in weights [one two])
        ]
    (try
      (assoc-in (assoc-in weights [one two] (increase-with-adjustement old-val adjustment))
                [two one] (increase-with-adjustement old-val adjustment))
      (catch Exception e (str "pair: " pair " adjustment: " adjustment)))
    ))

(defn increase-triplet-count
  [triplet weights adjustment]
  (reduce #(assoc-in % [%2 :triples] (increase-with-adjustement (get-in % [%2 :triples]) adjustment)) weights triplet))

(defn analyze-triplet
  [triplet ws adjustment]
  (let [weights (increase-triplet-count triplet ws adjustment)
        p1 (list (first triplet) (second triplet))
        p2 (list (first triplet) (nth triplet 2))
        p3 (rest triplet)]
    (reduce #(analyze-pair %2 % adjustment) weights (list p1 p2 p3)
      )
    ))

;; (defn analyze-group
;;   [group weights]
;;   (if (= (count group 3))))
(defn analyze-day
  "Expects format along the lines of (0.84 (5 13 11) (1 9) (10 15) (14 8) (12 6) (3 2) (4 7))"
  ([day-with-adjustment weights]
   (let [adjustment (first day-with-adjustment)
         day (rest day-with-adjustment)]
     (analyze-day adjustment day weights)))
  ([adjustment day weights]
   (cond (empty? day) weights
         (= (count (first day)) 3) (analyze-day adjustment (rest day) (analyze-triplet (first day) weights adjustment))
         :else (analyze-day adjustment (rest day) (analyze-pair (first day) weights adjustment)))))

;; (generate-data 10 15)
;; ((0 (5 13 11) (1 9) (10 15) (14 8) (12 6) (3 2) (4 7))
;;  (1 (8 15 13) (1 6) (2 3) (5 14) (10 11) (4 7) (12 9))
;;  (2 (15 9 13) (14 1) (11 10) (6 12) (8 5) (4 2) (7 3))
;;  (3 (9 10 12) (4 3) (1 11) (2 13) (7 6) (8 15) (5 14))
;;  (4 (12 10 4) (5 11) (13 2) (1 14) (7 3) (15 6) (8 9))
;;  (5 (8 13 7) (1 6) (5 14) (2 4) (11 15) (9 10) (3 12))
;;  (6 (1 14 9) (12 15) (13 6) (4 5) (11 7) (10 2) (3 8))
;;  (7 (5 3 1) (4 14) (11 8) (2 12) (15 10) (6 7) (9 13))
;;  (8 (15 4 3) (7 6) (5 11) (12 10) (8 1) (13 2) (9 14))
;;  (9 (5 14 2) (3 12) (9 11) (7 10) (6 15) (4 1) (13 8)))
