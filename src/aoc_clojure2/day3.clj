(ns aoc-clojure2.day3
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(defn char-range
  "Generates char range"
  [start end]
  (map char (range (int start) (inc (int end)))))

(def item-priorities
  "Generates and returns element priorities"
  (merge (zipmap (char-range \a \z) (range 1 27)) (zipmap (char-range \A \Z) (range 27 53))))

(defn item-priority
  [item]
  (get item-priorities item))


(defn get-item-prio
  [item]
  (get item-priorities item))

;;(get-item-prio \B)

(defn rucksack-find-duplicates
  "Finds duplicates present in both rucksack compartments"
  [rucksack]
  (let [[left right] (split-at (/ (count rucksack) 2) rucksack)]
    (set/intersection (into #{} left) (into #{} right))))

(defn rucksack-duplicates-total-score
  "Calculates sum of all duplicates priorities"
  [rucksack]
  (let [dupes (rucksack-find-duplicates rucksack)]
    (apply + (map #(get item-priorities %1) dupes))))

(defn solve-part-1
  "Given lines of an input, solve first part of day3 AOC challenge "
  [lines]
  (apply + (map rucksack-duplicates-total-score lines)))

;;;; part 1
(defn group-badge
  "Gets group badge from single group."
  [group]
  (apply set/intersection (map (comp set char-array) group)))

(defn solve-part-2
  "Given lines of an input, solve second part of day3 AOC challenge"
  [lines]
  (let [groups (partition 3 lines)]
    (apply + (map get-item-prio (flatten (map (comp seq group-badge) groups))))))

(solve-part-2 ["vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"
"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" "ttgJtRGJQctTZtZT" "CrZsJsPPZsGzwwsLwLmpwMDw"])

;; TODO: Move this to load-tests.clj or sth like that
(defn solve-from-file
  "Given filename, call function f with read lines from file"
  [filename f]
  (with-open [reader (io/reader filename)]
      (let [input (reduce conj [] (line-seq reader))]
        (f input))))



(apply set/intersection (map set (map char-array ["hello" "world"])))
(solve-from-file "./resources/day3.txt" solve-part-1)
(solve-from-file "./resources/day3.txt" solve-part-2)
