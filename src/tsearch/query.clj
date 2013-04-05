(ns tsearch.query
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.index :as index]))

(defn parseq [content]
  (lexer/tokenize content))

(defn insert [hmap pair]
  (let [word (nth pair 0)
        occur-list (nth pair 1)
        insert-f (fn [hmap2 pair2]
                   (let [path (nth pair2 0)
                         positions (nth pair2 1)]
                     (update-in hmap2 [path] #(conj % [word positions]))))]
    (reduce insert-f hmap occur-list)))

(defn filter-successors [l1 l2]
  (filter (fn [x] (some #(== % (- x 1)) l1)) l2))

(defn count-o [lists]
  (loop [ls lists]
    (if (empty? ls)
      0
      (let [l1 (first ls)
            l2 (first (rest ls))]
        (if (empty? (rest ls))
          (count l1)
          (recur (conj (rest (rest ls)) (filter-successors l1 l2))))))))

(defn count-occurrences [words-occurs]
  (count-o (map #(nth % 1) words-occurs)))

(defn perform [words index]
  (let [all-occurrences (map (fn [w] [w (index/find-oc w index)]) words)
        query-map (reduce insert (hash-map) all-occurrences)
        word-counter (count words)]
    (loop [pairs (seq query-map) acc (list)]
      (if (empty? pairs)
        acc
        (let [words-occurs (val (first pairs))]
          (if (= (count words-occurs) word-counter)
            (let [file-path (key (first pairs))
                  matches (count-occurrences (reverse words-occurs))]
              (if (> matches 0)
                (recur (rest pairs) (conj acc [file-path matches]))
                (recur (rest pairs) acc)))
            (recur (rest pairs) acc)))))))
