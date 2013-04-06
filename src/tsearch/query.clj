(ns tsearch.query
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.index :as index]))

(defn parseq [content]
  (lexer/tokenize content))

(defn- insert-f [word hmap pair]
  (let [path (first pair)
        positions (nth pair 1)]
    (update-in hmap [path] #(conj % [word positions]))))

(defn- insert [hmap pair]
  (let [word (first pair)
        occur-list (nth pair 1)]
    (reduce #(insert-f word %1 %2) hmap occur-list)))

(defn- filter-successors [l1 l2]
  (filter (fn [x] (some #(== % (+ x 1)) l1)) l2))

(defn- count-o [lists]
  (loop [ls (vec lists)]
    (if (empty? ls)
      0
      (let [l1 (first ls)
            l2 (first (rest ls))]
        (if (empty? (rest ls))
          (count l1)
          (recur (conj (rest (rest ls)) (filter-successors l1 l2))))))))

(defn- count-occurrences [words-occurs]
  (count-o (map #(nth % 1) words-occurs)))

(defn perform [words index]
  (let [all-occurrences (map (fn [w] [w (index/find-oc w index)]) words)
        query-map (reduce insert (hash-map) all-occurrences)
        word-counter (count words)]
    (loop [pairs (vec query-map) acc []]
      (if (empty? pairs)
        acc
        (let [words-occurs (val (first pairs))]
          (if (= (count words-occurs) word-counter)
            (let [file-path (key (first pairs))
                  matches (count-occurrences words-occurs)]
              (if (> matches 0)
                (recur (rest pairs) (conj acc [file-path matches]))
                (recur (rest pairs) acc)))
            (recur (rest pairs) acc)))))))
