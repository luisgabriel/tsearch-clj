(ns tsearch.query
  (:require [tsearch.lexer :as lexer]))

(defn parseq [content]
  (lexer/tokenize content))

(defn insert [hmap pair]
  (let [word (nth pair 0)
        occur-list (nth pair 1)
        insert-f (fn [hmap2 pair2]
                   (let [path (nth pair2 0)
                         positions (nth pair2 1)]
                     (merge-with concat hmap2 (hash-map path (list [word positions])))))]
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
  (let [all-occurrences (map (fn [w] [w (val (find index w))]) words)
        queryMap (reduce insert (hash-map) all-occurrences)
        wordCounter (count words)
        filteredMap (select-keys queryMap (for [[k v] queryMap :when (= (count v) wordCounter)] k))]
    (loop [pairs (seq filteredMap) acc (list)]
      (if (empty? pairs)
        acc
        (let [file-path (key (first pairs))
              words-occurs (val (first pairs))
              matches (count-occurrences words-occurs)]
          (if (> matches 0)
            (recur (rest pairs) (conj acc [file-path matches]))
            (recur (rest pairs) acc)))))))
