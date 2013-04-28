(ns tsearch.lexer
  (:require [clojure.string :as cjstr]))

(defn char-val [c]
  (Character/getNumericValue (char c)))

(defn is-ascii-alpha-num [c]
  (let [n (Character/getNumericValue (char c))]
    (or (and (>= n (char-val \a)) (<= n (char-val \z)))
        (and (>= n (char-val \A)) (<= n (char-val \Z)))
        (and (>= n (char-val \0)) (<= n (char-val \9))))))

(defn is-valid [c]
    (or (is-ascii-alpha-num c)
        (Character/isSpaceChar (char c))
        (.equals (str \newline) (str c))))

(defn lower-and-replace [c]
  (if (.equals (str \newline) (str c))
    \space
    (Character/toLowerCase (char c))))

(defn tokenize [content]
  (let [filtered (filter is-valid content)
        lowered (map lower-and-replace filtered)]
    (cjstr/split (apply str lowered) #"\s+")))

(defn process-content [content]
  (let [words (tokenize content)]
    (loop [ws words i 0 hmap (hash-map)]
      (if (empty? ws)
        hmap
        (recur (rest ws) (inc i) (update-in hmap [(first ws)] #(conj % i)))))))
