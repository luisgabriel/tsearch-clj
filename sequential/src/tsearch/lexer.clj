(ns tsearch.lexer
  (:require [clojure.string :as cjstr]))

(def a (Character/getNumericValue \a))
(def z (Character/getNumericValue \z))
(def A (Character/getNumericValue \A))
(def Z (Character/getNumericValue \Z))
(def zero (Character/getNumericValue \0))
(def nine (Character/getNumericValue \9))

(defn is-ascii-alpha-num [c]
  (let [n (Character/getNumericValue c)]
    (or (and (>= n a) (<= n z))
        (and (>= n A) (<= n Z))
        (and (>= n zero) (<= n nine)))))

(defn is-valid [c]
    (or (is-ascii-alpha-num c)
        (Character/isSpaceChar c)
        (.equals (str \newline) (str c))))

(defn lower-and-replace [c]
  (if (.equals (str \newline) (str c)) \space (Character/toLowerCase c)))

(defn tokenize [content]
  (let [filtered (filter is-valid content)
        lowered (map lower-and-replace filtered)]
    (cjstr/split (apply str lowered) #"\s+")))

(defn process-content [content]
  (let [words (tokenize content)]
    (loop [ws words i 0 hmap (hash-map)]
      (if (empty? ws)
        hmap
        (recur (rest ws) (+ i 1) (merge-with concat hmap (hash-map (first ws) (list i))))))))
