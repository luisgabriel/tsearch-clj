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
  (loop [cs content r (transient [])]
    (if (empty? cs)
      (cjstr/split (apply str (persistent! r)) #"\s+")
      (let [ch (first cs)]
        (if (is-valid ch)
          (recur (rest cs) (conj! r (lower-and-replace ch)))
          (recur (rest cs) r))))))

(defn process-content [content]
  (let [words (tokenize content)]
    (loop [ws words i 0 hmap (transient (hash-map))]
      (if (empty? ws)
        [i (persistent! hmap)]
        (let [hkey (first ws)
              old-val (get hmap hkey)
              new-val (if (nil? old-val) [i] (conj old-val i))]
        (recur (rest ws) (+ i 1) (assoc! hmap hkey new-val)))))))
