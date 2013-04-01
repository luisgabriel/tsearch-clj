(ns tsearch.core
  (:require [clojure.java.io :as cjio])
  (:require [tsearch.scanner :as scanner])
  (:require [tsearch.lexer :as lexer])
  (:gen-class))

(defn occurrences-of [file initial-i]
  (with-open [rdr (cjio/reader file)]
  (loop [lines (line-seq rdr) i initial-i hmap (hash-map)]
    (if (empty? lines)
      [hmap i]
      (let [processed (lexer/process-content (first lines) i hmap)
            index (nth processed 0)
            new-i (nth processed 1)]
      (recur (rest lines) new-i index))))))

(defn all-occurrences [files]
  (loop [fs files i 0 acc (list)]
    (if (empty? fs)
      acc
      (let [file (first fs)
            processed (occurrences-of file i)
            occurrences (nth processed 0)
            new-i (nth processed 1)]
        (recur (rest fs) new-i (conj acc [(.getCanonicalPath file) occurrences]))))))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (def files (scanner/all-files (first args)))
  (def occurrences (all-occurrences files))

  (println (str "Path: " (first args)))
  (doseq [oc occurrences]
    (print (str "-> " (first oc) ": "))
    (println (count (nth oc 1))))
)
