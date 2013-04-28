(ns tsearch.core
  (:require [tsearch.scanner :as scanner])
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.index :as index])
  (:require [tsearch.query :as query])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn print-result [result]
  (def ordered-result (sort-by (fn [e] [(/ 1.0 (nth e 1)) (first e)]) result))
  (doseq [pair ordered-result]
    (println "File: " (first pair) "    Occurrences: " (nth pair 1))))

(defn occurrences [^java.io.File file]
  (let [content (slurp file)
        occurrences (lexer/process-content content)]
    [(.getCanonicalPath file) occurrences]))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (def files (scanner/all-files (first args)))

  (def global-index
    (loop [index (index/empty-index) fs files]
      (if (empty? fs)
        index
        (let [ocs (occurrences (first fs))]
          (recur (index/insert ocs index) (rest fs))))))

  (doseq [raw-query (rest args)]
    (let [query (query/parseq raw-query)
          result (query/perform query global-index)]
      (println (str "RESULT for \"" raw-query "\":"))
      (print-result result)
      (println "")))
)
