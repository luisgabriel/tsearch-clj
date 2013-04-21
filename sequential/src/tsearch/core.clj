(ns tsearch.core
  (:require [tsearch.scanner :as scanner])
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.index :as index])
  (:require [tsearch.query :as query])
  (:gen-class))

(defn occurrences [file]
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

  (println (str "Path: " (first args)))
  (println (pr-str (query/perform (query/parseq (nth args 1)) global-index)))
)
