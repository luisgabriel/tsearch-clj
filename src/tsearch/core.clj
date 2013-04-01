(ns tsearch.core
  (:require [tsearch.scanner :as scanner])
  (:require [tsearch.lexer :as lexer])
  (:gen-class))

(defn all-occurrences [files]
  (loop [fs files acc (list)]
    (if (empty? fs)
      acc
      (let [file (first fs)
            content (slurp file)
            occurrences (lexer/process-content content)]
        (recur (rest fs) (conj acc [(.getCanonicalPath file) occurrences]))))))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (def files (scanner/all-files (first args)))
  (def occurrences (all-occurrences files))
  (pr-str occurrences)
)
