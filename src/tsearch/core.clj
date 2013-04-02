(ns tsearch.core
  (:require [tsearch.scanner :as scanner])
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.index :as index])
  (:require [tsearch.query :as query])
  (:gen-class))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (def files (scanner/all-files (first args)))

  (println (str "Path: " (first args)))
)
