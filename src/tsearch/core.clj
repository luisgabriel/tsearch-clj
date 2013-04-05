(ns tsearch.core
  (:require [tsearch.scanner :as scanner])
  (:require [tsearch.buffer :as buffer])
  (:require [tsearch.engine :as engine])
  (:gen-class))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (def nworkers 3)  ; number of indexing threads
  (def nindices 4)  ; initial sub-indices
  (def max-files 3) ; max number of files indexed per sub-index

  (def files (scanner/all-files (first args)))

  (engine/process-files nindices max-files nworkers files)
  (engine/process-search (rest args)))
