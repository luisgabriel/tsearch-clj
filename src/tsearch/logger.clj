(ns tsearch.logger)

(def separator "-----\n")
(def log (agent '()))

(defn- print-msg [_ _ _ messages]
  (if (empty? messages)
    (shutdown-agents)
    (println (first messages))))

(add-watch log :print print-msg)

(defn finish []
  (send-off log (fn [_] '())))

(defn message [msg]
  (send-off log conj msg))

(defn file-processed [task-id file-path]
  (let [header (str separator "[Thread " task-id "]\n")
        file (str header "New file processed: " file-path)]
    (send-off log conj file)))

(defn index-completed [id nfiles]
  (def msg (str separator "Sub-index " id " completed. (" nfiles " files)"))
  (send-off log conj msg))

(defn query-performed [query index-id]
  (def msg (str separator "Query \"" query "\" performed on sub-index " index-id))
  (send-off log conj msg))

(defn search-performed[query results]
  (let [header (str "RESULT for " (str "\"" query "\":\n"))]
    (loop [msg (str separator header) rs results]
      (if (empty? rs)
        (send-off log conj msg)
        (let[pair (first rs)
             file (first pair)
             occurrences (nth pair 1)
             line (str "File: " file "    Occurrences: " occurrences "\n")]
          (recur (str msg line) (rest rs)))))))
