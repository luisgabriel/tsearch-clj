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

(defn file-processed [task-id file-path words iwords files size]
  (let [header (str separator "[Thread " task-id "]\n")
        file (str header "New file processed: " file-path "\n")
        kbytes (str file (format "Kbytes processed so far: %.3f\n" (/ size 1024.0)))
        files (str kbytes "Files processed so far: " files "\n")
        w (str files "Words found so far: " words "\n")
        iw (str w "Words in the index: " iwords)]
    (send-off log conj iw)))

(defn index-completed [id nfiles]
  (def msg (str separator "Sub-index " id " completed. (" nfiles " files)"))
  (send-off log conj msg))

(defn query-performed [query index-id]
  (def msg (str separator "Query \"" query "\" performed on sub-index " index-id))
  (send-off log conj msg))

(defn search-performed[query results]
  (let [header (str "RESULT for " (str "\"" query "\":\n\n"))
        files (loop [msg (str separator header) rs (take 50 (nth results 1))]
          (if (empty? rs)
            msg
            (let[pair (first rs)
                 file (first pair)
                 occurrences (nth pair 1)
                 line (str "File: " file "    Occurrences: " occurrences "\n")]
              (recur (str msg line) (rest rs)))))
        matches (str files "\nTotal matching files: " (count (nth results 1)))
        qtime (str matches "\nQuery time: " (first results) " ms")]
     (send-off log conj qtime)))
