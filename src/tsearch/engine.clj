(ns tsearch.engine
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.buffer :as buffer])
  (:require [tsearch.index :as index])
  (:require [tsearch.query :as query]))

(def files (ref '()))
(def query-indices (agent '()))
(def qfutures (agent '()))

(defn- next-file []
  (let[file (first (ensure files))]
    (alter files rest)
    file))

(defn process-remaining-indices [index-buffer]
  (while (> (buffer/size index-buffer) 0)
    (let [index (buffer/try-enqueue index-buffer)]
      (if (> (:nfiles index) 0)
        (do
          (dorun (:index index))
          (send query-indices conj index))))))

(defn waiter [threads index-buffer]
  (doseq [thread threads]
    (.join thread))
  (process-remaining-indices index-buffer)
  (send query-indices (fn [_] '())))

(defn process-file [file max-files index-buffer]
  (let [file-path (.getCanonicalPath file)]
    (def content (slurp file))
    (def occurrences (lexer/process-content content))
    (def index (buffer/enqueue index-buffer))
    (def new-index (index/insert [file-path occurrences] index))
    (if (= (:nfiles new-index) max-files)
      (do
        (buffer/put index-buffer (index/empty-index))
        (dorun (:index new-index))
        (send query-indices conj new-index))
      (buffer/put index-buffer new-index))))

(defn process-file-job [id max-files index-buffer]
  (while (> (count @files) 0)
    (let [file (dosync (next-file))]
      (if file
        (let [file-path (.getCanonicalPath file)]
          (process-file file max-files index-buffer)
          (println "Thread" id "-> " file-path))))))

(defn process-files [nindices max-files nworkers fs]
  (let [index-buffer (buffer/newb (repeat nindices (index/empty-index)))]
    (dosync (ref-set files fs))

    (def threads (for [i (range nworkers)]
      (Thread. #(process-file-job (+ i 1) max-files index-buffer))))
    (doseq [thread threads]
      (.start thread))

    (.start (Thread. #(waiter threads index-buffer)))))

(defn- print-result [query results]
  (println "RESULT for" (str "\"" query "\":"))
  (doseq [pair results]
    (println "File:" (first pair) "    Occurrences:" (nth pair 1))))

(defn update-result [query-obj r]
  (let [results (:result query-obj)
        temp (dissoc query-obj :result)
        new-result (concat results r)
        return (assoc temp :result new-result)]
    return))

(defn search[query-obj index]
  (let [query (:query @query-obj)
        r (query/perform query index)]
    (dorun r)
    (dosync
      (ensure query-obj)
      (alter query-obj update-result r))))

(defn index-ready [qagents _ _ _ index]
  (if (empty? index)
    (do
      (doseq [f @qfutures]
        (deref f)) ; wait finish the searches
      (shutdown-agents)
      (doseq [q qagents]
        (print-result (:query @q) (:result @q))))
    (do
      (doseq [q qagents]
        (send qfutures conj (future (search q (first index))))))))

(defn process-search [raw-queries]
  (def query-agents (for [raw-query raw-queries]
    (let [query (query/parseq raw-query)
          qagent (ref {:raw-query raw-query :query query :result '()})]
      qagent)))

  (add-watch query-indices :index-ready #(index-ready query-agents %1 %2 %3 %4)))
