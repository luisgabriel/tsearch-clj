(ns tsearch.engine
  (:require [clojure.set :as cjset])
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.buffer :as buffer])
  (:require [tsearch.index :as index])
  (:require [tsearch.query :as query])
  (:require [tsearch.logger :as logger]))

(def files (ref '()))
(def query-indices (agent '()))
(def query-threads (agent '()))

(def word-counter (ref 0))
(def file-counter (ref 0))
(def size-counter (ref 0))
(def all-words (ref (hash-set)))

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
          (send query-indices conj index)
          (logger/index-completed (:id index) (:nfiles index)))))))

(defn waiter [threads index-buffer]
  (doseq [thread threads]
    (.join thread))
  (process-remaining-indices index-buffer)
  (send query-indices (fn [_] '())))

(defn process-file [file max-files index-buffer]
  (let [file-path (.getCanonicalPath file)]
    (def content (slurp file))
    (def w-occurs-pair (lexer/process-content content))
    (def occurrences (nth w-occurs-pair 1))
    (def index (buffer/enqueue index-buffer))
    (def new-index (index/insert [file-path occurrences] index))
    (if (= (:nfiles new-index) max-files)
      (do
        (buffer/put index-buffer (index/empty-index))
        (dorun (:index new-index))
        (send query-indices conj new-index)
        (logger/index-completed (:id new-index) (:nfiles new-index)))
      (buffer/put index-buffer new-index))
    (let [words (dosync (alter word-counter #(+ % (first w-occurs-pair))))
          word-set (dosync (alter all-words cjset/union (into #{} (keys occurrences))))]
      [words (count word-set)])))

(defn process-file-job [id max-files index-buffer]
  (while (> (count @files) 0)
    (let [file (dosync (next-file))]
      (if file
        (let [file-path (.getCanonicalPath file)
              r (process-file file max-files index-buffer)
              words (first r)
              indexed-words (nth r 1)
              files (dosync (alter file-counter inc))
              size (dosync (alter size-counter #(+ % (.length file))))]
          (logger/file-processed id file-path words indexed-words files size))))))

(defn process-files [nindices max-files nworkers fs]
  (let [index-buffer (buffer/newb (repeatedly nindices index/empty-index))]
    (dosync (ref-set files fs))

    (def threads (for [i (range nworkers)]
      (Thread. #(process-file-job (+ i 1) max-files index-buffer))))
    (doseq [thread threads]
      (.start thread))

    (.start (Thread. #(waiter threads index-buffer)))))

(defn update-result [query-obj r]
  (let [results (:result query-obj)
        temp (dissoc query-obj :result)
        new-time (+ (first results) (first r))
        new-result (concat (nth results 1) (nth r 1))
        return (assoc temp :result [new-time new-result])]
    return))

(defn search[query-obj index]
  (let [query (:query @query-obj)]
    (def start (System/currentTimeMillis))
    (def r (doall (query/perform query index)))
    (def end (System/currentTimeMillis))
    (def diff (- end start))
    (logger/query-performed query (:id index))
    (dosync
      ;(ensure query-obj)
      (alter query-obj update-result [diff r]))))

(defn index-ready [qrefs _ _ _ index]
  (if (empty? index)
    (do
      (doseq [t @query-threads]
        (.join t)) ; wait finish the searches
      (doseq [q qrefs]
        (def r (nth (:result @q) 1))
        (def diff (first (:result @q)))
        (def ordered-result (sort-by (fn [e] [(/ 1.0 (nth e 1)) (first e)]) r))
        (logger/search-performed (:query @q) [diff ordered-result]))
      (logger/finish))
    (do
      (doseq [q qrefs]
        (def t (Thread. #(search q (first index))))
        (.start t)
        (send query-threads conj t)))))

(defn process-search [raw-queries]
  (def query-refs (for [raw-query raw-queries]
    (let [query (query/parseq raw-query)
          qref (ref {:raw-query raw-query :query query :result [0 []]})]
      qref)))

  (add-watch query-indices :index-ready #(index-ready query-refs %1 %2 %3 %4)))
