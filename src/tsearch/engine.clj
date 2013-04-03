(ns tsearch.engine
  (:require [tsearch.lexer :as lexer])
  (:require [tsearch.buffer :as buffer])
  (:require [tsearch.index :as index]))

(defn process-remaining-indices [index-buffer qi-buffer]
  (loop []
    (let [index (dosync (buffer/readb index-buffer))]
      (if (not= index :nothing)
        (let [finished (dosync (buffer/is-empty index-buffer))]
          (dosync
            (if finished (buffer/finish qi-buffer))
            (buffer/write qi-buffer index))
          (recur))))))

(defn waiter [threads index-buffer qi-buffer]
  (doseq [thread threads]
    (.join thread))
  (dosync (buffer/finish  index-buffer))
  (process-remaining-indices index-buffer qi-buffer))

(defn process-file [file max-files index-buffer qi-buffer]
  (let [content (slurp file)
        occurrences (lexer/process-content content)
        index (dosync (buffer/readb index-buffer))
        file-path (.getCanonicalPath file)
        new-index (index/insert [file-path occurrences] index)]
    (if (> (:nfiles new-index) max-files)
      (do
        (dosync (buffer/write index-buffer index/empty-index))
        (dosync (buffer/write qi-buffer new-index)))
      (dosync (buffer/write index-buffer new-index)))))

(defn process-file-job [id file-buffer max-files index-buffer qi-buffer]
  (loop []
    (let [file (dosync (buffer/readb file-buffer))]
      (if (not= file :nothing)
        (let [file-path (.getCanonicalPath file)]
          (process-file file max-files index-buffer qi-buffer)
          (println "Thread" id "-> " file-path)
          (recur))))))

(defn process-files [nindices max-files nworkers file-buffer]
  (let [index-buffer (buffer/newb (repeat nindices index/empty-index) false)
        qi-buffer buffer/new-empty]
    (def threads (for [i (range nworkers)]
      (Thread. #(process-file-job (+ i 1) file-buffer max-files index-buffer qi-buffer))))
    (doseq [thread threads]
      (.start thread))
    (.start (Thread. #(waiter threads index-buffer qi-buffer)))
    qi-buffer))
