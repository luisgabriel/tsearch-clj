(ns tsearch.buffer
  (:import (java.util.concurrent LinkedBlockingQueue)))

(defn new-empty []
  (LinkedBlockingQueue.))

(defn newb [elements]
  (let [queue (new-empty)]
    (doseq [element elements]
      (.put queue element))
    queue))

(defn is-empty [b]
  (= (.size b) 0))

(defn size [b]
  (.size b))

(defn put [buffer value]
  (.put buffer value))

(defn enqueue [buffer]
  (.take buffer))

(defn try-enqueue [buffer]
  (.poll buffer))
