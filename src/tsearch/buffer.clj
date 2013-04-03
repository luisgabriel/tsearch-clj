(ns tsearch.buffer
  (:require [clojure.java.io :as cjio]))

(def new-empty
  (let [queue clojure.lang.PersistentQueue/EMPTY]
    (ref {:queue queue :finished false})))

(defn newb [elements finished]
  (let [equeue clojure.lang.PersistentQueue/EMPTY
        queue (reduce conj equeue elements)]
    (ref {:queue queue :finished finished})))

(defn finish [buffer-ref]
  (let [buffer (ensure buffer-ref)
        queue (:queue buffer)
        new-buffer {:finished true :queue queue}]
    (ref-set buffer-ref new-buffer)))

(defn is-empty [buffer-ref]
  (let [buffer (ensure buffer-ref)]
    (empty? (:queue buffer))))

(defn write [buffer-ref value]
  (let [buffer (ensure buffer-ref)
        queue (:queue buffer)
        finished (:finished buffer)
        new-buffer {:finished finished :queue (conj queue value)}]
    (ref-set buffer-ref new-buffer)))

(defn read-from-ref [buffer-ref]
  (let [buffer (ensure buffer-ref)
        finished (:finished buffer)
        queue (:queue buffer)
        value (first queue)
        size (count queue)
        new-buffer {:finished finished :queue (pop queue)}]
    (if (and finished (= size 0))
      :nothing
      (if (= size 0)
        :block
        (do
          (ref-set buffer-ref new-buffer)
          value)))))

(defn readb [buffer-ref]
  (loop []
    (let [result (read-from-ref buffer-ref)]
      (if (= result :nothing)
        :nothing
        (if (= result :block)
          (do
            (Thread/yield)
            (recur))
          result)))))
