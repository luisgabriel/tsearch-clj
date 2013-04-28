(ns tsearch.scanner
  (:require [clojure.java.io :as cjio]))

(defn all-files [path]
  (let [entries (file-seq (cjio/file path))]
      (filter (fn [^java.io.File file] (.isFile file)) entries)))
