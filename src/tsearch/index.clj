(ns tsearch.index)

(defn insert [pair index]
  (let [file-path (nth pair 0)
        occurrences (seq (nth pair 1))]
    (loop [ocs occurrences new-index index]
      (if (empty? ocs)
        new-index
        (let [oc-pair (first ocs)
              word (key oc-pair)
              positions (val oc-pair)]
          (recur (rest ocs) (merge-with concat new-index (hash-map word (list [file-path positions])))))))))

(defn build-index [occurrences]
  (loop [ocs occurrences index (hash-map)]
    (if (empty? ocs)
      index
      (recur (rest ocs) (insert (first ocs) index)))))

