(ns tsearch.index)

(def empty-index {:index (hash-map) :nfiles 0})

(defn insert [oc-pair index-obj]
  (let [file-path (first oc-pair)
        occurrences (seq (nth oc-pair 1))
        index (:index index-obj)
        file-counter (:nfiles index-obj)]
    (loop [ocs occurrences new-index index]
      (if (empty? ocs)
        {:index new-index :nfiles (+ file-counter 1)}

        (let [oc-pair (first ocs)
              word (key oc-pair)
              positions (val oc-pair)]
          (recur (rest ocs) (merge-with concat new-index (hash-map word (list [file-path positions])))))))))
