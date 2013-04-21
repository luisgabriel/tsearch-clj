(ns tsearch.index)

(def zero (Character/getNumericValue \0))

(defn- dord [ch]
  (- (Character/getNumericValue ch) zero))

(defn empty-index []
  (vec (repeat (+ (dord \z) 1) (hash-map))))

(defn insert [oc-pair index]
  (let [filep (first oc-pair)
        occurrences (seq (nth oc-pair 1))]
    (loop [ocs occurrences ind index]
      (if (empty? ocs)
        ind
        (let [oc-pair (first ocs)
              word (key oc-pair)
              positions (val oc-pair)
              voc-pos (dord (first word))
              voc (nth ind voc-pos)
              new-voc (update-in voc [word] #(conj % [filep positions]))
              new-index (assoc ind voc-pos new-voc)]
          (recur (rest ocs) new-index))))))

(defn find-oc [word index]
  (let [voc (nth index (dord (first word)))
        result (find voc word)]
    (if result (val result) '())))
