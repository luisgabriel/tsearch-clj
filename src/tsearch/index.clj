(ns tsearch.index)

(def id-counter (ref 0))
(def zero (Character/getNumericValue (char \0)))

(defn- dord [ch]
  (- (Character/getNumericValue ch) zero))

(defn empty-index []
  (let[index (vec (repeat (+ (dord \z) 1) (hash-map)))]
    (dorun index)
    { :index index :nfiles 0 :id (dosync (alter id-counter inc)) }))

(defn insert [oc-pair index-obj]
  (let [filep (first oc-pair)
        occurrences (seq (nth oc-pair 1))
        index (:index index-obj)
        file-counter (:nfiles index-obj)
        id (:id index-obj)]
    (loop [ocs occurrences ind index]
      (if (empty? ocs)
        {:index ind :nfiles (+ file-counter 1) :id id}

        (let [oc-pair (first ocs)
              word (key oc-pair)
              positions (val oc-pair)
              voc-pos (dord (first word))
              voc (nth ind voc-pos)
              new-voc (update-in voc [word] #(conj % [filep positions]))
              new-index (assoc ind voc-pos new-voc)]
          (recur (rest ocs) new-index))))))

(defn find-oc [word index-obj]
  (let [index (:index index-obj)
        voc (nth index (dord (first word)))
        result (find voc word)]
    (if result (val result) '())))
