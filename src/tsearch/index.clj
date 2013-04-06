(ns tsearch.index)

(def id-counter (ref 0))
(def zero (Character/getNumericValue 0))

(defn- dord [ch]
  (- (Character/getNumericValue ch) zero))

(defn empty-index []
  (let[index (vec (repeat (+ (dord \z) 1) (hash-map)))]
    { :index index :nfiles 0 :id (dosync (alter id-counter inc)) }))

(defn insert [oc-pair index-obj]
  (let [filep (first oc-pair)
        ocs (vec (nth oc-pair 1))
        index (:index index-obj)
        file-counter (:nfiles index-obj)
        id (:id index-obj)]
    (loop [i 0 ind (transient index)]
      (if (= i (count ocs))
        {:index (persistent! ind) :nfiles (+ file-counter 1) :id id}

        (let [oc-pair (nth ocs i)
              word (key oc-pair)
              positions (val oc-pair)
              voc-pos (dord (first word))
              voc (nth ind voc-pos)
              to-append [filep positions]
              old-val (get voc word)
              new-val (if (nil? old-val) [to-append] (conj old-val to-append))
              new-voc (assoc voc word new-val)
              new-index (assoc! ind voc-pos new-voc)]
          (recur (+ i 1) new-index))))))

(defn find-oc [word index-obj]
  (let [index (:index index-obj)
        voc (nth index (dord (first word)))
        result (find voc word)]
    (if result (val result) '())))
