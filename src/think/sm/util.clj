(ns think.sm.util)


(defn walkable-item? [item]
  (or (vector? item)
      (seq? item)
      (:type item)))

(defn walk-item [item-or-seq context walker]
  "walk the machine expecting walker to produce both new machine nodes
and new context data.  
Walker returns a vector containing first the new machine node and 
second the new context"
  (if (or (vector? item-or-seq)
          (seq? item-or-seq))
    (reduce (fn [[item-vec context] item]
              (let [[new-item context] (if (walkable-item? item)
                                         (walk-item item context walker)
                                         [item context])]
                [(conj item-vec new-item) context]))
            [[] context]
            item-or-seq)
    (let [[item context] (walker item-or-seq context)]
      (reduce (fn [[item context] key]
                (let [entry (key item)]
                  (if (walkable-item? entry)
                    (let [[new-entry context] (walk-item entry context walker)]
                      [(assoc item key new-entry) context])
                    [item context])))
              [item context]
              (keys item)))))
