(ns think.sm.util
  (:require [clojure.string :as str]))


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


(defn space-delimited-string-to-keyword-array [data]
  (if data
    (mapv keyword (str/split data #" "))
    []))

(defn space-delimited-string-to-array [data]
  (if data
    (vec (str/split data #" "))
    []))


(def attr-types [:string :keyword :string-list :keyword-list] )

(defn parse-attr [value type]
  (case type
    :string value
    :keyword (keyword value)
    :string-list (space-delimited-string-to-array value)
    :keyword-list (space-delimited-string-to-keyword-array value)
    value ))

(defn parse-attributes [xml-node retval attr-map]
  (let [attrs (:attrs xml-node)
        map-keys (keys attr-map)
        attr-key-value-pairs (filter identity 
                                     (mapcat (fn [key] 
                                            (if (key attrs)
                                              (let [map-entry (key attr-map)
                                                    [item-type item-key] (if (vector? map-entry)
                                                                           map-entry
                                                                           [map-entry key])]
                                                [item-key (parse-attr (key attrs) item-type)])
                                              nil))
                                          map-keys))]
    (apply assoc retval attr-key-value-pairs)))

(defn parse-time-val [^String time-str]
  "positive double number immediately followed by either
s or ms.  Returns integer milliseconds"
  (try
    (let [suffix (if (.endsWith time-str "ms")
                   "ms"
                   "s")
          time-str (.substring time-str 0 (- (.length time-str) (.length suffix)))
          value (Double/parseDouble time-str)
          value (if (= "s" suffix) (* value 1000) value)]
      (int value))
    (catch Exception e (println (str e)) 0)))
      

(defn generate-unique-id[stem id-seed previous-id-set]
  (loop [id-seed id-seed]
    (let [new-id (keyword (str stem id-seed))]
      (if (new-id previous-id-set)
        (recur (inc id-seed))
        [new-id id-seed (conj previous-id-set new-id)]))))
          

(defn get-content-child-data [node]
  (if (:expr (:attrs node))
    (:expr (:attrs node))
    (apply str (:content node))))


(defn parse-send-param [node]
  { :type :param :name (keyword (:name (:attrs node))) :expr (:expr (:attrs node))
   :location (keyword (:location (:attrs node))) })

(defn parse-content-or-param-children[node]
  (let [children (map parse-send-param (filter (fn [node] (= :param (:tag node))) (:content node)))
        content (first (map get-content-child-data (filter (fn [node] (= :content (:tag node))) (:content node))))]
    [children content]))
