(ns think.sm.datamodel
  (:require [think.sm.core :as core] ))

                    
(defn scan-node-for-datamodel-code-walker[node context]
  "If the node contains cond or expr then we have code
we need to save into an array and place an integer in it's place"
  (let [[var-vec code-vec] context
        var-vec (if (= :data (:type node))
                  (conj var-vec (:id node))
                  var-vec)]
      
    (let [data-to-replace (first (filter identity (map (fn [keyword]
                                                         (if (keyword node)
                                                           [keyword (node keyword)]
                                                           nil))
                                                       [:expr :cond])))
          idx (count code-vec)]
      (if data-to-replace
        (let [[keyword code] data-to-replace
              code-vec (conj code-vec data-to-replace)]
          [(assoc node keyword idx) [var-vec code-vec]])
        [node [var-vec code-vec]]))))


(defn scan-node-for-datamodel-code [node]
  "returns a new tree with code replaced by integers
and a context with a variable declaration vec and a code vec
that corresponds to the integers in the new machine"
  (core/walk-item node [[] []] scan-node-for-datamodel-code-walker))

(defn output-dm-prefix[var-vec]
  (println 
"(fn [context]
  (let [datamodel (:datamodel context)")
  (doseq [var-name var-vec]
    (println (str
"      " (name var-name) " (" var-name " datamodel)")))
  (println "      ]"))

(defn output-dm-postfix[]
  (println "))"))


(defn dm-code-to-string [dm-context]
  (with-out-str
    (let [[var-vec code-vec] dm-context]
      (println "[")
      (doseq [code-item code-vec]
        (output-dm-prefix var-vec)
        (println (code-item 1))
        (output-dm-postfix))
      (println "]" ))))
  
