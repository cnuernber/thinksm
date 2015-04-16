(ns think.sm.executable
  (:require [think.sm.datamodel :as dm]))

(defmulti parse-executable-content :tag)

(defmethod parse-executable-content :log [node]
  { :type :log :label (:label (:attrs node)) :expr (:expr (:attrs node)) })

(defmethod parse-executable-content :raise [node]
  { :type :raise :event (:event (:attrs node)) })

(defmethod parse-executable-content :assign [node]
  { :type :assign 
   :location (keyword (:location (:attrs node)))
   :expr (:expr (:attrs node)) })

(defmethod parse-executable-content :if [node]
  (let [stmt { :type :if :cond (:cond (:attrs node)) }
        children (reduce (fn [children node-child]
                           (conj children (parse-executable-content node-child)))
                         []
                         (:content node))]
    (assoc stmt :children children)))
    

(defmethod parse-executable-content :elseif [node]
  { :type :elseif :cond (:cond (:attrs node)) })

(defmethod parse-executable-content :else [node]
  { :type :else })

(defmethod parse-executable-content :default [node]
    (throw (Throwable. (str "Unrecognized executable content " (:tag node)))))

(defmulti execute-specific-content :type)


(defmethod execute-specific-content :log [item context]
  (println (dm/execute-expression context (:expr item)))
  context)


(defmethod execute-specific-content :raise [item context]
  (let [old-events (:events context)
        new-events (conj old-events (:event item))]
    (assoc context :events new-events)))

(defmethod execute-specific-content :if [item context]
  (let [content (:children item)]
    (loop [context context
           child-item (first content)
           content (rest content)
           condition (dm/execute-expression context (:cond item))]
      (let [item-type (:type child-item)
            is-elif-else (or (= item-type :elseif)
                             (= item-type :else))]
        (if (or is-elif-else (nil? child-item))
          (if (or condition (nil? child-item))
            context
            (let [condition (if (= :elseif item-type) 
                              (dm/execute-expression context (:cond child-item))
                              true)]
              (recur context (first content) (rest content) condition)))
          (let [context (if condition (execute-specific-content child-item context) context)]
            (recur context (first content) (rest content) condition)))))))

(defmethod execute-specific-content :assign [item context]
  (let [datamodel (:datamodel context)
        location (:location item)
        value (dm/execute-expression context (:expr item))
        datamodel (assoc datamodel location value)]
    (assoc context :datamodel datamodel)))
        
      
