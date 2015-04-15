(ns think.sm.executable)

(defmulti parse-executable-content :tag)

(defmethod parse-executable-content :log [node]
  { :type :log :label (:label (:attrs node)) :expr (:expr (:attrs node)) })

(defmethod parse-executable-content :raise [node]
  { :type :raise :event (:event (:attrs node)) })

(defmethod parse-executable-content :assign [node]
  { :type :assign 
   :location (:keyword (:location (:attrs node)))
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
  (println (:expr item))
  context)


(defmethod execute-specific-content :raise [item context]
  (let [old-events (:events context)
        new-events (conj old-events (:event item))]
    (assoc context :events new-events)))   
