(ns think.sm.executable
  (:require [think.sm.datamodel :as dm]
            [think.sm.util :as util]
            [slingshot.slingshot :as sling]))

(defmulti parse-executable-content :tag)

(defmethod parse-executable-content :log [node]
  { :type :log :label (:label (:attrs node)) :expr (:expr (:attrs node)) })

(defmethod parse-executable-content :raise [node]
  { :type :raise :event (:event (:attrs node)) })

(defmethod parse-executable-content :assign [node]
  { :type :assign 
   :location (keyword (:location (:attrs node)))
   :expr (:expr (:attrs node)) })


(defn parse-send-param [node]
  { :type :param :name (keyword (:name (:attrs node))) :expr (:expr (:attrs node)) })


(def send-attributes
  { :event :string
   :eventexpr :string
   :target :string
   :targetexpr :string
   :type :string
   :typeexpr :string
   :id :keyword
   :idlocation :string
   :delay :string
   :delayexpr :string
   :namelist :string-list })

(defmethod parse-executable-content :send [node]
  (let [children (map parse-send-param (:content node))]
    (util/parse-attributes node { :type :send :children children } send-attributes)))
    

(defn parse-executable-content-children [node]
  (reduce (fn [children node-child]
                           (conj children (parse-executable-content node-child)))
                         []
                         (:content node)))

(defmethod parse-executable-content :if [node]
  (let [stmt { :type :if :cond (:cond (:attrs node)) }
        children (parse-executable-content-children node)]
    (assoc stmt :children children)))
    

(defmethod parse-executable-content :elseif [node]
  { :type :elseif :cond (:cond (:attrs node)) })

(defmethod parse-executable-content :else [node]
  { :type :else })

(defmethod parse-executable-content :foreach [node]
  (let [attrs (:attrs node)
        stmt {:type :foreach 
              :item (keyword (:item attrs))
              :index (keyword (:index attrs))
              :array (keyword (:array attrs))}]
    (assoc stmt :children (parse-executable-content-children node))))

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
        
      
(defmethod execute-specific-content :foreach [item context]
  (let [item-var (:item item)
        index-var (:index item)
        array-var (:array item)
        data-seq (array-var (:datamodel context))
        children (:children item)]
    (loop [context context
           index 0
           data-item (first data-seq)
           data-seq (rest data-seq)]
      (if data-item
        (let [datamodel (assoc (:datamodel context) item-var data-item)
              datamodel (if index-var (assoc datamodel index-var index) datamodel)
              context (reduce (fn [context child]
                                (execute-specific-content child context))
                              (assoc context :datamodel datamodel)
                              children)]
          (recur context (inc index) (first data-seq) (rest data-seq)))
        context))))

(defn get-item-or-item-expr [item keywd keywdexpr context]
  (if (keywd item)
    (keywd item)
    (let [expr (keywdexpr item)]
      (if expr
        (dm/execute-expression context expr)
        nil))))
 
(defn add-delayed-event[context event delay]
  (let [delayed-events (:delayed-events context)
        delayed-events (conj delayed-events { :delay delay :start-time (:current-time context) :event event })]
    (assoc context :delayed-events delayed-events)))

(defn get-delayed-event-time[delayed-evt time]
  (let [current-delay (- time (:start-time delayed-evt))
        desired-delay (:delay delayed-evt)]
    (- desired-delay current-delay)))

(defn is-delayed-event-ready?[delayed-evt time]
  (>= 0 (get-delayed-event-time delayed-evt time)))
    

(defn update-delayed-events [context current-time]
  (let [time current-time
        events-to-signal (filter (fn [delayed] 
                                   (is-delayed-event-ready? delayed time)) 
                                 (:delayed-events context))]
    (if (not-empty events-to-signal)
      (let [new-delayed-events (filter (fn [delayed] 
                                         (not(is-delayed-event-ready? delayed time))) 
                                       (:delayed-events context))
            events-to-signal (sort-by identity (fn [evt evt2] 
                                                 (< (get-delayed-event-time evt time) 
                                                    (get-delayed-event-time evt2 time))) 
                                      events-to-signal)
            event-names (map :event events-to-signal)
            events (apply conj (:events context) event-names)]
        (assoc context :delayed-events new-delayed-events :events events :current-time current-time))
      (assoc context :current-time current-time ))))

(defn execute-send-param [param context]
  [ (:name param) (dm/execute-expression context (:expr param)) ])
        
(defn build-send-event [item context]
  (let [evt-name (get-item-or-item-expr item :event :eventexpr context)
        param-list (mapcat (fn [param] (execute-send-param param context)) (:children item))]
    (if (not-empty param-list)
      (assoc {} :name evt-name :data (apply assoc {} param-list))
      evt-name)))

(defmethod execute-specific-content :send [item context]
  (let [target (get-item-or-item-expr item :target :targetexpr context)]
    (if (and target (not (= "#_internal" target)))
      (sling/throw+ (assoc context 
                           :errormsg (str "Send target unsupported as of this time: " target) 
                           :errorevent "error.send" ))
      (let [event (build-send-event item context)
            delay-str (get-item-or-item-expr item :delay :delayexpr context)
            delay-ms (if delay-str (util/parse-time-val delay-str) 0)]
        (if (not event)
          (sling/throw+ (assoc context
                               :errormsg (str "Send with null event") 
                               :errorevent "error.send" ))
          (if (= 0 delay-ms)
            (assoc context :events (conj (:events context) event))
            (add-delayed-event context event delay-ms)))))))
 
