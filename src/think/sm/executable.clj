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



(def send-attributes
  { :event :string
   :eventexpr :string
   :target :string
   :targetexpr :string
   :type [:string :event-type]
   :typeexpr :string
   :id :keyword
   :idlocation :keyword
   :delay :string
   :delayexpr :string
   :namelist :keyword-list })

(defmethod parse-executable-content :send [node]
  (let [[params content] (util/parse-content-or-param-children node)
        stmt { :type :send }
        stmt (if (not-empty params) (assoc stmt :children params) stmt )
        stmt (if content (assoc stmt :expr content) stmt)]
    (util/parse-attributes node stmt send-attributes)))
    

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

(defmethod parse-executable-content :cancel [node]
  (util/parse-attributes node {:type :cancel} { :sendid :string :sendidexpr :string }))

(defmethod parse-executable-content :script [node]
  { :type :script :expr (apply str (:content node)) })

(defmethod parse-executable-content :default [node]
    (sling/throw+ { :type :parse-error :xml-node node :reason "Unrecognized executable content" }))

(defmulti execute-specific-content :type)


(defmethod execute-specific-content :log [item context]
  (println (dm/execute-expression context (:expr item)))
  context)

(def scxml-event-processor "http://www.w3.org/TR/scxml/#SCXMLEventProcessor")

(defn create-event[evt is-internal]
  (let [evt (if (string? evt) { :name evt } evt)
        evt-type (if (contains? evt :type) 
                   (:type evt)
                   (if is-internal
                     "internal"
                     "external"))
        origin-type (if (contains? evt :origintype)
                      (:origintype evt)
                      (if is-internal
                        nil
                        scxml-event-processor))]
    (assoc evt :origintype origin-type :type evt-type)))


(defn queue-event [evt context is-internal]
  (let [queue (if is-internal :events :external-events)
        evt (create-event evt is-internal)]
    (assoc context queue 
           (conj (queue context) evt))))


(defmethod execute-specific-content :raise [item context]
  (let [old-events (:events context)
        new-events (conj old-events (:event item))]
    (queue-event (:event item) context true)))


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
        location (:location item)]
    (if (contains? datamodel location)
      (let [value (dm/execute-expression context (:expr item))
            datamodel (assoc datamodel location value)]
        (assoc context :datamodel datamodel))
      (sling/throw+ (assoc context 
                           :errormsg (str "Assinging to invalid datamodel location: " location) 
                           :errorevent "error.execution" )))))
        
      
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
        delayed-events (conj delayed-events { :delay delay
                                             :start-time (:current-time context) 
                                             :event event })]
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
        (assoc context :delayed-events new-delayed-events :events events 
               :current-time current-time))
      (assoc context :current-time current-time ))))


(defn execute-send-param [param context]
  (if (:expr param)
    [ (:name param) (dm/execute-expression context (:expr param)) ]
    (if (contains? (:datamodel context) (:location param) )
      [ (:name param) ((:location param) (:datamodel context)) ]
      (sling/throw+ (assoc context 
                                                           :errormsg (str "Invalid datamodel location " (:location param) ) 
                                                           :errorevent "error.execution" )))))

(defn execute-event-data[item context]
  (let [param-list (mapcat (fn [param] (execute-send-param param context)) (:children item))]
    (if (:expr item) 
      (dm/execute-expression context (:expr item)) 
      (if (not-empty param-list)
        (apply assoc {} param-list)
        {}))))


(defn build-send-event [item context send-id target]
  (let [evt-type (if (or (:event-type item)
                         (:typeexpr item))
                   (get-item-or-item-expr item :event-type :typeexpr context)
                   scxml-event-processor)
        evt-name (if (:event item) (:event item) (dm/execute-expression context (:eventexpr item)))
        evt-data (execute-event-data item context)
        datamodel (:datamodel context)
        evt-data (if (:namelist item)
                   (apply assoc evt-data 
                          (mapcat (fn [keyword]
                                    (if (contains? datamodel keyword)
                                      [keyword (datamodel keyword)]
                                      (sling/throw+ (assoc context 
                                                           :errormsg (str "Invalid datamodel location " keyword) 
                                                           :errorevent "error.execution" ))))
                                  (:namelist item)))
                   evt-data)]
    (if (not (= evt-type scxml-event-processor))
      (sling/throw+ (assoc context 
                           :errormsg (str "Send type unsupported as of this time: " evt-type) 
                           :errorevent "error.execution" ))
      (create-event 
       (assoc {} :name evt-name :data evt-data :sendid (if send-id (name send-id)  nil)
              :origintype evt-type :origin target )
       (= evt-type "#_internal")))))


(defn get-or-generate-send-id [item context]
  (if (and (not (:id item))
           (not (:idlocation item)))
    [context nil]
    (if (:id item)
      [context (:id item)]
      (let [[new-id id-seed send-ids] (util/generate-unique-id 
                                       "send" (:id-seed context) (:send-ids context))
            datatable (if (:idlocation item) 
                        (assoc (:datamodel context) (:idlocation item) new-id)
                        (:datamodel context))
            context (assoc context :id-seed id-seed :send-ids send-ids :datamodel datatable)]
        [context new-id]))))
    

(defmethod execute-specific-content :send [item context]
  (let [target (get-item-or-item-expr item :target :targetexpr context)
        session_target (str "#_scxml_" (:session-id context))]
    (if (and target (not (or (= "#_internal" target)
                             (= session_target target))))
      (let [error-event (if (.startsWith target "#") "error.communication" "error.execution")]
        (sling/throw+ (assoc context 
                             :errormsg (str "Send target unsupported as of this time: " target) 
                             :errorevent error-event )))
      (let [[context id-for-event] (get-or-generate-send-id item context)
            event (build-send-event item context id-for-event session_target)
            delay-str (get-item-or-item-expr item :delay :delayexpr context)
            delay-ms (if delay-str (util/parse-time-val delay-str) 0)
            event-queue (if (= target "#_internal") :events :external-events)]
        (if (not event)
          (sling/throw+ (assoc context
                               :errormsg (str "Send with null event") 
                               :errorevent "error.send" ))
          (if (= 0 delay-ms)
            (assoc context event-queue (conj (event-queue context) event))
            (add-delayed-event context event delay-ms)))))))


(defmethod execute-specific-content :cancel [item context] 
  (let [send-id (get-item-or-item-expr item :sendid :sendidexpr context)
        delayed-events (filter (fn [delayed] 
                                 (not (= send-id (:sendid (:event delayed)))))
                                 (:delayed-events context))]
    (assoc context :delayed-events delayed-events)))

(defmethod execute-specific-content :script [item context]
  (dm/execute-expression context (:expr item)))
    
