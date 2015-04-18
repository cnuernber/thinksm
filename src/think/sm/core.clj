(ns think.sm.core
  (:require [clojure.xml :as xml]
            [clojure.string :as str]
            [think.sm.executable :as exe]
            [think.sm.datamodel :as dm]
            [think.sm.util :as util]
            [slingshot.slingshot :as sling])
  (:import (java.io FileInputStream File)
           (java.util ArrayDeque)))


(declare parse-state-child 
         parse-executable-content 
         add-ancestor-states-to-entry 
         add-descendant-states-to-enter
         add-ancestor-states-to-enter
         get-effective-target-states
         execute-datamodel-content 
         get-atomic-states-from-configuration
         get-initial-transition )


(defn is-state-type [type]
  (case type
    :state true
    :final true
    :parallel true
    :history true
    :scxml true
    false
    ))

(defn is-state-machine-node [node]
  (is-state-type (:type node)))


(defn parse-state [parent node]
  (let [type (:tag node)
        id-str (:id (:attrs node))
        id (if id-str (keyword id-str) nil)
        new-vestigial-state { :type type :id id :children [] :onentry [] 
                             :onexit [] :transitions [] :history [] 
                             :parent (:id parent)
                             :initial (util/space-delimited-string-to-keyword-array (:initial (:attrs node)))}
        new-state (reduce (fn [state child] (parse-state-child child state)) 
                          new-vestigial-state (:content node))
        new-children (conj (:children parent) new-state)]
    (assoc parent :children new-children)))


  

(defn parse-transition [parent node]
  (let [type :transition
        target-list (util/space-delimited-string-to-keyword-array (:target (:attrs node)))
        event-list (util/space-delimited-string-to-array (:event (:attrs node)))
        transition-content (reduce parse-executable-content [] (:content node))
        new-transition { :type type :content transition-content :targets target-list 
                        :parent (:id parent) :id (:id parent) :events event-list 
                        :cond (:cond (:attrs node))}
        new-transitions (conj (:transitions parent) new-transition)]
    (assoc parent :transitions new-transitions)))

(defn parse-executable-content [content node]
  (conj content (exe/parse-executable-content node)))                
        
 (defn parse-entry-exit [parent node]
   (let [dest (:tag node)
         content (reduce parse-executable-content [] (:content node))
         new-entry (conj (dest parent) content)]
     (assoc parent dest new-entry)))

(defmulti parse-state-child :tag)

(defmethod parse-state-child :state [child state]
  (parse-state state child))

(defmethod parse-state-child :parallel [child state]
  (parse-state state child))

(defmethod parse-state-child :final [child state]
  (parse-state state child))

(defmethod parse-state-child :transition [child state]
  (parse-transition state child))

(defmethod parse-state-child :onentry [child state]
  (parse-entry-exit state child))

(defmethod parse-state-child :onexit [child state]
  (parse-entry-exit state child))

(defn parse-keyword-attr-or-content [node keywd]
  (let [retval (keywd (:attrs node))]
    (if retval
      retval
      (apply str (:content node)))))

(defmethod parse-state-child :datamodel [child state]
  (let [data-defs (reduce (fn [defs node]
                            (conj defs {:type :data 
                                        :id (keyword (:id (:attrs node))) 
                                        :expr (parse-keyword-attr-or-content node :expr)}))
                          []
                          (:content child))]
    (assoc state :datamodel data-defs)))

(defmethod parse-state-child :default [child state]
  (sling/throw+ { :type :parse-error :xml-node child :reason "Unrecognized child in parse state" }))
                   

(defn parse-xml-scxml [node]
  (reduce (fn [scxml child]
            (parse-state-child child scxml))
          {:type :scxml :id :scxml_root :children [] 
           :datamodel-type (keyword (:datamodel (:attrs node))) 
           :initial (util/space-delimited-string-to-keyword-array (:initial (:attrs node)))
           :binding (keyword (:binding (:attrs node)))
           } 
          (:content node)))

(defn create-ordered-set [] { :set #{} :vec [] } )

(defn add-to-ordered-set [ordered-set state]
  (let [set (:set ordered-set)
        vec (:vec ordered-set)
        id (:id state)
        exist (id set)]
    (if exist
      ordered-set
      (let [set (conj set id)
            vec (conj vec state)]
        { :set set :vec vec } ))))

(defn filter-ordered-set[ordered-set pred]
  (filterv pred (:vec ordered-set)))

(defn remove-from-ordered-set[ordered-set state]
  (let [set (:set ordered-set)
        vec (:vec ordered-set)
        id (:id state)
        new-vec (filterv (fn [item] (not(= id (:id item)))) vec)
        new-set (disj set id)]
    { :set new-set :vec new-vec }))

(defn remove-from-ordered-set-seq [ordered-set state-seq]
  (reduce remove-from-ordered-set ordered-set state-seq))

(defn in-ordered-set? [ordered-set state]
  ((:id state) (:set ordered-set)))

(defn union-ordered-set [ordered-set state-seq]
  (reduce (fn [ordered-set state] (add-to-ordered-set ordered-set state)) 
          ordered-set 
          state-seq))

;;tree-seq implementation for iterating through all states in a machine
(defn has-state-children? [machine-node]
  (case (:type machine-node)
    :state true
    :parallel true
    :scxml true
    false))


(defn is-final-state?[state]
  (= (:type state) :final))

(defn is-atomic-state? [state]
  (and (or (is-final-state? state)
           (and (= :state (:type state))
                (= 0 (count (:children state)))))))


(defn state-children [machine-node]
  (filter is-state-machine-node (:children machine-node)))


(defn dfs-state-walk [machine-node]
  "Returns a sequence of a depth first walk"
  (tree-seq has-state-children? state-children machine-node))

(defn initial-machine-walker [item context]
  (let [[idx send-ids] context]
    (if (is-state-type (:type item))
      [(assoc item :document-order idx) [(inc idx) send-ids]]
      (if (= :send (:type item))
        (let [send-ids (if (:id item) (conj send-ids (:id item)) send-ids)]
          [item [idx send-ids]])
        [item [idx send-ids]]))))
      

(defn create-context 
  ([machine dm-context current-time]
   (let [[machine [state-count send-ids]] (util/walk-item machine [1 #{}] initial-machine-walker)
         id-state-map (reduce (fn [map node] (assoc map (:id node) node)) 
                              {} 
                              (dfs-state-walk machine))]
     { :machine machine 
      :configuration (add-to-ordered-set (create-ordered-set) machine);which states are we in
      :history {} ;saved history from exit from history states
      :datamodel {} 
      :id-state-map id-state-map 
      :visited-states #{}
      :events (clojure.lang.PersistentQueue/EMPTY)
      :external-events (clojure.lang.PersistentQueue/EMPTY)
      :dm-context dm-context 
      :current-time current-time
      :send-ids send-ids 
      :id-seed 1
      :session-id 1} ))
  ([machine dm-context]
     (create-context machine dm-context (System/currentTimeMillis))))

(defn get-parent-state [state-or-transition context]
  ((:parent state-or-transition) (:id-state-map context)))

(defn is-history-state [state]
  (= :history (:type state)))

(defn is-parallel-state [state]
  (= :parallel (:type state)))

(defn is-compound-state [state]
  (and (not (= :parallel (:type state))) 
       (not-empty (:children state))))

(defn add-descendant-and-ancestor-states[state-seq ancestor enter-args context]
  (loop [state (first state-seq)
         state-seq (rest state-seq)
         enter-args enter-args]
    (if state
      (let [enter-args (add-descendant-states-to-enter state enter-args context)
            enter-args (add-ancestor-states-to-enter state ancestor enter-args context)]
        (recur (first state-seq) (rest state-seq) enter-args))
      enter-args)))

(defn enter-parallel-state [state enter-args context]
  (let [[states-to-enter states-for-default-entry default-history-content] enter-args
        children (:children state)
        not-entered (filter (fn [state] (empty? (filter (fn [child] (is-descendant child state context)) (:vec states-to-enter)))) children)
        enter-args [states-to-enter states-for-default-entry default-history-content]]
    (reduce (fn [enter-args state] (add-descendant-states-to-enter state enter-args context)) enter-args not-entered)))

;worry about this when the time comes.
(defn get-history-state-initial-data [state]
  [() []])


(defn add-effective-target-state[state targets context]
  (if (is-history-state state)
    (let [context-history ((:id state) (:history context))]
      (if (not-empty context-history)
        (union-ordered-set targets context-history)
        (union-ordered-set targets (get-effective-target-states (:transition state)))))
    (add-to-ordered-set targets state)))

(defn get-state-initial-targets [state]
  (:targets (get-initial-transition state)))

(defn id-list-to-state-list[id-seq context]
  (let [id-map (:id-state-map context)]
    (mapv (fn [id] (id id-map)) id-seq)))
         

(defn get-effective-target-states [item context]
  (let [initial-target-list (if (= (:type item) :state)
                              (id-list-to-state-list (get-state-initial-targets item) 
                                                     context)
                              (id-list-to-state-list (:targets item) context))]
    (loop [retval (create-ordered-set)
           state (first initial-target-list)
           initial-target-list (rest initial-target-list)]
      (if state
        (recur (add-effective-target-state state retval context) (first initial-target-list) (rest initial-target-list))
        (:vec retval)))))
       

(defn add-descendant-states-to-enter [state enter-args context]
  (let [[states-to-enter states-for-default-entry default-history-content] enter-args]
    (if (is-history-state state)
      ;entering a history state
      (let [context-history ((:id state) (:history context))
            ancestor (get-parent-state state context)]
        (if (not-empty context-history)
          (add-descendant-and-ancestor-states context-history ancestor enter-args context)
          (let [[state-seq content] (get-history-state-initial-data state)
                default-history-content (assoc default-history-content (:id state) content)
                entry-args [states-to-enter states-for-default-entry default-history-content]]
            (add-descendant-and-ancestor-states state-seq ancestor enter-args context))))
      ;not entering history state
      (let [states-to-enter (add-to-ordered-set states-to-enter state)]
        (if (is-compound-state state)
          (let [states-for-default-entry (add-to-ordered-set states-for-default-entry state)
                enter-args [states-to-enter states-for-default-entry default-history-content] ]
            (add-descendant-and-ancestor-states (get-effective-target-states state context) state enter-args context))
          (if (is-parallel-state state)
            (enter-parallel-state state [states-to-enter states-for-default-entry default-history-content] context)
            [states-to-enter states-for-default-entry default-history-content]))))))
                                

(defn add-ancestor-state-to-enter [state enter-args context]
  (let [[states-to-enter states-for-default-entry default-history-content] enter-args
        states-to-enter (add-to-ordered-set states-to-enter state)
        enter-args [states-to-enter states-for-default-entry default-history-content]]
    (if (is-parallel-state state)
      (enter-parallel-state state enter-args context)
      enter-args)))

(defn get-proper-ancestors 
  ([state ancestor context retval]
  (let [state-map (:id-state-map context)]
    (loop [retval retval
           parent (state-map (:parent state))]
      (if (and parent 
               (or (not ancestor)
                   (not (= (:id parent) (:id ancestor)))))
        (recur (conj retval parent) (state-map (:parent parent)))
        retval))))
  ([state ancestor context]
   (get-proper-ancestors state ancestor context [])))


(defn add-ancestor-states-to-enter [state ancestor enter-args context]
  (let [parent (get-parent-state state context)
        ancestors (get-proper-ancestors state ancestor context)]
    (reduce (fn [enter-args state] (add-ancestor-state-to-enter state enter-args context)) enter-args ancestors)))

(defn compute-entry-set-transition-targets [state-id-list enter-args context]
  (let [id-map (:id-state-map context)
        state-list (filter identity (map (fn [id] (id id-map)) state-id-list))]
    (reduce (fn [args state] (add-descendant-states-to-enter state enter-args context)) enter-args state-list)))


(defn get-document-child-range [parent]
  "If a node's doc order is outside this range, inclusive
then that node is not a child of this parent"
  (let [child-vec (:children parent)
        child-count (count child-vec)]
    (if (> child-count 0)
      [(:document-order (child-vec 0)) (:document-order (child-vec (dec child-count)))]
      [0 -1])))


(defn is-descendant [child parent context]
  (seq (filter (fn [ancestor] (= (:id parent) (:id ancestor))) 
                     (get-proper-ancestors child nil context))))

(defn non-parallel-or-parent [state context]
  (if (= (:type state)
         :parallel)
    (non-parallel-or-parent (get-parent-state state context) context)
    state))


(defn find-LCCA-state [lhs-state rhs-state context]
  "least common compound ancestor"
  (let [lhs-doc (:document-order lhs-state)
        rhs-doc (:document-order rhs-state)
        lhs-less-than (< lhs-doc rhs-doc)
        [min-state max-state] (if lhs-less-than [lhs-state rhs-state] [rhs-state lhs-state])]
    (if (is-descendant max-state min-state context)
      (non-parallel-or-parent min-state context)
      (find-LCCA-state (get-parent-state min-state context) max-state context))))
        


(defn find-LCCA [state-seq context]
  (reduce (fn [lhs rhs] (find-LCCA-state lhs rhs context)) state-seq))


(defn get-transition-domain [transition context]
  "take start, end points and find common ancestor"
  (let [targets (get-effective-target-states transition context)
        state ((:parent transition) (:id-state-map context))
        is-internal (:internal transition)
        source (get-parent-state transition context)]
    (if (and is-internal
             (is-compound-state source)
             (every? (fn [state] (is-descendant state source context)) targets))
      source
      (find-LCCA (conj targets source) context))))
              

(defn targetted-transitions [transitions]
  (filter (fn [transition] (> (count (:targets transition)) 0)) transitions))

(defn compute-entry-set [transitions enter-args context]
  (let [targetted (targetted-transitions transitions)
        targets (mapcat :targets targetted)
        id-map (:id-state-map context)
        ;first pass add those states directly that are targets
        enter-args (reduce (fn [enter-args id] 
                             (add-descendant-states-to-enter (id id-map) enter-args context))
                           enter-args
                           targets)]
    (reduce (fn [enter-args transition] 
              (let [domain (get-transition-domain transition context)]
                (reduce (fn [enter-args id]
                          (add-ancestor-states-to-enter (id id-map) domain enter-args context))
                        enter-args
                        (:targets transition))))
            enter-args
            targetted)))
        
        
(defn enter-state-sort [state-seq]
  (sort-by :document-order state-seq))

(defn execute-content [content-list context]
  (sling/try+ 
    (reduce (fn [context content-or-list]
              (if (or (seq? content-or-list)
                      (vector? content-or-list))
                (execute-content content-or-list context)
                (exe/execute-specific-content content-or-list context)))
            context
            content-list)
    (catch map? context 
            (let [msg (:errormsg context)
                  evt (:errorevent context)
                  context (assoc context :errormsg nil :errorevent nil 
                                 :events (conj (:events context) evt))]
              (println (str "Error during execution: " msg))
              context))))
        

(defn get-initial-transition [state]
  (let [transition (:initial state)]
    (if (= (:type transition) :transition)
      transition
      (if (seq transition)
        { :parent (:id state) :content [] :type :transition :targets transition }
        (let [first-child (first (:children state))]
          (if first-child
            { :parent (:id state) :content [] :type :transition :targets [(:id first-child)] }
            nil))))))

(defn execute-datamodel-content [context state]
  (if (not ((:visited-states context) (:id state)))
    (let [visited (conj (:visited-states context) (:id state))
          dm-list (if (:datamodel state) (:datamodel state) [])
          context (dm/execute-data-list context dm-list)]
      (assoc context :visited-states visited))
    context))
      
        

(defn execute-onentry-content[context state]
  (let [on-entry-content (:onentry state)]
    (if on-entry-content
      (execute-content on-entry-content context)
      context)))

(defn execute-default-entry-content[context state states-for-default-entry]
  (if (in-ordered-set? states-for-default-entry state)
    (execute-content (:content (get-initial-transition state)) context)
    context))

(defn execute-default-history-content[context state default-history-content]
  (let [history-content ((:id state) default-history-content)]
    (if history-content
      (execute-content history-content context)
      context)))

(defn do-enter-state[context state states-for-default-entry default-history-content]
  (let [new-context (-> context
                        (execute-datamodel-content state)
                        (execute-onentry-content state)
                        (execute-default-entry-content state states-for-default-entry)
                        (execute-default-history-content state default-history-content))
        configuration (add-to-ordered-set (:configuration new-context) state)]
    (assoc new-context :configuration configuration)))

;this is done in a separate pass so that we generate only exactly the number of
;events we should
(defn signal-done-states[context ordered-enter-states]
  (let [final-states (filter is-final-state? ordered-enter-states)
        context (reduce (fn [context final]
                          (assoc context :events 
                                 (conj (:events context) 
                                       (str "done.state." (name (:parent final))))))
                        context
                        final-states)
        all-ancestors (mapcat (fn [state] (get-proper-ancestors state nil context)) 
                              final-states)
        parallel-ancestors (distinct (filter (fn [state] (= :parallel (:type state))) 
                                           all-ancestors))
        config-atomics (get-atomic-states-from-configuration context)
        done-pa (filter (fn [parallel]
                          (let [parallel-children (filter 
                                                   (fn [state]
                                                     (is-descendant state parallel context))
                                                   config-atomics)]
                            (every? is-final-state? parallel-children)))
                        parallel-ancestors)]
    (reduce (fn [context parallel]
              (assoc context :events 
                     (conj (:events context)
                             (str "done.state." (name (:id parallel))))))
            context
            done-pa)))
        

(defn enter-states[context transitions]
  "Enter the states indicated by this list of transitions.  
Return a new configuration ordered set"
  (let [enter-args [(create-ordered-set) (create-ordered-set) {}]
        enter-args (compute-entry-set transitions enter-args context)
        [states-to-enter states-for-default-entry default-history-content] enter-args
        ordered-enter-states (enter-state-sort (:vec states-to-enter))
        final-context (reduce (fn [context state]
                                (do-enter-state context state states-for-default-entry 
                                                default-history-content))
                              context
                              ordered-enter-states)
        final-context (signal-done-states final-context ordered-enter-states)
        configuration (:configuration final-context)
        ordered-configuration-vec (vec (enter-state-sort (:vec configuration)))]
    (assoc final-context :configuration (assoc configuration :vec ordered-configuration-vec))))


(defn execute-early-binding[context]
  (let [machine (:machine context)
        all-states (dfs-state-walk machine)]
    (reduce execute-datamodel-content context all-states)))

(defn get-initial-configuration [context]
  (let [scxml (:machine context)
        transition (get-initial-transition scxml)
        context (if (or (not (:binding scxml))
                        (= :early (:binding scxml)))
                  (execute-early-binding context)
                  (execute-datamodel-content context scxml))]
    (enter-states context [transition])))


(defn get-atomic-states-from-configuration [context]
  (let [configuration (:configuration context)]
    (filter is-atomic-state? (:vec configuration))))


(defn state-seq-to-transition-seq [state-seq]
  (mapcat :transitions state-seq))

(defn is-active-transition?[transition context]
  (if (:cond transition)
    (dm/execute-expression context (:cond transition))
    true))
;since we don't support event or conditions yet...
(defn eventless-transition? [transition context]
  (let [event-list (:events transition)
        event-count (if event-list (count event-list) 0)]
    (= event-count 0)))

(defn event-name-and-transition-event-spec-match? [^String event-name ^String event-spec]
  (let [name-len (if (nil? event-name) 0 (.length event-name))
        spec-len (.length event-spec)]
    (if (or (= 0 spec-len)
            (= 0 name-len))
      false
      (if (= event-spec "*")
        true
        (let [spec-general (.endsWith event-spec ".*")
              event-spec (if spec-general (.substring event-spec 0 (- spec-len 2)) event-spec)
              spec-len (.length event-spec)
              sub-match (.startsWith event-name event-spec)]
          (if sub-match
            (if (= name-len spec-len)
              true
              (and (< spec-len name-len)
                   (= \. (.charAt event-name spec-len))))
            false))))))


(defn evented-transition?[transition event-name context]
  (not-empty (filter 
                   (fn [event-spec] (event-name-and-transition-event-spec-match? event-name event-spec))
                   (:events transition))))


(defn get-active-transition[state-seq filterp context]
  (let [transitions (state-seq-to-transition-seq state-seq)
        active-transitions (filter (fn [transition] (is-active-transition? transition context)) (filter filterp transitions))]
    (first active-transitions)))

(defn get-transitions-from-atomics [atomics-seq filterp context]
  (reduce (fn [transitions atomic]
            (let [state-seq (get-proper-ancestors atomic nil context [atomic])
                  first-transition (get-active-transition state-seq filterp context)]
              (if first-transition
                (conj transitions first-transition)
                transitions)))
          []
          atomics-seq))

(defn transition-preempt? [first-trans second-trans context]
  "does the first transition preempt the second transition"
  (let [first-domain (get-transition-domain first-trans context)
        second-domain (get-transition-domain second-trans context)]
    (or (= (:id first-domain)
           (:id second-domain))
        (is-descendant second-domain first-domain context))))
    

(defn update-filtered-transitions[filtered-transitions incoming context]
  (let [filter-vec (:vec filtered-transitions)
        incoming-preempted (not-empty (filter 
                                       (fn [filter-trans] 
                                         (transition-preempt? filter-trans incoming context))
                                       filter-vec))]
    (if incoming-preempted
      filtered-transitions
      (let [filtered-preempted (filter
                                (fn [filter-trans]
                                  (transition-preempt? incoming filter-trans context))
                                filter-vec)
            filtered-transitions (remove-from-ordered-set-seq filtered-transitions filtered-preempted)]
        (add-to-ordered-set filtered-transitions incoming)))))

(defn remove-conflicting-transitions [transitions context]
  "it is possible that the same transition is selected multiple times
coming from different leaves.  Furthermore the selected transitions may
conflict with each other at this point forcing a further filtering step"
  (let [filtered-transitions (reduce
                              (fn [filtered-transitions incoming]
                                (update-filtered-transitions filtered-transitions incoming context))
                              (create-ordered-set)
                              transitions)]
    (:vec filtered-transitions)))



(defn select-transitions [context transition-p]
  (let [atomics (get-atomic-states-from-configuration context)
        selected (get-transitions-from-atomics atomics transition-p context)]
    (remove-conflicting-transitions selected context)))



(defn select-eventless-transitions [context]
  (select-transitions context (fn [transition] (eventless-transition? transition context))))

(defn select-evented-transitions [context event-name]
  (select-transitions context (fn [transition] (evented-transition? transition event-name context))))


(defn get-descendants [parents children context]
  "if a child is a descendant of any of the parents then return the child
must not change the order of children, simply remove children that do not
fit criteria"
  (filter (fn [child]
            (not-empty (filter (fn [parent] (is-descendant child parent context)) parents)))
            children))


(defn compute-exit-set [transitions context]
  (let [domains (map (fn [transition] (get-transition-domain transition context)) (targetted-transitions transitions))
        descendants (get-descendants domains (:vec (:configuration context)) context)]
    ;we know configuration is sorted in document order
    ;so we know that descendants must be.  For exiting states, however
    ;we really want exactly the reverse of document order.
    (reverse descendants)))


(defn exit-states[context transitions]
  "exit states returning a new configuration"
  (let [states-to-exit (compute-exit-set transitions context)]
    (reduce (fn [context state]
              (let [context (execute-content (:onexit state) context)
                    configuration (remove-from-ordered-set (:configuration context) state)]
                (assoc context :configuration configuration)))
            context
            states-to-exit)))


(defn execute-transition-content [context transitions]
  (reduce (fn [context content] (execute-content content context)) context (map :content transitions)))


(defn microstep [context transitions]
  (-> context
      (exit-states transitions)
      (execute-transition-content transitions)
      (enter-states transitions)))


;returns a pure machine.  Note that you still need context to execute this machine.
(defn load-scxml-file [fname]
  (let [xml-dom (xml/parse (FileInputStream. fname))]
    (parse-xml-scxml xml-dom)))


(defn create-and-initialize-context [loaded-scxml]
  (let [[loaded-scxml dm-context] (dm/create-datamodel-context loaded-scxml)]
    (-> (create-context loaded-scxml dm-context)
        (get-initial-configuration))))
    
;step the state machine returning a new context with an
;update configuration.

(defn get-event-name [event]
  (if (string? event)
    event
    (:name event)))

(defn step-next-event [context event-queue]
  (let [events (event-queue context)
        next-event (first events)]
    (if next-event
      (let [events (pop events)
            event-name (get-event-name next-event)
            context (assoc context event-queue events :event next-event)
            transitions (select-evented-transitions context event-name)
            context (if (not-empty transitions) (microstep context transitions) context)]
        context)
      context)))

(defn step-state-machine 
  ([context current-time]
   (let [context (exe/update-delayed-events (assoc context :event nil) current-time)
         eventless (select-eventless-transitions context)]
     (if (not-empty eventless)
       (microstep context eventless)
       (if (not-empty (:events context))
         (step-next-event context :events)
         (step-next-event context :external-events)))))
  ([context]
   (step-state-machine context (System/currentTimeMillis))))

(defn pop-and-select-evented-transitions [context]
  (let [next-event (first (:events context))]
    (if next-event
      (select-evented-transitions (assoc context :event next-event) (get-event-name next-event))
      ())))

(defn state-machine-stable? [context]
  (not (or (not-empty (select-eventless-transitions context))
           (not-empty (:events context))
           (not-empty (:external-events context))
           (not-empty (:delayed-events context)))))

(defn step-until-stable [context]
  (loop [context context]
    (if (state-machine-stable? context)
      context
      (recur (step-state-machine context)))))
