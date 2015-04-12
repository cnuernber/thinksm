(ns think.sm.core
  (:require [clojure.xml :as xml]
            [clojure.string :as str])
  (:import (java.io FileInputStream File)))


(declare parse-state-child 
         parse-executable-content 
         add-ancestor-states-to-entry 
         add-descendant-states-to-enter
         add-ancestor-states-to-enter
         get-effective-target-states)


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
        new-vestigial-state { :type type :id id :children [] :onentry [] :onexit [] :transitions [] :history [] :parent (:id parent) }
        new-state (reduce parse-state-child new-vestigial-state (:content node))
        new-children (conj (:children parent) new-state)]
    (assoc parent :children new-children)))

(defn space-delimited-string-to-keyword-array [data]
  (mapv keyword (str/split data #" ")))


(defn parse-transition [parent node]
  (let [type :transition
        target-list (space-delimited-string-to-keyword-array (:target (:attrs node)))
        transition-content (reduce parse-executable-content [] (:content node))
        new-transition { :type type :content transition-content :targets target-list :parent (:id parent) :id (:id parent) }
        new-transitions (conj (:transitions parent) new-transition)]
    (assoc parent :transitions new-transitions)))

(defn parse-log [node]
  { :type :log :label (:label (:attrs node)) :expr (:expr (:attrs node)) }) 

(defn parse-executable-content [content node]
  (let [item (case (:tag node)
               :log (parse-log node)
               (throw (Throwable. "Unrecognized executable content")))]
    (conj content item)))
                
        
 (defn parse-entry-exit [parent node]
   (let [dest (:tag node)
         content (reduce parse-executable-content [] (:content node))
         new-entry (conj (dest parent) content)]
     (assoc parent dest new-entry)))
      

(defn parse-state-child [state child]
  (let [type (:tag child)]
    (if (is-state-type type)
      (parse-state state child)
      (if (= type :transition)
        (parse-transition state child )
        (if (or (= type :onentry)
                (= type :onexit))
          (parse-entry-exit state child)
          (throw (Throwable. "Unrecognized state child")))))))
                   

(defn parse-xml-scxml [node]
  (reduce parse-state-child {:type :scxml :id :scxml_root :children [] :datamodel (keyword (:attrs node)) } (:content node)))

;returns a pure machine.  Note that you still need context to execute this machine.
(defn load-scxml-file [fname]
  (let [xml-dom (xml/parse (FileInputStream. fname))]
    (parse-xml-scxml xml-dom)))

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
  (reduce (fn [ordered-set state] (add-to-ordered-set ordered-set state)) ordered-set state-seq))

;;tree-seq implementation for iterating through all states in a machine
(defn has-state-children? [machine-node]
  (case (:type machine-node)
    :state true
    :parallel true
    :scxml true
    false))

(defn state-children [machine-node]
  (filter is-state-machine-node (:children machine-node)))


(defn dfs-state-walk [machine-node]
  "Returns a sequence of a depth first walk"
  (tree-seq has-state-children? state-children machine-node))

;This is done as a second step so that we can create states outside the xml
;context and then when creating the state machine executable context things get
;setup.
(defn set-document-order [state idx]
  (let [state (assoc state :document-order idx)
        [idx children] (reduce (fn [[idx children] state] 
                                 (let [[new-child new-idx] (set-document-order state (inc idx))]
                                   [new-idx (conj children new-child)]))
                               [idx []]
                               (:children state))]
    [(assoc state :children children) idx]))
        


(defn create-context [machine]
  ;run through machine creating map from id to state
  ;and setting document order
  (let [[machine state-count] (set-document-order machine 1)
        id-state-map (reduce (fn [map node] (assoc map (:id node) node)) {} (dfs-state-walk machine))]
    { :machine machine 
     :configuration (add-to-ordered-set (create-ordered-set) machine);which states are we in
     :history {} ;saved history from exit from history states
     :datamodel {} 
     :id-state-map id-state-map } ))

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
        not-entered (filter (fn [state] (not(in-ordered-set? states-to-enter state))) children) ;delicate naming on that one
        enter-args [states-to-enter states-for-default-entry default-history-content]]
    (reduce (fn [state] (add-descendant-states-to-enter state enter-args context)) enter-args not-entered)))

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

(defn get-state-initial-targets [state context]
  (let [initial-transition (:initial state)]
    (if initial-transition
      (:targets initial-transition)
      (list (:id (first (:children state)))))))

(defn id-list-to-state-list[id-seq context]
  (let [id-map (:id-state-map context)]
    (mapv (fn [id] (id id-map)) id-seq)))
         

(defn get-effective-target-states [item context]
  (let [initial-target-list (if (= (:type item) :state)
                              (get-state-initial-targets item context)
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
        state-list (map (fn [id] (id id-map)) state-id-list)]
    (reduce (fn [args state] (add-descendant-states-to-enter state enter-args context)) enter-args state-list)))


(defn get-document-child-range [parent]
  "If a node's doc order is outside this range, inclusive
then that node is not a child of this parent"
  (let [child-vec (:children parent)
        child-count (count child-vec)]
    (if (> child-count 0)
      [(:document-order (child-vec 0)) (:document-order (child-vec (dec child-count)))]
      [0 -1])))


(defn is-descendant [child parent]
  (let [child-doc-order (:document-order child)
        [range-start range-end] (get-document-child-range parent)]
        (if (and (<= child-doc-order range-end)
                 (>= child-doc-order range-start))
          true
          false)))


(defn non-parallel-or-parent [state context]
  (if (= (:type state)
         :parallel)
    (non-parallel-or-parent (get-parent-state state context))
    state))


(defn find-LCCA-state [lhs-state rhs-state context]
  "least common compound ancestor"
  (let [lhs-doc (:document-order lhs-state)
        rhs-doc (:document-order rhs-state)
        lhs-less-than (< lhs-doc rhs-doc)
        [min-state max-state] (if lhs-less-than [lhs-state rhs-state] [rhs-state lhs-state])]
    (if (is-descendant max-state min-state)
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
             (every? (fn [state] (is-descendant state source)) targets))
      source
      (find-LCCA (conj targets source) context))))
              

(defn compute-entry-set-transition[transition enter-args context]
  (let [enter-args (compute-entry-set-transition-targets (:targets transition) enter-args context)
        ancestor (get-transition-domain transition context)
        targets (get-effective-target-states transition context)]
    (reduce (fn [enter-args state] (add-ancestor-states-to-enter state ancestor enter-args context)) enter-args targets)))
       

(defn compute-entry-set [transitions enter-args context]
  (reduce (fn [enter-args transition] (compute-entry-set-transition transition enter-args context)) enter-args transitions))

(defn enter-state-sort [state-seq]
  (sort-by :document-order state-seq))

(defn execute-content [content-list-list context])

(defn get-initial-transition [state]
  (let [transition (:initial state)]
    (if transition
      transition
      (let [first-child (first (:children state))]
        (if first-child
          { :parent (:id state) :content [] :type :transition :targets [(:id first-child)] }
          nil)))))

(defn enter-states[transitions context]
  "Enter the states indicated by this list of transitions.  Return a new configuration ordered set"
  (let [enter-args [(create-ordered-set) (create-ordered-set) {}]
        enter-args (compute-entry-set transitions enter-args context)
        [states-to-enter states-for-default-entry default-history-content] enter-args
        ordered-enter-states (enter-state-sort (:vec states-to-enter))
        configuration (reduce (fn [configuration state]
                                (execute-content (:onentry state) context)
                                (when (in-ordered-set? states-for-default-entry state)
                                  (execute-content (:content (get-initial-transition state))))
                                (when ((:id state) default-history-content)
                                  (execute-content ((:id state) default-history-content) context))
                                (add-to-ordered-set configuration state))
                              (:configuration context)
                              ordered-enter-states)
        ordered-configuration (enter-state-sort (:vec configuration))]
    ;we keep configuration sorted in document order.  This allows some algorithmic
    ;simplifications and is just logical to look at.
    (assoc configuration :vec (vec ordered-configuration))))
        
                           
                

(defn get-initial-configuration [context]
  (let [scxml (:machine context)
        transition (get-initial-transition scxml)]
    (enter-states [transition] context)))

(defn is-atomic-state [state]
  (and (= (:type state)
          :state)
       (= 0 (count (:children state)))))

(defn get-atomic-states-from-configuration [context]
  (let [configuration (:configuration context)]
    (filter is-atomic-state (:vec configuration))))


(defn state-seq-to-transition-seq [state-seq]
  (mapcat :transitions state-seq))

;since we don't support event or conditions yet...
(defn active-eventless-transition? [transition context]
  true)

(defn get-active-transition[state-seq filterp]
  (let [transitions (state-seq-to-transition-seq state-seq)
        active-transitions (filter filterp transitions)]
    (first active-transitions)))

(defn get-transitions-from-atomics [atomics-seq filterp context]
  (reduce (fn [transitions atomic]
            (let [state-seq (get-proper-ancestors atomic nil context [atomic])
                  first-transition (get-active-transition state-seq filterp)]
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
        (is-descendant second-domain first-domain))))
    

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
  (select-transitions context (fn [transition] (active-eventless-transition? transition context))))


(defn get-descendants [parents children]
  "if a child is a descendant of any of the parents then return the child
must not change the order of children, simply remove children that do not
fit criteria"
  (filter (fn [child]
            (not-empty (filter (fn [parent] (is-descendant child parent)) parents)))
            children))


(defn compute-exit-set [transitions context]
  (let [domains (map (fn [transition] (get-transition-domain transition context)) transitions)
        descendants (get-descendants domains (:vec (:configuration context)))]
    ;we know configuration is sorted in document order
    ;so we know that descendants must be.  For exiting states, however
    ;we really want exactly the reverse of document order.
    (reverse descendants)))


(defn exit-states[transitions context]
  "exit states returning a new configuration"
  (let [states-to-exit (compute-exit-set transitions context)]
    (reduce (fn [configuration state]
              (execute-content (:onexit state) context)
              ;note that the state is still in the configuration when its content is executed
              (remove-from-ordered-set configuration state))
            (:configuration context)
            states-to-exit)))
                
    
(defn microstep [transitions context]
  (let [configuration (exit-states transitions context)
        transition-content (map :content transitions)
        _ (reduce (fn [_ content] (execute-content content context) []) [] transition-content)
        context (assoc context :configuration configuration)
        configuration (enter-states transitions context)]
    (assoc context :configuration configuration)))

    
;step the state machine returning a new context with an
;update configuration.

(defn step-state-machine [context]
  (let [eventless (select-eventless-transitions context)]
    (if (not-empty eventless)
      (microstep eventless context)
      context)))
      
