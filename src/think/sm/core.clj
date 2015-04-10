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
        new-vestigial-state { :type type :id id :children [] :onentry [] :onexit [] :transitions [] :parent (:id parent) }
        new-state (reduce parse-state-child new-vestigial-state (:content node))
        new-children (conj (:children parent) new-state)]
    (assoc parent :children new-children)))

(defn space-delimited-string-to-keyword-array [data]
  (mapv keyword (str/split data #" ")))


(defn parse-transition [parent node]
  (let [type :transition
        target (space-delimited-string-to-keyword-array (:target (:attrs node)))
        transition-content (reduce parse-executable-content [] (:content node))
        new-transition { :type type :content transition-content :targets [target] :parent (:id parent) }
        new-transitions (conj (:transitions parent) new-transition)]
    (assoc parent :transitions new-transition)))

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
  (reduce parse-state-child {:type :scxml :children [] :datamodel (keyword (:attrs node)) } (:content node)))

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
  (let [state (assoc state :doc-order idx)
        [idx children] (reduce (fn [[idx children] state] 
                                 (let [[new-child new-idx] (set-document-order state idx)]
                                   [new-idx (conj new-child children)]))
                               [idx []]
                               (:children state))]
        [(assoc state :children children) idx]))
        


(defn create-state-machine-executable-context [machine]
  ;run through machine creating map from id to state
  ;and setting document order
  (let [[machine state-count] (set-document-order machine 1)
        id-state-map (reduce (fn [map node] (assoc map (:id node) node)) {} (rest (dfs-state-walk machine)))]
    { :machine machine 
     :configuration (create-ordered-set) ;which states are are in
     :history {} ;saved history from exit from history states
     :datamodel {} 
     :id-state-map id-state-map } ))

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
         

(defn get-effective-target-states [item context]
  (let [initial-target-list (if (= (:type item) :state)
                              (get-state-initial-targets item context)
                              (:targets item))]
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
            ancestor ((:parent state) (:id-state-map context))]
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



(defn get-proper-ancestors [state ancestor context]
  (let [state-map (:id-state-map context)]
    (loop [retval ()
           parent ((:parent state) state-map)]
      (if (and parent 
               (not (= (:id parent) (:id ancestor))))
        (recur (conj retval parent) ((:parent parent) state-map))
        retval))))


(defn add-ancestor-states-to-enter [state ancestor enter-args context]
  (let [parent ((:parent state) (:id-state-map context))
        ancestors (get-proper-ancestors state ancestor context)]
    (reduce (fn [enter-args state] (add-ancestor-state-to-enter state enter-args context)) enter-args ancestors)))

(defn compute-entry-set-transition-targets [state-id-list enter-args context]
  (let [id-map (:id-state-map context)
        state-list (map (fn [id] (id id-map)))]
    (reduce (fn [args state] (add-descendant-states-to-enter enter-args context)) enter-args state-list)))


;;----------------------------------------------------
(defn get-transition-domain [transition context]
  "take start, end points and find common ancestor"
  (let [targets (get-effective-target-states transition context)
        state ((:parent transition) (:id-state-map context))
        is-internal (:internal transition)]
    (if is-internal
      (let []
    (reduce (fn [domain new-state]

;;----------------------------------------------------
              

(defn compute-entry-set-transition[transition enter-args context]
  (let [enter-args (compute-entry-set-transition-targets (:targets transition) enter-args context)
        ancester (get-transition-domain transition context)
        targets (get-effective-target-states transition context)]
    (reduce (fn [enter-args state] (add-ancestor-states-to-enter state ancestor enter-args context)) enter-args targets)))
       

(defn compute-entry-set [transitions enter-args context]
  (reduce (fn [enter-args transition] (compute-entry-set-transition transition enter-args context)) enter-args transitions))

(defn entry-state-sort [state-seq]
  (sort-by :document-order state-seq))

(defn enter-states[transitions context]
  (let [enter-args [(create-ordered-set) (create-ordered-set) {}]
        enter-args (compute-entry-set transitions enter-args context)
        [states-to-enter states-for-default-entry default-history-content] enter-args
        ordered-enter-states (enter-state-sort (:vec states-to-enter))]
    (reduce (fn [configuration state]
              (execute-content (:onentry state) context)
              (when (in-ordered-set? states-for-default-entry state)
                (execute-content (:content (get-initial-transition state))))
              (when ((:id state) default-history-content context)
                (execute-content ((:id state) default-history-content)))
              (add-to-ordered-set configuration state))
            (:configuration context)
            states-to-enter)))
                
