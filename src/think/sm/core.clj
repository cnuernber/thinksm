(ns think.sm.core
  (:require [clojure.xml :as xml])
  (:import (java.io FileInputStream File)))

(defn is-state-type [type]
  (case type
    :state true
    :final true
    :parallel true
    false
    ))


(defn parse-state [parent node]
  (let [type (:tag node)
        id-str (:id (:attrs node))
        id (if id-str (keyword id-str) nil)
        new-vestigial-state { :type type :id id :children [] :onentry [] :onexit [] :transitions [] }
        new-state (reduce parse-state-child new-vestigial-state (:content node))
        new-children (conj (:children parent) new-state)]
    (assoc parent :children new-children)))


(defn parse-transition [parent node]
  (let [type :transition
        target (keyword (:target (:attrs node)))
        new-vestigial-transition {:type type :onentry [] :onexit [] :target target}
        new-transition (reduce parse-state-child new-vestigial-transition (:content node))
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

(defn load-scxml-file [fname]
  (let [xml-dom (xml/parse (FileInputStream. fname))]
    (parse-xml-scxml xml-dom)))

