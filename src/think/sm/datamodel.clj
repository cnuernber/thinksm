(ns think.sm.datamodel
  (:require [think.sm.util :as util]
            [slingshot.slingshot :as sling]))
                    
(defn scan-node-for-datamodel-code-walker[node context]
  "If the node contains cond or expr then we have code
we need to save into an array and place an integer in it's place"
  (let [[var-vec code-vec] context
        var-vec (if (= :data (:type node))
                  (conj var-vec (:id node))
                  (if (= :foreach (:type node))
                    (apply conj var-vec (filter identity (map node [:index :array :item])))
                    var-vec))]
      
    (let [data-seq (filter identity (map (fn [keyword]
                                           (if (keyword node)
                                             [keyword (node keyword)]
                                             nil))
                                         [:expr :cond 
                                          :targetexpr 
                                          :eventexpr
                                          :typeexpr
                                          :delayexpr
                                          :sendidexpr]))
          idx (count code-vec)
          [node idx code-vec] (reduce (fn [[node idx code-vec] data-to-replace]
                                        (let [[keyword code] data-to-replace]
                                          [(assoc node keyword idx) 
                                           (inc idx) 
                                           (conj code-vec data-to-replace)]))
                                      [node idx code-vec]
                                      data-seq)]
      [node [var-vec code-vec]])))


(defn scan-node-for-datamodel-code [node]
  "returns a new tree with code replaced by integers
and a context with a variable declaration vec and a code vec
that corresponds to the integers in the new machine"
  (util/walk-item node [#{} []] scan-node-for-datamodel-code-walker))

(defn output-dm-prefix[var-vec]
  (println 
"(fn [context]
  (let [datamodel (:datamodel context)
       event (:event context)
       _event event
       _sessionid (:session-id context)
       _name (:name (:machine context))")
  (doseq [var-name var-vec]
    (println (str
"      " (name var-name) " (" var-name " datamodel)")))
  (println "      ]"))

(defn output-dm-postfix[]
  (println "))"))


(defn dm-code-to-string [dm-context]
  (with-out-str
    (let [[var-vec code-vec] dm-context
          var-vec (vec var-vec)]
      (println "(defn in-state [context id] (id (:set (:configuration context))))")
      (println "(defn set-datamodel-value [context var value]
  (assoc context :datamodel (assoc (:datamodel context) var value)))")
      (println "[")
      (doseq [code-item code-vec]
        (output-dm-prefix var-vec)
        (println (code-item 1))
        (output-dm-postfix))
      (println "]" ))))

(defn create-datamodel-context [machine]
  (sling/try+
   (let [[machine scanned] (scan-node-for-datamodel-code machine)
         code (dm-code-to-string scanned)
         ;_ (println code)
         function-vec (load-string code)]
     [machine function-vec])
   (catch Throwable e (sling/throw+ { :type :compilation-failure :exception e }))))

(defn execute-expression [context expression]
  (let [dm-context (:dm-context context)
        function (dm-context expression)]
    (when (not function)
      (sling/throw+ { :type :execution-error 
                     :component :datamodel 
                     :reason "Missing expression" }))
    (sling/try+
     (function context)
     (catch Throwable e (sling/throw+
                         (assoc context
                                :errormsg (str "Exception during execution: " e) 
                                :errorevent "error.execution"))))))


              
    
