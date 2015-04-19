(ns think.sm.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [think.sm.core :refer :all]
            [slingshot.slingshot :as sling])
  (:import (java.io File)))

(def test-base-dir "data")

(defn run-data-test [testname is-optional]
  (testing (.concat "scxml conformance test: " testname))
  (let [subdir (if is-optional "optional" "mandatory")
        full-test-name (str/join [test-base-dir "/" subdir "/test" testname ".txml.clj.scxml"])
        machine (load-scxml-file full-test-name)
        context (create-and-initialize-context machine)
        final-context (step-until-stable context)
        final-configuration (:configuration final-context)]
    (is (and (:pass (:set final-configuration))
             (not (:fail (:set final-configuration)))))
    final-configuration))
    

(defmacro create-conformance-test [number is-optional]
  (let [testname (symbol (.concat "test-" (str number)))]
  `(deftest ~testname
     (run-data-test ~(str number) ~is-optional))))


(create-conformance-test 355 false)
(create-conformance-test 144 false)
(create-conformance-test 147 false)
(create-conformance-test 148 false)
(create-conformance-test 150 false)
(create-conformance-test 151 false)
;(create-conformance-test 152 false) syntax error causes compilation failure
(create-conformance-test 153 false)
(create-conformance-test 155 false)
;(create-conformance-test 156 false) syntax error in test
(create-conformance-test 158 false)
(create-conformance-test 159 false)
(create-conformance-test 172 false)
(create-conformance-test 173 false)
(create-conformance-test 174 false)
(create-conformance-test 175 false)
(create-conformance-test 176 false)
;(create-conformance-test 178 false) crap test
(create-conformance-test 179 false)
(create-conformance-test 183 false)
(create-conformance-test 185 false)
(create-conformance-test 186 false)
;(create-conformance-test 187 false) invoke
;(create-conformance-test 189 false) internal-vs-external queue
;(create-conformance-test 190 false) internal-vs-external queue
;(create-conformance-test 191 false) invoke
;(create-conformance-test 192 false) invoke
(create-conformance-test 194 false)
(create-conformance-test 198 false)
(create-conformance-test 199 false)
(create-conformance-test 200 false)
(create-conformance-test 205 false)
;(create-conformance-test 207 false) invoke
(create-conformance-test 208 false)
(create-conformance-test 210 false)
;(create-conformance-test 215 false) invoke
;(create-conformance-test 216 false) invoke
;(create-conformance-test 220 false) invoke
;(create-conformance-test 223 false) invoke
;(create-conformance-test 224 false) invoke
;(create-conformance-test 226 false) invoke
;(create-conformance-test 228 false) invoke
;(create-conformance-test 229 false) invoke
;(create-conformance-test 230 false) invoke
(create-conformance-test 230 false)
(create-conformance-test 230 false)
(create-conformance-test 230 false)

(defn test-number-to-file-name [number]
  (str/join [test-base-dir "/" "mandatory" "/test" number ".txml.clj.scxml"]))

;552 needs an external file.  This won't always work and I am not going
;to support it at this time.  The others are test that require user checking
;of some sort
(def test-number-blacklist [415 178 152 156 552])
(def test-path-blacklist (into #{} (map test-number-to-file-name test-number-blacklist)))

(defn run-test-and-report-result [fname]
  (sling/try+
   (let [full-test-name fname
         machine (load-scxml-file full-test-name)
         context (create-and-initialize-context machine)
         final-context (step-until-stable context)
         final-configuration (:configuration final-context)]
     (if (and (:pass (:set final-configuration))
              (not (:fail (:set final-configuration))))
       { :type :pass }
       { :type :fail }))
   (catch map? data data)
   (catch Exception e { :type :unknown-error :exception e })))




(defn list-all-test-files[]
  (let [folder (File. "data/mandatory")
        files (.listFiles folder)
        str-files (map (fn [f] (.toString f)) files)]
    (sort (filter (fn [str]
                    (and (not (test-path-blacklist str))
                         (.endsWith str "clj.scxml")))
            str-files))))



(defn run-test-and-bin-results [test-list]
  (reduce (fn [results test-file-name]
            (let [test-result (run-test-and-report-result test-file-name)]
              (if (= :parse-error (:type test-result))
                (let [xml-tag (:tag (:xml-node test-result))
                      existing (:parse-error results)
                      result-map (if existing existing {})
                      result-map (assoc result-map xml-tag (conj (xml-tag result-map) test-file-name))]
                  (assoc results :parse-error result-map))
                (assoc results (:type test-result) (conj ((:type test-result) results) test-file-name)))))
          {}
          test-list))
                  
          
(defn bin-all-tests[]
  (run-test-and-bin-results (list-all-test-files)))

(defn test-get-initial-context [testnum]
  (create-and-initialize-context (load-scxml-file (test-number-to-file-name testnum))))

(defn run-individual-test[testnum]
  (run-test-and-report-result (test-number-to-file-name testnum)))


(defn fn-pow-2 [f x n] (reduce (fn [x _] (f x)) x (range n)))
