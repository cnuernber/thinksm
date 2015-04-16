(ns think.sm.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [think.sm.core :refer :all]))

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
