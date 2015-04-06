(ns think.sm.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [think.sm.core :refer :all]))

(deftest a-test
  (testing (.concat "testing from: " (System/getProperty "user.dir"))
    (is (= 0 1))))

(def test-base-dir "data")

(defn run-data-test [testname is-optional]
  (testing (.concate "scxml conformance test: " testname))
  (let [subdir (if is-optional "optional" "mandatory")
        full-test-name (str/join [test-base-dir "/" subdir "/test" testname ".txml.ecma.scxml"])
        machine (load-scxml-file full-test-name)
        configuration (execute machine)
        pass-count (count (filter (fn [state] (= (:id state) :pass)) configuration))]
    (is (= 1 pass-count))))
    
