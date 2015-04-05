(ns think.sm.testdata
  (:require [clojure.xml :as xml]
            [clojure.string :as str]
            [clj-http.client :as http])
  (:import (java.io FileInputStream File)
           (java.nio.file Paths Path)
           (net.sf.saxon Transform)))

;Parses the data table, downloads the urls to appropriate directories, and
;runs the xpath stuff require to produce ECMAscript tests

(def test-src-url "http://www.w3.org/Voice/2013/scxml-irp/")

(defn parse-test-html-xml [data-dir]
  (let [html-file (Paths/get data-dir (into-array String ["test_src_html.xml"]))
        table (xml/parse (FileInputStream. (.toString html-file)))
        body ((:content table) 0)
        tr-list (rest (:content body))]
    tr-list))

(defn content-text [item]
  (:content item))

(defn content-href [item]
  (:href (:attrs ((:content item) 0))))
    
    
(defn parse-test-entry [entry]
  (let [items (:content entry)
        test-url (content-href (items 1))
        test-name (.substring test-url (+ (.indexOf test-url (int \/)) 1))
        test-level ((content-text (items 3)) 0)
        full-input-url (str/join [test-src-url test-url])
        relative-output-url (str/join [test-level "/" test-name])]
    { :url full-input-url :file relative-output-url }))


(defn parse-test-spec-to-map-data [data-dir]
  (map parse-test-entry (parse-test-html-xml data-dir)))

(defn make-directory! [path]
  (.mkdir (File. path)))

(defn write-test-data-item-untransformed! [data-dir item]
  (let [request (http/get (:url item))
        text (:body request)
        rel-out-path (:file item)
        output-result-file-path (Paths/get data-dir (into-array String [rel-out-path]))
        out-dir (.substring rel-out-path 0 (.indexOf rel-out-path (int \/)))
        output-result-directory (Paths/get data-dir (into-array String [out-dir]))
        _ (make-directory! (.toString output-result-directory))]
    (spit (.toString output-result-file-path) text)
    {:text text :result-path (.toString output-result-file-path) :out-dir (.toString output-result-directory)}))


(defn write-test-data-untransformed! [data-dir parsed]
  (loop [item (first parsed)
         items (rest parsed)]
    (let [next (first items)]
      (write-test-data-item-untransformed! data-dir item)
      (when next
        (recur next (rest items))))))
      


(defn transform-data-item! [data-dir item]
  (let [rel-out-path (:file item)
        output-result-file-path (.toString (Paths/get data-dir (into-array String [rel-out-path])))
        output-transformed (str/join [output-result-file-path ".ecma.scxml"])
        xslt-path (.toString (Paths/get data-dir (into-array String ["confEcma.xsl"])))
        transform-args [(.concat "-s:" output-result-file-path) (.concat "-xsl:" xslt-path) (.concat "-o:" output-transformed)]]
    (when (not (.endsWith output-result-file-path ".txt"))
      (try
        (Transform/main (into-array String transform-args))
        (catch Exception e nil)))))

        
  
(defn transform-data! [data-dir parsed]
  (loop [item (first parsed)
         items (rest parsed)]
    (let [next (first items)]
      (transform-data-item! data-dir item)
      (when next
        (recur next (rest items))))))
