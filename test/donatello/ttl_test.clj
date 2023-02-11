(ns donatello.ttl-test
  (:require [clojure.test :refer [testing is deftest]]
            [donatello.ttl :as ttl :refer [serialize]])
  (:import [java.net URL URI]
           [java.util Date]
           [java.time Instant LocalDate]
           [java.io StringWriter]))

(deftest test-serialize
  (testing "Conversion of different types to strings"
    (is (= "5" (serialize 5)))
    (is (= "5.0" (serialize 5.0)))
    (is (= "true" (serialize true)))
    (is (= "false" (serialize false)))
    (is (= "\"test\"" (serialize "test")))
    (is (= "xsd:long" (serialize :xsd/long)))
    (is (= "<http://test.org/>" (serialize (URL. "http://test.org/"))))
    (is (= "<http://test.org/>" (serialize (URI. "http://test.org/"))))
    (is (= "\"2023-02-11T22:39:06.109Z\"^^<xsd:dateTime>" (serialize (Date. 1676155146109))))
    (is (= "\"2023-02-11T22:39:06.109Z\"^^<xsd:dateTime>" (serialize (Instant/ofEpochMilli 1676155146109))))
    (is (= "\"2023-02-11\"^^<xsd:date>" (serialize (LocalDate/of 2023 2 11))))
    (is (= "\"-26.84372,150.54195\"^^asami:geo" (serialize (ttl/typed-literal "-26.84372,150.54195" :asami/geo))))
    (is (= "\"-26.84372,150.54195\"^^<http://quoll.clojars.org/geo>"
           (serialize (ttl/typed-literal "-26.84372,150.54195" (URL. "http://quoll.clojars.org/geo")))))))

(defn write
  [f arg]
  (let [sw (StringWriter.)]
    (f sw arg)
    (str sw)))

(def default-ns "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n")
(def p1 "@prefix ns1: <http://demo.org/ns1/> .\n@prefix ns2: <http://ex.com/ns2#> .\n")

(deftest test-prefixes
  (testing "Writing a prefix map"
    (is (= (str p1 default-ns \newline)
           (write ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                       :ns2 "http://ex.com/ns2#"})))
    (is (= (str p1 \newline)
           (binding [ttl/*include-defaults* false]
             (write ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                         :ns2 "http://ex.com/ns2#"}))))))



