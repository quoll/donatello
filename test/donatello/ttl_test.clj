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
           (serialize (ttl/typed-literal "-26.84372,150.54195" (URL. "http://quoll.clojars.org/geo")))))
    (is (= "\"chat\"@fr"
           (serialize (ttl/lang-literal "chat" "fr"))))
    ))

(deftest test-camel-case
  (testing "Converting ascii text strings to CamelCase"
    (is (= "HelloWorld" (ttl/camel-case "hello world")))
    (is (= "HelloWorld" (ttl/camel-case "hello, world!")))
    (is (= "XMg01mgMg" (ttl/camel-case "[_X/mg]; [0.1mg/mg].")))
    (is (= "goodbyeCruelWorld"
           (ttl/lower-camel-case "Goodbye, cruel world! \ud83d\ude29"))))) ;; The "Weary" face emoticon

(defn write
  [f & args]
  (let [sw (StringWriter.)]
    (apply f sw args)
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


(deftest test-triples
  (testing "Writing subject with property/objects"
    ;; NOTE: the following depends on the ordering of hashsets
    (is (= "ns:_inst :p1 \"data a\", \"data b\";\n         <http://ex.com/> 5.\n\n"
           (write #(ttl/write-triples! %1 :ns/_inst %2) {:p1 #{"data a" "data b"}
                                                         (URI. "http://ex.com/") 5})))))

(deftest test-single-triple
  (testing "Writing a single triple"
    (is (= "<http://ex.com/_1> <http://ex.com/p> \"test\".\n"
           (write #(ttl/write-triple! % (URI. "http://ex.com/_1") (URI. "http://ex.com/p") "test"))))
    (is (= "ex:_1 <http://ex.com/p> 5.0.\n"
           (write #(ttl/write-triple! % :ex/_1 (URI. "http://ex.com/p") 5.0))))))

(deftest test-triples-map
  (testing "Writing a nested map"
    (is (= (str "ns:_inst1 :p1 \"data a\", \"data b\";\n"
                "          <http://ex.com/> 5.\n\n"
                "d:data :p2 d:value;\n"
                "       :p3 4, 5;\n"
                "       d:p x:y.\n\n")
           (write ttl/write-triples-map! {:ns/_inst1 {:p1 #{"data a" "data b"}
                                                      (URI. "http://ex.com/") 5}
                                          :d/data {:p2 :d/value
                                                   :p3 #{4 5}
                                                   :d/p :x/y}})))))
