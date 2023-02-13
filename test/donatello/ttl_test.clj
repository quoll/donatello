(ns donatello.ttl-test
  (:require [clojure.test :refer [testing is deftest]]
            [donatello.ttl :as ttl :refer [serialize]])
  (:import [java.net URL URI]
           [java.util Date]
           [java.time Instant LocalDate]
           [java.io StringWriter]))

(set! *warn-on-reflection* true)

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
           (serialize (ttl/lang-literal "chat" "fr"))))))

(deftest test-camel-case
  (testing "Converting ascii text strings to CamelCase"
    (is (= "HelloWorld" (ttl/camel-case "hello world")))
    (is (= "HelloWorld" (ttl/camel-case "hello, world!")))
    (is (= "XMg01mgMg" (ttl/camel-case "[_X/mg]; [0.1mg/mg].")))
    (is (= "goodbyeCruelWorld"
           (ttl/lower-camel-case "Goodbye, cruel world! \ud83d\ude29"))))) ;; The "Weary" face emoticon

(defn write
  [f & args]
  (let [sw (StringWriter.)
        n (apply f sw args)]
    [(str sw) n]))

(defn fwrite
  [f & args]
  (first (apply write f args)))

(deftest blank-object
  (testing "Writes out an anonymous object"
    (let [[s0 w0] (write #(#'ttl/write-blank-object! %1 %2 %3) {} 0)
          [s1 w1] (write #(#'ttl/write-blank-object! %1 %2 %3)
                         {:p1 5, :a/p2 #{"t1" "t2"}, (URI. "http://ugh.com/") 11} 0)
          [s2 w2] (write #(#'ttl/write-blank-object! %1 %2 %3)
                         {:p1 5, :a/p2 #{"t1" "t2"}, (URI. "http://ugh.com/") 11} 3)
          [s3 w3] (write #(#'ttl/write-blank-object! %1 %2 %3)
                         {:p1 {}, :a/p2 #{{:x/y 4} "t2"}, :a/c 11} 0)
          [s4 w4] (write #(#'ttl/write-blank-object! %1 %2 %3)
                         {:p1 {}, :a/p2 #{{:x/y 4} {:x/y 5 :x/z 6}}, :a/c 11} 0)
          ]
      (is (= "[]" s0))
      (is (= 2 w0))
      (is (= "[:p1 5;\n a:p2 \"t1\", \"t2\";\n <http://ugh.com/> 11]" s1))
      (is (= 22 w1))
      (is (= "[:p1 5;\n    a:p2 \"t1\", \"t2\";\n    <http://ugh.com/> 11]" s2))
      (is (= 25 w2))
      (is (= "[:p1 [];\n a:p2 [x:y 4], \"t2\";\n a:c 11]" s3))
      (is (= 8 w3))
      (is (= "[:p1 [];\n a:p2 [x:y 5; x:z 6], [x:y 4];\n a:c 11]" s4))
      (is (= 8 w4)))))

(deftest entity
  (testing "Writing entities"
    (is (= ["5" 1] (write #'ttl/write-entity! 5)))
    (is (= ["5.0" 3] (write #'ttl/write-entity! 5.0)))
    (is (= ["(:ex \"a\" 5)" 11] (write #'ttl/write-entity! [:ex "a" 5])))))

(deftest test-po
  (testing "Internal method of predicate/object(s) pairs"
    (let [[s0 w0] (write #(#'ttl/write-po! %1 :p1 %2 %3) #{"data a" "data b"} 0)
          [s1 w1] (write #(#'ttl/write-po! %1 :p1 %2 %3) #{"data a" "data b"} 3)
          [s2 w2] (write #(#'ttl/write-po! %1 (URI. "http://ex.com/") %2 %3)
                         #{1 2 3 4 5 6 7 8 9 10 11} 0)
          [s3 w3] (write #(#'ttl/write-po! %1 (URI. "http://ex.com/") %2 %3)
                         #{1 2 3 4 5 6 7 8 9 10 11} 3)
          [s4 w4] (write #(#'ttl/write-po! %1 (URI. "http://ex.com/") %2 %3)
                         '(1 2 3 4 5 6 7 8 9) 0)
          [s5 w5] (write #(#'ttl/write-po! %1 (URI. "http://ex.com/") %2 %3)
                         [1 2 3 4 5 6 7 8 9] 3)
          [s6 w6] (write #(#'ttl/write-po! %1 :p1 %2 %3) ["a" "b"] 3)]
      (is (= ":p1 \"data a\", \"data b\"" s0))
      (is (= 22 w0))
      (is (= ":p1 \"data a\", \"data b\"" s1))
      (is (= 25 w1))
      (is (= (str "<http://ex.com/> 7, 1, 4, 6, 3,\n"
                  "                 2, 11, 9, 5, 10,\n"
                  "                 8") s2))
      (is (= 18 w2))
      (is (= (str "<http://ex.com/> 7, 1, 4, 6, 3,\n"
                  "                    2, 11, 9, 5, 10,\n"
                  "                    8") s3))
      (is (= 21 w3))
      (is (= (str "<http://ex.com/> (1 2 3 4 5\n"
                  "                  6 7 8 9)") s4))
      (is (= 26 w4))
      (is (= (str "<http://ex.com/> (1 2 3 4 5\n"
                  "                     6 7 8 9)") s5))
      (is (= 29 w5))
      (is (= ":p1 (\"a\" \"b\")" s6))
      (is (= 16 w6)))))

(def default-ns "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n")
(def p1 "@prefix ns1: <http://demo.org/ns1/> .\n@prefix ns2: <http://ex.com/ns2#> .\n")

(deftest test-prefixes
  (testing "Writing a prefix map"
    (is (= (str p1 default-ns \newline)
           (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                       :ns2 "http://ex.com/ns2#"})))
    (is (= (str p1 \newline)
           (binding [ttl/*include-defaults* false]
             (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                         :ns2 "http://ex.com/ns2#"}))))))

(deftest test-single-triple
  (testing "Writing a single triple"
    (is (= "<http://ex.com/_1> <http://ex.com/p> \"test\".\n"
           (fwrite #(ttl/write-triple! % (URI. "http://ex.com/_1") (URI. "http://ex.com/p") "test"))))
    (is (= "ex:_1 <http://ex.com/p> 5.0.\n"
           (fwrite #(ttl/write-triple! % :ex/_1 (URI. "http://ex.com/p") 5.0))))))

(deftest test-triples
  (testing "Writing subject with property/objects"
    ;; NOTE: the following depends on the ordering of hashsets
    (is (= "ns:_inst :p1 \"data a\", \"data b\";\n         <http://ex.com/> 5.\n\n"
           (fwrite #(ttl/write-triples! %1 :ns/_inst %2) {:p1 #{"data a" "data b"}
                                                         (URI. "http://ex.com/") 5})))
    (is (= (str "ns:_inst :p1 \"data a\", \"data b\";\n"
                "         <http://ex.com/> 7, 1, 4, 6, 3,\n"
                "                          2, 11, 9, 5, 10,\n"
                "                          8.\n\n")
           (fwrite #(ttl/write-triples! %1 :ns/_inst %2) {:p1 #{"data a" "data b"}
                                                         (URI. "http://ex.com/") #{1 2 3 4 5 6 7 8 9 10 11}})))))

(deftest test-triples-map
  (testing "Writing a nested map"
    (is (= (str "ns:_inst1 :p1 \"data a\", \"data b\";\n"
                "          <http://ex.com/> 5.\n\n"
                "d:data :p2 d:value;\n"
                "       :p3 4, 5;\n"
                "       d:p x:y.\n\n")
           (fwrite ttl/write-triples-map! {:ns/_inst1 {:p1 #{"data a" "data b"}
                                                      (URI. "http://ex.com/") 5}
                                          :d/data {:p2 :d/value
                                                   :p3 #{4 5}
                                                   :d/p :x/y}})))))
