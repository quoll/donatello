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
    (is (= "\"test \\\\ \\\"escape\\\"\"" (serialize "test \\ \"escape\"")))
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
    (is (re-find #"^_:b[0-9]+$" (serialize (ttl/blank-node))))
    (is (re-find #"^_:b[0-9]+$" (serialize (ttl/blank-node "x"))))))

(deftest test-context-serialize
  (testing "Converting URLs and URIs to strings within a context"
    (binding [ttl/*context-prefixes* {:ex "http://ex.com/"
                                      :t "http://test.org/"}]
      (is (= "t:foo" (serialize (URL. "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (URL. "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (URL. "http://example.com/bar"))))
      (is (= "t:foo" (serialize (URI. "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (URI. "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (URI. "http://example.com/bar"))))
      (is (= "rdf:type"
             (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (binding [ttl/*include-defaults* false]
        (is (= "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))

    (binding [ttl/*context-prefixes* {"ex" "http://ex.com/"
                                      "t" "http://test.org/"}]
      (is (= "t:foo" (serialize (URL. "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (URL. "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (URL. "http://example.com/bar"))))
      (is (= "t:foo" (serialize (URI. "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (URI. "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (URI. "http://example.com/bar"))))
      (is (= "rdf:type"
             (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (binding [ttl/*include-defaults* false]
        (is (= "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))

    (is (= "rdf:type" (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))

    (binding [ttl/*context-prefixes* (assoc ttl/*context-prefixes* :ex "http://ex.com/")]
      (is (= "rdf:type" (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (is (= "<http://test.org/foo>" (serialize (URL. "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (URL. "http://ex.com/bar")))))
    (binding [ttl/*context-prefixes* nil]
      (is (= "rdf:type" (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (binding [ttl/*include-defaults* false]
        (is (= "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               (serialize (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))))

(deftest test-camel-case
  (testing "Converting ascii text strings to CamelCase"
    (is (= "HelloWorld" (ttl/camel-case "hello world")))
    (is (= "HelloWorld" (ttl/camel-case "hello, world!")))
    (is (= "XMg01mgMg" (ttl/camel-case "[_X/mg]; [0.1mg/mg].")))
    (is (= "goodbyeCruelWorld"
           (ttl/lower-camel-case "Goodbye, cruel world! \ud83d\ude29"))))) ;; The "Weary" face emoticon

(deftest test-blank
  (testing "If blank nodes are consistently different or the same when requested"
    (is (= (ttl/blank-node "x") (ttl/blank-node "x")))
    (is (not= (ttl/blank-node "x") (ttl/blank-node "y")))
    (is (not= (ttl/blank-node) (ttl/blank-node)))))

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
    (let [[s0 w0] (write #'ttl/write-blank-object! {} 0)
          [s1 w1] (write #'ttl/write-blank-object!
                         {:p1 5, :a/p2 #{"t1" "t2"}, (URI. "http://ugh.com/") 11} 0)
          [s2 w2] (write #'ttl/write-blank-object!
                         {:p1 5, :a/p2 #{"t1" "t2"}, (URI. "http://ugh.com/") 11} 3)
          [s3 w3] (write #'ttl/write-blank-object!
                         {:p1 {}, :a/p2 #{{:x/y 4} "t2"}, :a/c 11} 0)
          [s4 w4] (write #'ttl/write-blank-object!
                         {:p1 {}, :a/p2 #{{:x/y 4} {:x/y 5 :x/z 6}}, :a/c 11} 0)]
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

(deftest object
  (testing "Writing single object"
    (is (= ["[a data:Number; rdf:value 5].\n\n" nil]
           (write #'ttl/write-object! {:a :data/Number, :rdf/value 5})))))

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

(deftest test-list
  (testing "Test the list output"
    (let [[s0 w0] (write #'ttl/write-list! '(1 2 3) 0)
          [s1 w1] (write #'ttl/write-list! '() 0)
          [s2 w2] (write #'ttl/write-list! '({:b 1} {:c "x"}) 0)
          [s3 w3] (write #'ttl/write-list! '((1 2) ("x" "y")) 0)
          [s4 w4] (write #'ttl/write-list! '(1 2 3 4 5 6) 0)
          [s5 w5] (write #'ttl/write-list! '(1 2 3) 2)
          [s6 w6] (write #'ttl/write-list! '() 2)
          [s7 w7] (write #'ttl/write-list! '({:b 1} {:c "x"}) 2)
          [s8 w8] (write #'ttl/write-list! '((1 2) ("x" "y")) 2)
          [s9 w9] (write #'ttl/write-list! '(1 2 3 4 5 6) 2)]
      (is (= "(1 2 3)" s0))
      (is (= 7 w0))
      (is (= "()" s1))
      (is (= 2 w1))
      (is (= "([:b 1]\n [:c \"x\"])" s2))
      (is (= 10 w2))
      (is (= "((1 2)\n (\"x\" \"y\"))" s3))
      (is (= 11 w3))
      (is (= "(1 2 3 4 5\n 6)" s4))
      (is (= 3 w4))
      (is (= "(1 2 3)" s5))
      (is (= 9 w5))
      (is (= "()" s6))
      (is (= 4 w6))
      (is (= "([:b 1]\n   [:c \"x\"])" s7))
      (is (= 12 w7))
      (is (= "((1 2)\n   (\"x\" \"y\"))" s8))
      (is (= 13 w8))
      (is (= "(1 2 3 4 5\n   6)" s9))
      (is (= 5 w9)))))

(deftest test-short-anon
  (testing "Test the short anonymous object"
    (let [[s0 w0] (write #'ttl/write-short-anon! {:x 1} 0)
          [s1 w1] (write #'ttl/write-short-anon! {:a/x 1 :a/y "2"} 0)
          [s2 w2] (write #'ttl/write-short-anon! {:a/x 1 :a/y "2" :a/z :b/b} 0)
          [s3 w3] (write #'ttl/write-short-anon! {:x 1} 2)
          [s4 w4] (write #'ttl/write-short-anon! {:a/x 1 :a/y "2"} 2)
          [s5 w5] (write #'ttl/write-short-anon! {:a/x 1 :a/y "2" :a/z :b/b} 2)]
      (is (= ":x 1]" s0))
      (is (= 6 w0))
      (is (= "a:x 1; a:y \"2\"]" s1))
      (is (= 16 w1))
      (is (= "a:x 1; a:y \"2\"; a:z b:b]" s2))
      (is (= 25 w2))
      (is (= ":x 1]" s3))
      (is (= 8 w3))
      (is (= "a:x 1; a:y \"2\"]" s4))
      (is (= 18 w4))
      (is (= "a:x 1; a:y \"2\"; a:z b:b]" s5))
      (is (= 27 w5)))))

(def default-ns "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n")
(def p1 "@prefix ns1: <http://demo.org/ns1/> .\n@prefix ns2: <http://ex.com/ns2#> .\n")

(deftest test-base
  (testing "Writing a base"
    (is (= "@base <http://local.net/> .\n"
           (fwrite ttl/write-base! "http://local.net/"))))
  (testing "Outputting with a base context"
    (binding [ttl/*context-base* "http://local.net/"]
      (is (= "<foo>" (ttl/serialize (URI. "http://local.net/foo"))))
      (is (= "<foo>" (ttl/serialize (URL. "http://local.net/foo")))))))

(deftest test-prefixes
  (testing "Writing a prefix map"
    (is (= (str default-ns p1 \newline)
           (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                       :ns2 "http://ex.com/ns2#"})))
    (is (= (str p1 \newline)
           (binding [ttl/*include-defaults* false]
             (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                         :ns2 "http://ex.com/ns2#"}))))))

(deftest test-single-triple
  (testing "Writing a single triple"
    (is (= "<http://ex.com/_1> <http://ex.com/p> \"test\".\n"
           (fwrite ttl/write-triple! (URI. "http://ex.com/_1") (URI. "http://ex.com/p") "test")))
    (is (= "ex:_1 <http://ex.com/p> 5.0.\n"
           (fwrite ttl/write-triple! :ex/_1 (URI. "http://ex.com/p") 5.0)))
    (is (= "[a data:Class; b:data data:_123] data:rel [a data:Class; b:data data:_246].\n"
           (fwrite ttl/write-triple!
                   {:a :data/Class
                    :b/data :data/_123}
                   :data/rel
                   {:a :data/Class
                    :b/data :data/_246})))
    (is (= (str "[a data:Class;\n"
                " b:data data:_123;\n"
                " b:more [a data:Inner;\n"
                "         b:list (1 2 3)]] data:rel [:p1 \"data a\", \"data b\";\n"
                "                                    <http://ex.com/> 5].\n")
           (fwrite ttl/write-triple!
                   {:a :data/Class
                    :b/data :data/_123
                    :b/more {:a :data/Inner :b/list [1 2 3]}}
                   :data/rel
                   {:p1 #{"data a" "data b"}
                    (URI. "http://ex.com/") 5})))
    (is (= (str "(\"one\" \"two\" \"three\" \"four\" \"five\"\n"
                " \"six\") data:rel [:p1 \"data a\", \"data b\";\n"
                "                  <http://ex.com/> 5].\n")
           (fwrite ttl/write-triple!
                   ["one" "two" "three" "four" "five" "six"]
                   :data/rel
                   {:p1 #{"data a" "data b"}
                    (URI. "http://ex.com/") 5})))
    ))

(deftest test-triples
  (testing "Writing subject with property/objects"
    ;; NOTE: the following depends on the ordering of hashsets
    (is (= "ns:_inst :p1 \"data a\", \"data b\";\n         <http://ex.com/> 5.\n\n"
           (fwrite ttl/write-triples! :ns/_inst {:p1 #{"data a" "data b"}
                                                 (URI. "http://ex.com/") 5})))
    (is (= (str "ns:_inst :p1 \"data a\", \"data b\";\n"
                "         <http://ex.com/> 7, 1, 4, 6, 3,\n"
                "                          2, 11, 9, 5, 10,\n"
                "                          8.\n\n")
           (fwrite ttl/write-triples! :ns/_inst {:p1 #{"data a" "data b"}
                                                 (URI. "http://ex.com/") #{1 2 3 4 5 6 7 8 9 10 11}})))
    (is (= (str "[a data:Class; b:data data:_123] :p1 \"data a\", \"data b\";\n"
                "                                 <http://ex.com/> 5.\n\n")
           (fwrite ttl/write-triples!
                   {:a :data/Class
                    :b/data :data/_123}
                   {:p1 #{"data a" "data b"}
                    (URI. "http://ex.com/") 5})))
    (is (= (str "[a data:Class;\n"
                " b:data data:_123;\n"
                " b:more [a data:Inner;\n"
                "         b:list (1 2 3)]] :p1 \"data a\", \"data b\";\n"
                "                          <http://ex.com/> 5.\n\n")
           (fwrite ttl/write-triples!
                   {:a :data/Class
                    :b/data :data/_123
                    :b/more {:a :data/Inner :b/list [1 2 3]}}
                   {:p1 #{"data a" "data b"}
                    (URI. "http://ex.com/") 5})))
    (is (= "[a data:Class; b:data data:_123] data:rel [a data:Class; b:data data:_246].\n\n"
           (fwrite ttl/write-triples!
                   {:a :data/Class
                    :b/data :data/_123}
                   {:data/rel {:a :data/Class
                               :b/data :data/_246}})))))

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
