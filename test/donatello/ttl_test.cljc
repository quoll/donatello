(ns donatello.ttl-test
  (:require [clojure.test :refer [testing is deftest]]
            [donatello.ttl :as ttl :refer [serialize]]
            [quoll.rdf :as rdf]
            [tiara.data :as d])
  #?(:clj
     (:import [java.net URL URI]
              [java.util Date]
              [java.time Instant LocalDate]
              [java.io StringWriter])
     :cljs
     (:import [goog.string StringBuffer]
              [goog Uri])))

#?(:clj (set! *warn-on-reflection* true))

(defn uri [u] #?(:clj (URI. u) :cljs (Uri. u)))
(defn url [u] #?(:clj (URL. u) :cljs (Uri. u)))

(deftest test-serialize
  (testing "Conversion of different types to strings"
    (is (= "5" (serialize 5)))
    #?(:clj(is (= "5.0" (serialize 5.0)))
       :cljs (is (= "5.1" (serialize 5.1))))
    (is (= "true" (serialize true)))
    (is (= "false" (serialize false)))
    (is (= "\"test\"" (serialize "test")))
    (is (= "\"test \\\\ \\\"escape\\\"\"" (serialize "test \\ \"escape\"")))
    (is (= "xsd:long" (serialize :xsd/long)))
    (is (= "<http://test.org/>" (serialize (url "http://test.org/"))))
    (is (= "<http://test.org/>" (serialize (uri "http://test.org/"))))
    #?(:clj (is (= "\"2023-02-11T22:39:06.109Z\"^^xsd:dateTime" (serialize (Date. 1676155146109))))
       :cljs (is (= "\"2023-02-11T22:39:06.109-00:00\"^^xsd:dateTime" (serialize (js/Date. 1676155146109)))))
    #?(:clj (is (= "\"2023-02-11T22:39:06.109Z\"^^xsd:dateTime" (serialize (Instant/ofEpochMilli 1676155146109)))))
    #?(:clj (is (= "\"2023-02-11\"^^xsd:date" (serialize (LocalDate/of 2023 2 11)))))
    (is (= "\"-26.84372,150.54195\"^^asami:geo" (serialize (rdf/typed-literal "-26.84372,150.54195" :asami/geo))))
    (is (= "\"-26.84372,150.54195\"^^<http://quoll.clojars.org/geo>"
           (serialize (rdf/typed-literal "-26.84372,150.54195" (url "http://quoll.clojars.org/geo")))))
    (is (= "\"chat\"@fr"
           (serialize (rdf/lang-literal "chat" "fr"))))
    (is (re-find #"^_:b[0-9]+$" (serialize (rdf/blank-node))))
    (is (re-find #"^_:b[0-9]+$" (serialize (rdf/blank-node "x"))))))

(deftest test-context-serialize
  (testing "Converting URLs and URIs to strings within a context"
    (binding [ttl/*context-prefixes* {:ex "http://ex.com/"
                                      :t "http://test.org/"}]
      (is (= "t:foo" (serialize (url "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (url "http://ex.com/bar"))))
      (is (= "<http://ex.com/bar/err>" (serialize (url "http://ex.com/bar/err"))))
      (is (= "<http://example.com/bar>" (serialize (url "http://example.com/bar"))))
      (is (= "t:foo" (serialize (uri "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (uri "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (uri "http://example.com/bar"))))
      (is (= "rdf:type"
             (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (binding [ttl/*include-defaults* false]
        (is (= "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))

    (binding [ttl/*context-prefixes* {"ex" "http://ex.com/"
                                      "t" "http://test.org/"}]
      (is (= "t:foo" (serialize (url "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (url "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (url "http://example.com/bar"))))
      (is (= "t:foo" (serialize (uri "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (uri "http://ex.com/bar"))))
      (is (= "<http://example.com/bar>" (serialize (uri "http://example.com/bar"))))
      (is (= "rdf:type"
             (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (binding [ttl/*include-defaults* false]
        (is (= "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))

    (is (= "rdf:type" (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))

    (binding [ttl/*context-prefixes* (assoc ttl/*context-prefixes* :ex "http://ex.com/")]
      (is (= "rdf:type" (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (is (= "<http://test.org/foo>" (serialize (url "http://test.org/foo"))))
      (is (= "ex:bar" (serialize (url "http://ex.com/bar")))))
    (binding [ttl/*context-prefixes* nil]
      (is (= "rdf:type" (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))
      (binding [ttl/*include-defaults* false]
        (is (= "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               (serialize (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))))))

(deftest test-camel-case
  (testing "Converting ascii text strings to CamelCase"
    (is (= "HelloWorld" (ttl/camel-case "hello world")))
    (is (= "HelloWorld" (ttl/camel-case "hello, world!")))
    (is (= "XMg01mgMg" (ttl/camel-case "[_X/mg]; [0.1mg/mg].")))
    (is (= "goodbyeCruelWorld"
           (ttl/lower-camel-case "Goodbye, cruel world! \ud83d\ude29"))))) ;; The "Weary" face emoticon

(deftest test-blank
  (testing "If blank nodes are consistently different or the same when requested"
    (is (= (rdf/blank-node "x") (rdf/blank-node "x")))
    (is (not= (rdf/blank-node "x") (rdf/blank-node "y")))
    (is (not= (rdf/blank-node) (rdf/blank-node)))))

(defn write
  [f & args]
  #?(:clj
     (let [sw (StringWriter.)
           n (apply f sw args)]
       [(str sw) n])
     :cljs
     (let [sb (StringBuffer.)
           sw (StringBufferWriter. sb)
           n (apply f sw args)]
       [(str sb) n])))

(defn fwrite
  [f & args]
  (first (apply write f args)))

(deftest blank-object
  (testing "Writes out an anonymous object"
    (binding [ttl/*space-flag* (volatile! false)]
      (let [[s0 w0] (write #'ttl/write-blank-object! {} 0)
            [s1 w1] (write #'ttl/write-blank-object!
                           {:p1 5, :a/p2 #{"t1" "t2"}, (uri "http://ugh.com/") 11} 0)
            [s2 w2] (write #'ttl/write-blank-object!
                           {:p1 5, :a/p2 #{"t1" "t2"}, (uri "http://ugh.com/") 11} 3)
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
        (is (= 8 w4))))))

(deftest entity
  (testing "Writing entities"
    (is (= ["5" 1] (write #'ttl/write-entity! 5)))
    #?(:clj (is (= ["5.0" 3] (write #'ttl/write-entity! 5.0)))
       :cljs (is (= ["5.1" 3] (write #'ttl/write-entity! 5.1))))
    (is (= ["(:ex \"a\" 5)" 11] (write #'ttl/write-entity! [:ex "a" 5])))
    (is (= ["\"-26.84372,150.54195\"^^asami:geo" 32] (write #'ttl/write-entity! (rdf/typed-literal "-26.84372,150.54195" :asami/geo))))
    (is (= ["\"-26.84372,150.54195\"^^<http://quoll.clojars.org/geo>" 53]
           (write #'ttl/write-entity! (rdf/typed-literal "-26.84372,150.54195" (url "http://quoll.clojars.org/geo")))))
    (is (= ["\"chat\"@fr" 9] (write #'ttl/write-entity! (rdf/lang-literal "chat" "fr"))))
    (is (re-find #"^_:b[0-9]+$" (first (write #'ttl/write-entity! (rdf/blank-node)))))
    (is (re-find #"^_:b[0-9]+$" (first (write #'ttl/write-entity! (rdf/blank-node "label")))))
    (is (= ["_:label" 7] (write #'ttl/write-entity! (rdf/unsafe-blank-node "label"))))))

(deftest object
  (testing "Writing single object"
    (is (= "[a data:Number; rdf:value 5].\n\n"
           (ttl/to-string #'ttl/write-object! {:a :data/Number, :rdf/value 5})))))

(defn digits [n] (apply str (take n (cycle (range 10)))))
(defn spaces [n] (apply str (repeat n \space)))

(deftest test-po
  (testing "Internal method of predicate/object(s) pairs"
    (binding [ttl/*space-flag* (volatile! false)]
      (let [[s0 w0] (write #(#'ttl/write-po! %1 :p1 %2 %3) #{"data a" "data b"} 0)
            [s1 w1] (write #(#'ttl/write-po! %1 :p1 %2 %3) #{"data a" "data b"} 3)
            [s2 w2] (write #(#'ttl/write-po! %1 (uri "http://ex.com/") %2 %3)
                           #{1 2 3 4 5 6 7 8 9 10 11} 0)
            [s3 w3] (write #(#'ttl/write-po! %1 (uri "http://ex.com/") %2 %3)
                           #{1 2 3 4 5 6 7 8 9 10 11} 3)
            [s4 w4] (write #(#'ttl/write-po! %1 (uri "http://ex.com/") %2 %3)
                           '(1 2 3 4 5 6 7 8 9) 0)
            [s5 w5] (write #(#'ttl/write-po! %1 (uri "http://ex.com/") %2 %3)
                           [1 2 3 4 5 6 7 8 9] 3)
            [s6 w6] (write #(#'ttl/write-po! %1 :p1 %2 %3) ["a" "b"] 3)
            [s7 w7] (write #(#'ttl/write-po! %1 (uri (str "http://twenty.com/" (digits 90))) %2 %3)
                           (d/ordered-set 1 2 3 4 5 6 7 8 99 10 11 12) 0)
            [s8 w8] (write #(#'ttl/write-po! %1 (uri (str "http://twenty.com/" (digits 90))) %2 %3)
                           (d/ordered-set "123456789012345" "1" "23" "45" "67" "8" "901" "234") 0)]
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
        (is (= 16 w6))
        (is (= (str "<http://twenty.com/" (digits 90) "> 1, 2, 3,\n"
                    (spaces 111) "4, 5, 6,\n"
                    (spaces 111) "7, 8, 99,\n"
                    (spaces 111) "10, 11,\n"
                    (spaces 111) "12") s7))
        (is (= 113 w7))
        (is (= (str "<http://twenty.com/" (digits 90) "> \"123456789012345\",\n"
                    (spaces 111) "\"1\", \"23\",\n"
                    (spaces 111) "\"45\",\n"
                    (spaces 111) "\"67\", \"8\",\n"
                    (spaces 111) "\"901\",\n"
                    (spaces 111) "\"234\"") s8))
        (is (= 116 w8))))))

(defn spaces [n] (apply str (repeat n \space)))

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
          [s9 w9] (write #'ttl/write-list! '(1 2 3 4 5 6) 2)
          [s10 w10] (write #'ttl/write-list! '(1 2 3 4 5 6) 116)
          [s10a w10a] (write #'ttl/write-list! '(1 2 33 4 5 6) 116)
          [s11 w11] (write #'ttl/write-list! '("one" "two" "three" "four" "five" "six") 104)]
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
      (is (= 5 w9))
      (is (= 121 w10))
      (is (= (str "(1 2\n" (spaces 117) "3 4\n" (spaces 117) "5 6)") s10))
      (is (= 119 w10a))
      (is (= (str "(1 2\n" (spaces 117) "33\n" (spaces 117) "4 5\n" (spaces 117) "6)") s10a))
      (is (= 118 w11))
      (is (= (str "(\"one\" \"two\"\n" (spaces 105) "\"three\" \"four\"\n" (spaces 105) "\"five\" \"six\")") s11)))))

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
(def updated-ns "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n@prefix ns1: <http://demo.org/ns1/> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix ns2: <http://ex.com/ns2#> .\n\n")

(deftest test-base
  (testing "Writing a base"
    (is (= "@base <http://local.net/> .\n"
           (fwrite ttl/write-base! "http://local.net/"))))
  (testing "Outputting with a base context"
    (binding [ttl/*context-base* "http://local.net/"]
      (is (= "<foo>" (ttl/serialize (uri "http://local.net/foo"))))
      (is (= "<foo>" (ttl/serialize (url "http://local.net/foo")))))))

(deftest test-prefixes
  (testing "Writing a prefix map"
    (is (= (str default-ns p1 \newline)
           (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                       :ns2 "http://ex.com/ns2#"})))
    (is (= updated-ns
           (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                        :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                                        :ns2 "http://ex.com/ns2#"})))
    (is (= updated-ns
           (fwrite ttl/write-prefixes! {"ns1" "http://demo.org/ns1/"
                                        "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                                        "ns2" "http://ex.com/ns2#"})))
    (is (= updated-ns
           (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                        "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                                        :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                                        :ns2 "http://ex.com/ns2#"})))
    (is (= updated-ns
           (fwrite ttl/write-prefixes! {"ns1" "http://demo.org/ns1/"
                                        :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                                        "ns2" "http://ex.com/ns2#"})))
    (is (= (str p1 \newline)
           (binding [ttl/*include-defaults* false]
             (fwrite ttl/write-prefixes! {:ns1 "http://demo.org/ns1/"
                                         :ns2 "http://ex.com/ns2#"}))))))

(deftest test-single-triple
  (testing "Writing a single triple"
    (is (= "<http://ex.com/_1> <http://ex.com/p> \"test\".\n"
           (fwrite ttl/write-triple! (uri "http://ex.com/_1") (uri "http://ex.com/p") "test")))
    #?(:clj (is (= "ex:_1 <http://ex.com/p> 5.0.\n"
                   (fwrite ttl/write-triple! :ex/_1 (uri "http://ex.com/p") 5.0)))
       :cljs (is (= "ex:_1 <http://ex.com/p> 5.1.\n"
              (fwrite ttl/write-triple! :ex/_1 (uri "http://ex.com/p") 5.1))))
    (is (= "ex:_1 <http://ex.com/p> 5 .\n"
           (fwrite ttl/write-triple! :ex/_1 (uri "http://ex.com/p") 5)))
    (is (= "ex:_1 <http://ex.com/p> true.\n"
           (fwrite ttl/write-triple! :ex/_1 (uri "http://ex.com/p") true)))
    (binding [ttl/*neptune* true]
      (is (= "ex:_1 <http://ex.com/p> true .\n"
             (fwrite ttl/write-triple! :ex/_1 (uri "http://ex.com/p") true))))
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
                    (uri "http://ex.com/") 5})))
    (is (= (str "(\"one\" \"two\" \"three\" \"four\" \"five\"\n"
                " \"six\") data:rel [:p1 \"data a\", \"data b\";\n"
                "                  <http://ex.com/> 5].\n")
           (fwrite ttl/write-triple!
                   ["one" "two" "three" "four" "five" "six"]
                   :data/rel
                   {:p1 #{"data a" "data b"}
                    (uri "http://ex.com/") 5})))))

(deftest test-triples
  (testing "Writing subject with property/objects"
    ;; NOTE: the following depends on the ordering of hashsets
    (is (= "ns:_inst :p1 \"data a\", \"data b\";\n         <http://ex.com/> 5 .\n\n"
           (fwrite ttl/write-triples! :ns/_inst {:p1 #{"data a" "data b"}
                                                 (uri "http://ex.com/") 5})))
    (is (= (str "ns:_inst :p1 \"data a\", \"data b\";\n"
                "         <http://ex.com/> 7, 1, 4, 6, 3,\n"
                "                          2, 11, 9, 5, 10,\n"
                "                          8 .\n\n")
           (fwrite ttl/write-triples! :ns/_inst {:p1 #{"data a" "data b"}
                                                 (uri "http://ex.com/") #{1 2 3 4 5 6 7 8 9 10 11}})))
    (is (= (str "ns:_inst :p1 \"data a\", \"data b\";\n"
                "         <http://ex.com/> 7, 1, 4,\n"
                "                          6, 3, 2,\n"
                "                          11, 9, 5,\n"
                "                          10, 8 .\n\n")
           (binding [ttl/*list-limit* 3]
             (fwrite ttl/write-triples! :ns/_inst {:p1 #{"data a" "data b"}
                                                   (uri "http://ex.com/") #{1 2 3 4 5 6 7 8 9 10 11}}))))
    (is (= (str "ns:_inst <http://ex.com/> [:v 4], [:v 1],\n"
                "                          [:v 3], [:v 5],\n"
                "                          [:v 2], [:v 6].\n\n")
           (binding [ttl/*object-list-limit* 2]
             (fwrite ttl/write-triples! :ns/_inst {(uri "http://ex.com/") #{{:v 1} {:v 2} {:v 3} {:v 4}
                                                                             {:v 5} {:v 6}}}))))
    (is (= (str "ns:_inst <http://ex.com/> [:v 4], [:v 1], [:v 3],\n"
                "                          [:v 5], [:v 2], [:v 6].\n\n")
           (binding [ttl/*object-list-limit* 3]
             (fwrite ttl/write-triples! :ns/_inst {(uri "http://ex.com/") #{{:v 1} {:v 2} {:v 3} {:v 4}
                                                                             {:v 5} {:v 6}}}))))
    (is (= (str "[a data:Class; b:data data:_123] :p1 \"data a\", \"data b\";\n"
                "                                 <http://ex.com/> 5 .\n\n")
           (fwrite ttl/write-triples!
                   {:a :data/Class
                    :b/data :data/_123}
                   {:p1 #{"data a" "data b"}
                    (uri "http://ex.com/") 5})))
    (is (= (str "[a data:Class;\n"
                " b:data data:_123;\n"
                " b:more [a data:Inner;\n"
                "         b:list (1 2 3)]] :p1 \"data a\", \"data b\";\n"
                "                          <http://ex.com/> 5 .\n\n")
           (fwrite ttl/write-triples!
                   {:a :data/Class
                    :b/data :data/_123
                    :b/more {:a :data/Inner :b/list [1 2 3]}}
                   {:p1 #{"data a" "data b"}
                    (uri "http://ex.com/") 5})))
    (is (= "[a data:Class; b:data data:_123] data:rel [a data:Class; b:data data:_246].\n\n"
           (fwrite ttl/write-triples!
                   {:a :data/Class
                    :b/data :data/_123}
                   {:data/rel {:a :data/Class
                               :b/data :data/_246}})))))

(deftest test-triples-map
  (testing "Writing a nested map"
    (is (= (str "ns:_inst1 :p1 \"data a\", \"data b\";\n"
                "          <http://ex.com/> 5 .\n\n"
                "d:data :p2 d:value;\n"
                "       :p3 4, 5;\n"
                "       d:p x:y.\n\n")
           (fwrite ttl/write-triples-map! {:ns/_inst1 {:p1 #{"data a" "data b"}
                                                      (uri "http://ex.com/") 5}
                                          :d/data {:p2 :d/value
                                                   :p3 #{4 5}
                                                   :d/p :x/y}})))))

#?(:cljs (cljs.test/run-tests))
