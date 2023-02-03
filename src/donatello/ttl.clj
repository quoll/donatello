(ns donatello.ttl
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.net URL URI]
           [java.util Date]
           [java.time Instant]
           [java.time.format DateTimeFormatter]))

(def obj-limit 5)

(defmulti serialize "Converts a simple datatype into a Turtle representation" class)
(defmethod serialize Long [v] (str v))
(defmethod serialize Double [v] (str v))
(defmethod serialize String [v] (str \" (s/replace v "\"" "\\\"") \"))
(defmethod serialize URI [v] (str "<" v ">"))
(defmethod serialize URL [v] (str "<" v ">"))
(defmethod serialize Date [v] (str \" (.format DateTimeFormatter/ISO_INSTANT (.toInstant v)) "\"^^<xsd:dateTime>"))
(defmethod serialize Instant [v] (str \" (.format DateTimeFormatter/ISO_INSTANT v) "\"^^<xsd:dateTime>"))

(defmethod serialize clojure.lang.Keyword
  [v]
  (if-let [lns (namespace v)]
    (str lns ":" (name v))
    (if (= v :a) "a" (str ":" (name v)))))

(defn write-prefixes!
  "Writes a prefix map to the provided output stream.
   out: The output stream to write to.
   mp: a map where keys are either strings or keywords for a localname,
       and values are strings containing the full namespace."
  [out mp]
  (doseq [[l p] mp]
    (.write out "@prefix ")
    (.write out (name l))
    (.write out ": <")
    (.write out (str p))
    (.write out "> .\n"))
  (.write out "\n"))

(defn write-triples!
  "Writes the triples for a single subject, as a group.
   The subject is provided as a single element, and the property/values are provided as a map.
   The map contains predicates as keys, and objects as values. If a value is a collection
   then this will be emitted as multiple values with the same property.
   NOTE: Does not yet support blank nodes. TODO.
   out: The output stream to write to.
   subj: The subject to write triples for.
   property-map: A map of properties to values, or collections of values."
  [out subj property-map]
  (let [s (serialize subj)
        newline-indent (apply str "\n" (repeat (inc (count s)) \space))
        sp (str ";" newline-indent)
        write-po! (fn [p o]
                    (let [pred (serialize p)]
                      (.write out pred)
                      (.write out " ")
                      (if (coll? o)
                        (let [[[f] & r] (map vector o (range))
                              indent (apply str newline-indent (repeat (inc (count pred)) \space))]
                          (.write out (serialize f))
                          (doseq [[o1 n] r]
                            (.write out ", ")
                            (when (zero? (mod n obj-limit))
                              (.write out indent))
                            (.write out (serialize o1))))
                        (.write out (serialize o)))))]
    (.write out s)
    (.write out " ")
    (let [[[p o] & props] property-map]
      (write-po! p o)
      (doseq [[p o] props]
        (.write out sp)
        (write-po! p o))))
  (.write out ".\n\n"))

(defn write-triples-map!
  "Writes to a stream a nested map of subjects to maps of predicates to objects.
   Objects may be individuals or collections.
   out: The output stream to write to.
   mp: A map of subjects to property/value maps."
  [out mp]
  (doseq [[subj props] mp]
    (write-triples! out subj props)))

