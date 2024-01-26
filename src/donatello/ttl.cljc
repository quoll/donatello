(ns donatello.ttl
  (:require [clojure.string :as s]
            [tiara.data :refer [ordered-map EMPTY_MAP]]
            [quoll.rdf :as rdf :refer [lang-literal typed-literal iri common-prefixes]]
            #?(:clj [clojure.java.io :as io]))
  #?(:clj
     (:import [java.io Writer StringWriter]
              [java.net URL URI]
              [java.util Date]
              [java.time Instant LocalDate]
              [java.time.format DateTimeFormatter]
              [clojure.lang Keyword])
     :cljs
     (:import [goog.string StringBuffer]
              [goog Uri])))

;; #?(:clj (set! *warn-on-reflection* true))

;; The maximum number of items from a list to print on a single line
(def ^:dynamic *list-limit* 5)

;; The maximum number of structured items from a list to print on a single line
(def ^:dynamic *object-list-limit* 5)

;; The maximum column width to print to in lists.
;; This only applies to scalar values, not structures
(def ^:dynamic *soft-max-width* 120)

;; The maximum number of elements to include in an object before
;; it will be split over multiple lines
(def ^:dynamic *embedded-limit* 2)

;; Whether or not to include the `default-prefixes` in an output document
(def ^:dynamic *include-defaults* true)

;; Indicates if the last object written needs a space before a line terminator
(def ^:dynamic *space-flag* nil)

;; Whether to include extra whitespace to correct for the Neptune parser that not conforming to the spec
(def ^:dynamic *neptune* false)

(def default-prefixes
  (ordered-map
   :rdf (common-prefixes :rdf)
   :rdfs (common-prefixes :rdfs)
   :xsd (common-prefixes :xsd)))

(def ^:dynamic *context-base* nil)
(def ^:dynamic *context-prefixes* {}) 

(def skip-iri-chars #"[^0-9a-zA-Z]")
(def bad-iri-chars #"['*]")

(defn camel-case
  "Converts a string into a CamelCase variation"
  [s]
  (let [parts (-> s
                  (s/replace bad-iri-chars "")
                  (s/replace skip-iri-chars " ")
                  (s/split #" +"))]
    (apply str (map s/capitalize parts))))

(defn lower-camel-case
  "Converts a string into a camelCase variation that starts with the lower-case character."
  [s]
  (let [[fpart & rparts] (-> s
                             (s/replace bad-iri-chars "")
                             (s/replace skip-iri-chars " ")
                             (s/split #" +"))]
    (apply str (s/lower-case fpart) (map s/capitalize rparts))))

#?(:clj
   (def complex?
     "A defined set of the built-in collection types in Clojure. This is a hack to avoid reflection."
     #{clojure.lang.PersistentList$EmptyList
         clojure.lang.PersistentList
         clojure.lang.Cons
         clojure.lang.LazySeq
         clojure.lang.PersistentVector
         clojure.lang.PersistentHashSet
         clojure.lang.PersistentTreeSet
         clojure.lang.PersistentArrayMap
         clojure.lang.PersistentHashMap}))

(defn scalar?
  "Is an object scalar or complex?"
  [s]
  (not #?(:clj (complex? (type s))
          :cljs (and (coll? s)
                     (not (instance? rdf/BlankNode s))
                     (not (instance? rdf/TypedLiteral s))
                     (not (instance? rdf/LangLiteral s))))))

(defn scalar-seq?
  "Tests if a seq contains elements that are scalar and not complex"
  [s]
  (not #?(:clj (some complex? (map class s))
          :cljs (some coll? s))))

(defn simple-seq?
  "Tests if a seq contains elements that are scalar or short embeded objects"
  [s]
  (every? #(or (scalar? %)
               (and (map? %)
                    (<= (count %) *embedded-limit*)
                    (every? scalar? (vals %)))) s))

(defn uri-output
  [u abs?]
  (let [s (str u)]
    (cond
      (and abs? *context-base* (s/starts-with? s *context-base*))
      (str \< (subs s (count *context-base*)) \>)

      (and abs? (or *context-prefixes* *include-defaults*))
      (if-let [[k v] (first (filter (fn [[_ v]] (and (s/starts-with? s v)
                                                     (nil? (s/index-of s \/ #?(:clj (.length ^String v)
                                                                               :cljs (count v))))))
                                    (if *include-defaults*
                                      (into default-prefixes *context-prefixes*)
                                      *context-prefixes*)))]
        (str (name k) \: (subs s (count v)))
        (str \<  s \>))

      :default (str \< s \>))))

(defprotocol Serializable
    (serialize [v] "Serializes an object or value to its TTL string representation"))

#?(:clj
   (extend-protocol Serializable
     Object
     (serialize [v] (str v))

     String
     (serialize [v] (str \" (rdf/print-escape v) \"))

     URI 
     (serialize [v] (uri-output v (.isAbsolute ^URI v)))

     URL 
     (serialize [v] (uri-output v true))

     Date 
     (serialize [v] (str \" (.format DateTimeFormatter/ISO_INSTANT (.toInstant v)) "\"^^xsd:dateTime"))

     Instant 
     (serialize [v] (str \" (.format DateTimeFormatter/ISO_INSTANT v) "\"^^xsd:dateTime"))

     LocalDate 
     (serialize [v] (str \" (.format DateTimeFormatter/ISO_DATE v) "\"^^xsd:date"))

     Keyword
     (serialize 
       [v]
       (if-let [lns (namespace v)]
         (str lns ":" (name v))
         (if (= v :a) "a" (str ":" (name v))))))

   :cljs
   (extend-protocol Serializable
     object
     (serialize [v] (str v))

     string
     (serialize [v] (str \" (rdf/print-escape v) \"))

     Uri 
     (serialize [v] (uri-output v (boolean (seq (.getDomain v)))))

     js/Date 
     (serialize [v] (str (typed-literal v)))

     Inst 
     (serialize [v] (str (typed-literal v)))

     Keyword
     (serialize 
       [v]
       (if-let [lns (namespace v)]
         (str lns ":" (name v))
         (if (= v :a) "a" (str ":" (name v)))))))

#?(:clj (defn -write [^Writer out ^String value] (.write out value)))

#?(:clj (defn -write-char [^Writer out value] (.write out (int value)))
   :cljs (defn -write-char [out value] (-write out value)))

(defn write-base!
  "Writes a base to the provided output stream.
   out: The output stream to write to.
   base: The base URI as a string."
  [out base]
  (-write out "@base <")
  (-write out (str base))
  (-write out "> .\n"))

(defn mixed-key-into
  [dest src]
  (reduce (fn [m [k v]] (if (nil? (or (get m k) (get m (name k)))) (assoc m k v) m))
          dest src))

(defn write-prefixes!
  "Writes a prefix map to the provided output stream.
   out: The output stream to write to.
   mp: a map where keys are either strings or keywords for a localname,
       and values are strings containing the full namespace. Optional."
  ([out] (when *include-defaults* (write-prefixes! out {})))
  ([out mp]
   (let [mpx (if *include-defaults*
               (mixed-key-into
                 ;; The following is similar to `select-keys` but removes the keys found in mp
                 (reduce (fn [m [k v]]
                           (if (nil? (or (get mp k) (get mp (name k)))) (assoc m k v) m))
                         EMPTY_MAP default-prefixes)
                 mp)
               mp)]
     (doseq [[l p] mpx]
       (-write out "@prefix ")
       (-write out (name l))
       (-write out ": <")
       (-write out (str p))
       (-write out "> .\n")))
   (-write-char out \newline)))


(declare write-entity! write-po! write-blank-object!)

(defn- check-object!
  "Checks if a written object needs a space before a line terminator"
  [o]
  (vreset! *space-flag* (or (int? o)
                            (and *neptune* (boolean? o)))))

(defn- clear-space-flag!
  "Clears the last object space flag"
  []
  (vreset! *space-flag* false))

(defn- write-list!
  "Writes a sequence as an rdf:List object.
   Returns the width of the final line."
  [out lst indent]
  (-write-char out \()
  (if-not (seq lst)
    (do
      (-write-char out \))
      (+ indent 2))
    (let [width (if (scalar-seq? lst) *list-limit* 1)
          indent (inc indent)
          next-line (apply str \newline (repeat indent \space))
          first-width (write-entity! out (first lst))]
      (loop [[e & r] (rest lst) line-elt 1 last-width (+ indent first-width)]
        (if e
          (letfn [(write-spacing! [newline-test]
                    (if newline-test
                      (do (-write out next-line)
                          [indent 1])
                      (do (-write-char out \space)
                          [(inc last-width) (inc line-elt)])))]
            (if (scalar? e)
              (let [s (serialize e)
                    sl (count s)
                    [in next-elt] (write-spacing!
                                   (or (>= line-elt width)
                                       (and (> line-elt 0)
                                            (> (+ last-width sl 1) *soft-max-width*))))]
                (-write out s)
                (recur r (long next-elt) (+ in sl)))
              (let [[in next-elt] (write-spacing! (= line-elt width))]
                (recur r (long next-elt) (write-entity! out e in)))))
          (do
            (-write-char out \))
            (inc last-width)))))))

(defn- write-short-anon!
  "Writes a short anonymous object. The first [ character has already been written.
   Returns the length of the written text incremented to include the previously written [ character."
  [out obj indent]
  (let [[[p o] & r] obj
        pred (serialize p)]
    (-write out pred)
    (-write-char out \space)
    (let [ob-len (if (map? o)
                   (write-blank-object! out o 0)
                   (let [ob (serialize o)]
                     (-write out ob)
                     (count ob)))
          n (+ indent 2 (count pred) ob-len)]
      (loop [[[np no :as npo] & nr] r ind n]
        (if npo
          (let [nps (serialize np)]
            (-write out "; ")
            (-write out nps)
            (-write-char out \space)
            (let [nos-len (if (map? no)
                            (write-blank-object! out no 0)
                            (let [nos (serialize no)]
                              (-write out nos)
                              (count nos)))]
              (recur nr (+ ind 3 (count nps) nos-len))))
          (do
            (-write-char out \])
            (inc ind)))))))

(defn- write-blank-object!
  "Writes a blank node in square brackets.
   Short embedded blank node objects will be serialized inline without using newlines.
   Returns the indent from the final line"
  [out obj indent]
  (-write-char out \[)
  (cond
    ;; Short circuit empty nodes
    (empty? obj)
    (do
      (-write-char out \])
      (+ indent 2))

    ;; small nodes will be inlined without newline characters
    (simple-seq? (vals obj)) (write-short-anon! out obj indent)

    :default
    (let [indent (inc indent)
          sp (apply str ";\n" (repeat indent \space))
          [[p o] & props] obj
          line-width (write-po! out p o indent)]
      (loop [[[p o :as po] & r] props last-width line-width]
        (if po
          (do
            (-write out sp)
            (recur r (write-po! out p o indent)))
          (do
            (-write-char out \])
            (inc last-width)))))))

(defn- write-entity!
  "Writes an entity to the output stream. Returns the width of the final line.
   out: The output stream to write to.
   subj: The subject to seralize and write.
   indent: The initial indent width to use."
  ([out subj] (write-entity! out subj 0))
  ([out subj indent]
   (cond
     (scalar? subj) (let [s (serialize subj)]
                      (-write out s)
                      (+ indent (count s)))
     (sequential? subj) (write-list! out subj indent)
     (map? subj) (write-blank-object! out subj indent)
     :default (throw (ex-info (str "Unexpected data type for subject: " (type subj)) {:subject subj})))))

(defn- write-po!
  "Writes a predicate/object(s) pairing.
   Returns the indent of the final line."
  [out p o ind]
  (let [pred (serialize p)
        pwidth (count pred)]
    (-write out pred)
    (-write-char out \space)
    (if (set? o)
      (let [[f & r] o 
            pwidth (count pred)
            indent (+ ind pwidth 1)
            indent-str (apply str \newline (repeat indent \space))
            in (write-entity! out f indent)]
        (loop [[o1 & r1] r ocount 1 last-indent in last-obj? (not (scalar? f))]
          (letfn [(write-spacing! [newline-test]
                    (if newline-test
                      (do (-write out indent-str)
                          [indent 1])
                      (do (-write-char out \space)
                          [(+ 2 last-indent) (inc ocount)])))]
            (if o1
              (do
                (-write-char out \,)
                (if (scalar? o1)
                  (let [s (serialize o1)
                        sl (count s)
                        [n newcount] (write-spacing!
                                      (or (>= ocount *list-limit*)
                                          (> (+ last-indent sl 2) *soft-max-width*)))]
                    (-write out s)
                    (check-object! o1)
                    (recur r1 (long newcount) (+ n sl) false))
                  (let [[n newcount] (write-spacing! (or (>= ocount *list-limit*)
                                                         (and last-obj?
                                                              (>= ocount *object-list-limit*))))]
                    (clear-space-flag!)
                    (recur r1 (long newcount) (write-entity! out o1 n) true))))
              last-indent))))
      (do
        (check-object! o)
        (write-entity! out o (+ ind pwidth 1))))))

(defn write-triples!
  "Writes the triples for a single subject, as a group.
   The subject is provided as a single element, and the property/values are provided as a map.
   The map contains predicates as keys, and objects as values. If a value is a collection
   then this will be emitted as multiple values with the same property.
   out: The output stream to write to.
   subj: The subject to write triples for.
   property-map: A map of properties to values, or collections of values."
  [out subj property-map]
  (binding [*space-flag* (volatile! false)]
    (let [w (write-entity! out subj)
          newline-indent (apply str \newline (repeat (inc w) \space))
          sp (str ";" newline-indent)]
      (-write-char out \space)
      (let [indent (inc w)
            [[p o] & props] property-map]
        (write-po! out p o indent)
        (doseq [[p o] props]
          (-write out sp)
          (clear-space-flag!)
          (write-po! out p o indent))))
    (when @*space-flag*
      (-write-char out \space)
      (clear-space-flag!)))
  (-write out ".\n\n"))

(defn write-triple!
  "Writes a single triple to the output stream.
   out: The output stream to write to.
   subj: The subject of the triple.
   pred: The predicate of the triple.
   obj: The object of the triple."
  [out subj pred obj]
  (binding [*space-flag* (volatile! false)]
    (let [w (write-entity! out subj)
          p (serialize pred)
          w2 (+ 2 (count p) w)]
      (-write-char out \space)
      (-write out p)
      (-write-char out \space)
      (write-entity! out obj w2)
      (check-object! obj)
      (when @*space-flag*
        (-write-char out \space))
      (-write out ".\n"))))

(defn write-triples-map!
  "Writes to a stream a nested map of subjects to maps of predicates to objects.
   Objects may be individuals or collections.
   out: The output stream to write to.
   mp: A map of subjects to property/value maps."
  [out mp]
  (doseq [[subj props] mp]
    (write-triples! out subj props)))

(defn write-object!
  "Writes a single anonymous object to the output stream.
   out: The object stream to write to.
   e: The entity to write"
  [out e]
  (binding [*space-flag* (volatile! false)]
    (write-entity! out e)
    (when @*space-flag*
      (-write-char out \space)
      (clear-space-flag!)))
  (-write out ".\n\n"))

#?(:clj
   (defn to-string
     [f & args]
     (let [sw (StringWriter.)]
       (apply f sw args)
       (str sw)))

   :cljs
   (defn to-string
     [f & args]
     (let [sb (StringBuffer.)
           sw (StringBufferWriter. sb)]
       (apply f sw args)
       (str sb))))
