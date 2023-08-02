(ns donatello.ttl
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [tiara.data :refer [ordered-map EMPTY_MAP]])
  (:import [java.io Writer]
           [java.net URL URI]
           [java.util Date]
           [java.time Instant LocalDate]
           [java.time.format DateTimeFormatter]))

;; The maximum number of items from a list to print on a single line
(def ^:dynamic *list-limit* 5)

;; The maximum column width to print to in lists.
;; This only applies to scalar values, not structures
(def ^:dynamic *soft-max-width* 120)

;; The maximum number of elements to include in an object before
;; it will be split over multiple lines
(def ^:dynamic *embedded-limit* 2)

;; Whether or not to include the `default-prefixes` in an output document
(def ^:dynamic *include-defaults* true)

(def default-prefixes
  (ordered-map
   :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   :rdfs "http://www.w3.org/2000/01/rdf-schema#"
   :xsd "http://www.w3.org/2001/XMLSchema#"))

(def ^:dynamic *context-base* nil)
(def ^:dynamic *context-prefixes* {}) 

(def echar-map {\newline "\\n"
                \return "\\r"
                \tab "\\t"
                \formfeed "\\f"
                \backspace "\\b"
                \" "\\\""
                \\ "\\\\"})

(defn escape
  "Escapes a string for serializing"
  [s]
  (str (-> s
           (s/replace #"[\n\r\t\f\"\\]" #(echar-map (.charAt % 0)))
           (s/replace "\b" "\\b"))))

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
    clojure.lang.PersistentHashMap})

(defn scalar?
  "Is an object scalar or complex?"
  [s]
  (not (complex? (class s))))

(defn scalar-seq?
  "Tests if a seq contains elements that are scalar and not complex"
  [s]
  (not (some complex? (map class s))))

(defn simple-seq?
  "Tests if a seq contains elements that are scalar or short embeded objects"
  [s]
  (every? #(or (scalar? %)
               (and (map? %)
                    (<= (count %) *embedded-limit*)
                    (every? scalar? (vals %)))) s))

;; An object to wrapping the components of a Typed Literal
(defrecord TypedLiteral [text type])
(defn typed-literal [text type] (->TypedLiteral text type))

;; An object to wrapping the components of a Language tagged Literal
(defrecord LangLiteral [text lang])
(defn lang-literal [text lang] (->LangLiteral text lang))

(defrecord BlankNode [id])

(let [counter (atom 0)]
  (def ^:private labelled-blank-node
    (memoize (fn [label] (->BlankNode (swap! counter inc)))))

  (defn blank-node
    ([] (->BlankNode (swap! counter inc)))
    ([label] (labelled-blank-node label))))

(defn uri-output
  [u abs?]
  (let [s (str u)]
    (cond
      (and abs? *context-base* (s/starts-with? s *context-base*))
      (str \< (subs s (count *context-base*)) \>)

      (and abs? (or *context-prefixes* *include-defaults*))
      (if-let [[k v] (first (filter (fn [[_ v]] (s/starts-with? s v))
                                    (if *include-defaults*
                                      (into default-prefixes *context-prefixes*)
                                      *context-prefixes*)))]
        (str (name k) \: (subs s (count v)))
        (str \<  s \>))

      :default (str \< s \>))))

(defmulti serialize "Converts a simple datatype into a Turtle representation" class)
(defmethod serialize Long [v] (str v))
(defmethod serialize Double [v] (str v))
(defmethod serialize Boolean [v] (str v))
(defmethod serialize String [v] (str \" (escape v) \"))
(defmethod serialize URI [v] (uri-output v (.isAbsolute ^URI v)))
(defmethod serialize URL [v] (uri-output v true))
(defmethod serialize Date [v] (str \" (.format DateTimeFormatter/ISO_INSTANT (.toInstant v)) "\"^^xsd:dateTime"))
(defmethod serialize Instant [v] (str \" (.format DateTimeFormatter/ISO_INSTANT v) "\"^^xsd:dateTime"))
(defmethod serialize LocalDate [v] (str \" (.format DateTimeFormatter/ISO_DATE v) "\"^^xsd:date"))
(defmethod serialize TypedLiteral [{:keys [text type]}] (str \" (escape text) "\"^^" (serialize type)))
(defmethod serialize LangLiteral [{:keys [text lang]}] (str \" (escape text) "\"@" (str lang)))
(defmethod serialize BlankNode [{:keys [id]}] (str "_:b" id))

(defmethod ^String serialize clojure.lang.Keyword
  [v]
  (if-let [lns (namespace v)]
    (str lns ":" (name v))
    (if (= v :a) "a" (str ":" (name v)))))

(defn write-base!
  "Writes a base to the provided output stream.
   out: The output stream to write to.
   base: The base URI as a string."
  [^Writer out base]
  (.write out "@base <")
  (.write out (str base))
  (.write out "> .\n"))

(defn mixed-key-into
  [dest src]
  (reduce (fn [m [k v]] (if (nil? (or (get m k) (get m (name k)))) (assoc m k v) m))
          dest src))

(defn write-prefixes!
  "Writes a prefix map to the provided output stream.
   out: The output stream to write to.
   mp: a map where keys are either strings or keywords for a localname,
       and values are strings containing the full namespace. Optional."
  ([^Writer out] (when *include-defaults* (write-prefixes! out {})))
  ([^Writer out mp]
   (let [mpx (if *include-defaults*
               (mixed-key-into
                 ;; The following is similar to `select-keys` but removes the keys found in mp
                 (reduce (fn [m [k v]]
                           (if (nil? (or (get mp k) (get mp (name k)))) (assoc m k v) m))
                         EMPTY_MAP default-prefixes)
                 mp)
               mp)]
     (doseq [[l p] mpx]
       (.write out "@prefix ")
       (.write out (name l))
       (.write out ": <")
       (.write out (str p))
       (.write out "> .\n")))
   (.write out (int \newline))))


(declare write-entity! write-po! write-blank-object!)

(defn- write-list!
  "Writes a sequence as an rdf:List object.
   Returns the width of the final line."
  [^Writer out lst indent]
  (.write out (int \())
  (if-not (seq lst)
    (do
      (.write out (int \)))
      (+ indent 2))
    (let [width (if (scalar-seq? lst) *list-limit* 1)
          indent (inc indent)
          next-line (apply str \newline (repeat indent \space))
          first-width (write-entity! out (first lst))]
      (loop [[e & r] (rest lst) line-elt 1 last-width (+ indent first-width)]
        (if e
          (letfn [(write-spacing! [newline-test]
                    (if newline-test
                      (do (.write out next-line)
                          [indent 1])
                      (do (.write out (int \space))
                          [(inc last-width) (inc line-elt)])))]
            (if (scalar? e)
              (let [s (serialize e)
                    sl (count s)
                    [in next-elt] (write-spacing!
                                   (or (>= line-elt width)
                                       (and (> line-elt 0)
                                            (> (+ last-width sl 1) *soft-max-width*))))]
                (.write out s)
                (recur r next-elt (+ in sl)))
              (let [[in next-elt] (write-spacing! (= line-elt width))]
                (recur r next-elt (write-entity! out e in)))))
          (do
            (.write out (int \)))
            (inc last-width)))))))

(defn- write-short-anon!
  "Writes a short anonymous object. The first [ character has already been written.
   Returns the length of the written text incremented to include the previously written [ character."
  [^Writer out obj indent]
  (let [[[p o] & r] obj
        pred (serialize p)]
    (.write out pred)
    (.write out (int \space))
    (let [ob-len (if (map? o)
                   (write-blank-object! out o 0)
                   (let [ob (serialize o)]
                     (.write out ob)
                     (count ob)))
          n (+ indent 2 (count pred) ob-len)]
      (loop [[[np no :as npo] & nr] r ind n]
        (if npo
          (let [nps (serialize np)]
            (.write out "; ")
            (.write out nps)
            (.write out (int \space))
            (let [nos-len (if (map? no)
                            (write-blank-object! out no 0)
                            (let [nos (serialize no)]
                              (.write out nos)
                              (count nos)))]
              (recur nr (+ ind 3 (count nps) nos-len))))
          (do
            (.write out (int \]))
            (inc ind)))))))

(defn- write-blank-object!
  "Writes a blank node in square brackets.
   Short embedded blank node objects will be serialized inline without using newlines.
   Returns the indent from the final line"
  [^Writer out obj indent]
  (.write out (int \[))
  (cond
    ;; Short circuit empty nodes
    (empty? obj)
    (do
      (.write out (int \]))
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
            (.write out sp)
            (recur r (write-po! out p o indent)))
          (do
            (.write out (int \]))
            (inc last-width)))))))

(defn- write-entity!
  "Writes an entity to the output stream. Returns the width of the final line.
   out: The output stream to write to.
   subj: The subject to seralize and write.
   indent: The initial indent width to use."
  ([out subj] (write-entity! out subj 0))
  ([^Writer out subj indent]
   (cond
     (scalar? subj) (let [s (serialize subj)]
                      (.write out s)
                      (+ indent (count s)))
     (sequential? subj) (write-list! out subj indent)
     (map? subj) (write-blank-object! out subj indent)
     :default (throw (ex-info (str "Unexpected data type for subject: " (class subj)) {:subject subj})))))

(defn- write-po!
  "Writes a predicate/object(s) pairing.
   Returns the indent of the final line."
  [out p o ind]
  (let [pred (serialize p)
        pwidth (count pred)]
    (.write out pred)
    (.write out " ")
    (if (set? o)
      (let [[f & r] o 
            pwidth (count pred)
            indent (+ ind pwidth 1)
            indent-str (apply str \newline (repeat indent \space))
            in (write-entity! out f indent)]
        (loop [[o1 & r1] r ocount 1 last-indent in]
          (letfn [(write-spacing! [newline-test]
                    (if newline-test
                      (do (.write out indent-str)
                          [indent 1])
                      (do (.write out (int \space))
                          [(+ 2 last-indent) (inc ocount)])))]
            (if o1
              (do
                (.write out (int \,))
                (if (scalar? o1)
                  (let [s (serialize o1)
                        sl (count s)
                        [n newcount] (write-spacing!
                                      (or (>= ocount *list-limit*)
                                          (and (> ocount 0)
                                               (> (+ last-indent sl 2) *soft-max-width*))))]
                    (.write out s)
                    (recur r1 newcount (+ n sl)))
                  (let [[n newcount] (write-spacing! (= ocount *list-limit*))]
                    (recur r1 newcount (write-entity! out o1 n)))))
              last-indent))))
      (write-entity! out o (+ ind pwidth 1)))))

(defn write-triples!
  "Writes the triples for a single subject, as a group.
   The subject is provided as a single element, and the property/values are provided as a map.
   The map contains predicates as keys, and objects as values. If a value is a collection
   then this will be emitted as multiple values with the same property.
   out: The output stream to write to.
   subj: The subject to write triples for.
   property-map: A map of properties to values, or collections of values."
  [^Writer out subj property-map]
  (let [w (write-entity! out subj)
        newline-indent (apply str \newline (repeat (inc w) \space))
        sp (str ";" newline-indent)]
    (.write out (int \space))
    (let [indent (inc w)
          [[p o] & props] property-map]
      (write-po! out p o indent)
      (doseq [[p o] props]
        (.write out sp)
        (write-po! out p o indent))))
  (.write out ".\n\n"))

(defn write-triple!
  "Writes a single triple to the output stream.
   out: The output stream to write to.
   subj: The subject of the triple.
   pred: The predicate of the triple.
   obj: The object of the triple."
  [^Writer out subj pred obj]
  (let [w (write-entity! out subj)
        p (serialize pred)
        w2 (+ 2 (count p) w)]
    (.write out (int \space))
    (.write out p)
    (.write out (int \space))
    (write-entity! out obj w2)
    (.write out ".\n")))

(defn write-triples-map!
  "Writes to a stream a nested map of subjects to maps of predicates to objects.
   Objects may be individuals or collections.
   out: The output stream to write to.
   mp: A map of subjects to property/value maps."
  [^Writer out mp]
  (doseq [[subj props] mp]
    (write-triples! out subj props)))

(defn write-object!
  "Writes a single anonymous object to the output stream.
   out: The object stream to write to.
   e: The entity to write"
  [^Writer out e]
  (write-entity! out e)
  (.write out ".\n\n"))
