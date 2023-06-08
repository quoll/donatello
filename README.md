# Donatello

A small library for writing TTL files.

### deps.edn
Add the following dependency to the `:deps` map in `deps.edn`:

```clojure
io.github.quoll/donatello {:git/tag "v1.2.6" :git/sha "78455ce"}
```

## Usage

This is a small library with few features and no checking of data validity.
It writes data in TTL format as provided, with very little processing.

Clojure Keywords are treated as [QNames](https://en.wikipedia.org/wiki/QName) or [CURIEs](http://www.w3.org/TR/curie). The special keyword `:a` is treated as the Turtle synonym for `rdf:type`.

To use, open an output stream, then write a header and then the triples:

```clojure
(require '[donatello.ttl :as ttl])
(require '[clojure.java.io :as io])

(with-open [out (io/writer "myfile.ttl")]
  (ttl/write-base! out "http://local.athome.net/")
  (ttl/write-prefixes! out {:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                            :ex "http://example.org/data/"})
  (ttl/write-triples-map! out {:ex/fred {:ex/name "Fred"
                                         :ex/age 40}
                               :ex/wilma {:ex/name "Wilma"
                                          :ex/age 39}})
  (ttl/write-triples! out :ex/wilma {:ex/spouse :ex/fred
                                     :ex/child :ex/bambam})
                                  
  (ttl/write-triple! out :ex/fred :ex/child :ex/bambam)
```

This will create the following TTL file:
```ttl
@base <http://local.athome.net/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/data/> .

ex:fred ex:name "Fred";
        ex:age 40.

ex:wilma ex:name "Wilma";
         ex:age 39.

ex:wilma ex:spouse ex:fred;
         ex:child ex:bambam.

ex:fred ex:child ex:bambam.
```

Note how each call to `write-triples!` or `write-triples-map!` writes new subjects without
merging the data for existing subjects. This is because these functions are writing
data immediately to the stream. If merging is required, then be sure to merge them in maps.

### URLs, URIs and Multiple Objects
URLs and URIs are both supported. Also, if the Object in a triple is a set, then
this will be expanded in multiple triples, using the same subject/predicate for each
object in the collection:

```clojure
(ttl/write-triples! *out* (URI. "http://example.org/data/bambam")
                          {:ex/parent #{(URL. "http://example.org/data/wilma")
                                        (URL. "http://example.org/data/fred")}})
```

This will create an output of:
```ttl
<http://example.org/data/bambam> ex:parent <http://example.org/data/wilma>, <http://example.org/data/fred>.
```

Note how the output is unaware that no prefix map has been emitted on this stream.
Consequently, the keyword `:ex/parent` is emitted without awareness that there is
no prefix describing the `ex` namespace. Similarly, even if the prefix for `ex` had
been included, there is no attempt to convert URIs/URLs into a QName format.

## Contexts
There are 2 context dynamic vars for setting the current context when emitting URLs or URIs:
- `*context-base*`: The current document base.
- `*context-prefixes*`: The current prefixes.

```clojure
(binding [ttl/*context-base* "http://ex.com/"
          ttl/*context-prefixes* {:ex "http://example.org/data/"}]
  (ttl/write-triple! *out* (URI. "http://ex.com/document") :dc/author "Paula")
  (ttl/write-triples! *out* (URI. "http://example.org/data/bambam")
                            {:ex/parent #{(URL. "http://example.org/data/wilma")
                                          (URL. "http://example.org/data/fred")}}))
```

This will create an output of:
```ttl
<document> dc:author "Paula".
ex:bambam ex:parent ex:wilma, ex:fred.
```

## Blank Nodes
Anonymous objects are included as blank nodes:
```clojure
(ttl/write-triple! *out* {:a :data/Class, :b/data :data/_123}
                         :data/rel
                         {:a :data/Class, :b/data :data/_246})
```

Creates an output of:
```ttl
[a data:Class; b:data data:_123] data:rel [a data:Class; b:data data:_246]
```

As per the Turtle spec, single objects may also be written:
```clojure
(ttl/write-object! *out* {:a :data/Number, :rdf/value 5})
```
Leads to:
```ttl
[a data:Number; rdf:value 5].
```

## Collections
Clojure seqs are written as [RDF Collections](https://www.w3.org/TR/rdf-schema/#ch_collectionvocab).
```clojure
(ttl/write-triple! *out* :data/instance
                         :data/values
                         ["one" "two" "three"])
```
```ttl
data:instance data:values ("one" "two" "three").
```

## Nesting
Data structures can be fully nested:
```clojure
(ttl/write-triple! *out* {:a :data/Class
                          :b/data :data/_123
                          :b/more {:a :data/Inner :b/list [1 2 3]}}
                         :data/rel
                         {:p1 #{"data a" "data b"}
                          (URI. "http://ex.com/") 5})
```
```ttl
[a data:Class;
 b:data data:_123;
 b:more [a data:Inner;
         b:list (1 2 3)]] data:rel [:p1 "data a", "data b";
                                    <http://ex.com/> 5].
```

## Functions
- `write-base!` - Writes a "base" directive to set the base IRI document.
- `write-prefixes!` - Writes a map of keywords to string forms of full IRIs to an output stream. Includes default namespaces.
- `write-triple!` - Writes a triple to an output stream. Both the subject and object can be compound terms.
- `write-triples!` - Writes all the triples for a single subject with a map of property/values to an output stream.
- `write-triples-map!` - Writes an entire nested map as a stream of triples to an output stream.
- `write-object!` - Write an anonymous object to an output stream.
- `typed-literal` - Creates a literal out of a lexical representation and its datatype IRI.
- `lang-literal` - Creates a language literal out of a lexical representation and it language string.
- `blank-node` - Creates an explicit blank node to include in a triple.
- `camel-case` - Converts a string into a CamelCase form suitable for class IRIs.
- `lower-camel-case` - Converts a string into a lowerCamerCase form suitable for predicate IRIs.

## Serializing
Various data types will be serialized appropriately:
- `java.net.URI`, `java.net.URL` - These are both serialized as a full IRI, unless:
  - the context includes a matching prefix, in which case a QName is written.
  - the context base matches, in which case a relative IRI is written.
- keyword `:a` - This is treated as the [Turtle special synonym](https://www.w3.org/TR/turtle/#iri-a) for `rdf:type`.
- keywords - Converted as a QName/CURIE (Qualified Name/Compact URI). e.g. `:ns/name` becomes `ns:name`.
- String, boolean, long, double - serialized as the appropriate literals in TTL.
- `java.util.Date`, `java.time.Instant` - serialized as `xsd:dateTime` literals
- `java.util.LocalDate` - serialized as `xsd:date` literals

## Prefixes
By default when prefixes are written, they will automatically be expanded to include the definitions
of the following namespaces if they are not already defined:
- rdf
- rdfs
- xsd

These prefixes are also included in contexts by default.

Binding `*include-defaults*` to `false` will exclude these values before calling `write-prefixes!` or serializing URIs and URLs:

```clojure
(binding [ttl/*include-defaults* false]
  (write-prefixes! output {:ex "http://ex.com/"}))
```
Will only output a single prefix, and not 4.

Also, giving any of these namespaces your own definition will not be overridden by the defaults.

## Extending
Donatello writes objects to the output using the `serialize` multimethod. If you have new objects that you want to write, then you can extend the multimethod to these objects as well.

For instance, [Aristotle](https://github.com/arachne-framework/aristotle#literals) uses symbols for blank nodes, where:

|Symbol|Represents|
|----|----|
|the symbol `_`|unique blank node|
|symbols starting with `_`| named blank node|
|other symbols| IRI of the form `<urn:clojure:namespace/name>`.|

Donatello can be extended to this with:

```clojure
(defmethod ttl/serialize clojure.lang.Symbol [s]
  (cond
    (= s '_) (str "_:" (gensym))
    (= \_ (first (name s))) (str "_:" (subs (name s) 1))
    :default (str "<urn:clojure:" (namespace s) \/ (name s) \>)))
```

## TODO
- Create limits for the above (possibly based on string width, but probably just use a max count per line).

## License

Copyright © 2023 Paula Gearon

Distributed under the Eclipse Public License version 2.0.
