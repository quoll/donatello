# Donatello

A small library for writing TTL files.

### deps.edn
Add the following dependency to the `:deps` map in `deps.edn`:

```clojure
io.github.quoll/donatello {:git/tag "v0.0.5" :git/sha "6b2e0b5"}
```

## Usage

This is a very simple library with few features and no checking of data validity.
It writes data in TTL format as it provided, with very little processing.

To use, open an output stream, then write a header and then the triples:

```clojure
(require '[donatello.ttl :as ttl])
(require '[clojure.java.io :as io])

(with-open [out (io/writer "myfile.ttl")]
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
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns> .
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

## Functions
- `write-prefixes` - Writes a map of keywords to string forms of full IRIs to an output stream. Includes default namespaces.
- `write-triple!` - Writes a single triple to an output stream.
- `write-triples!` - Writes all the triples for a single subject with a map of property/values to an output stream.
- `write-triples-map!` - Writes an entire nested map as a stream of triples to an output stream.
- `typed-literal` - Creates a literal out of a lexical representation and its datatype IRI.
- `lang-literal` - Creates a language literal out of a lexical representation and it language string.
- `camel-case` - Converts a string into a CamelCase form suitable for class IRIs.
- `lower-camel-case` - Converts a string into a lowerCamerCase form suitable for predicate IRIs.

## Serializing
Various data types will be serialized appropriately:
- `java.net.URI`, `java.net.URL` - These are both serialized as a full IRI
- keywords - Converted as a QName/CURIE (Qualified Name/Compact URI). e.g. `:ns/name` becomes `ns:name`
- String, boolean, long, double - serialized as the appropriate literals in TTL.
- `java.util.Date`, `java.time.Instant` - serialized as `xsd:dateTime` literals
- `java.util.LocalDate` - serialized as `xsd:date` literals

## Prefixes
By default when prefixes are written, they will automatically be expanded to include the definitions
of the following namespaces if they are not already defined:
- rdf
- rdfs
- xsd

This can be overridden by binding `*include-defaults* to `false` before calling `write-prefixes!`:

```clojure
(binding [ttl/*include-defaults* false]
  (write-prefixes! output {:ex "http://ex.com/"}))
```
Will only output a single prefix, and not 4.

Also, giving any of these namespaces your own definition will not be overridden by the defaults.

## TODO
Immediate plans:
- Passing maps in the subject or object position should result in a blank node with predicate/object pairs.
- Passing sequential values in the subject or object positions should create a `rdf:List`.
- Create limits for the above (possibly based on string width, but probably just use a max count per line).

Future ideas:
- Scanning URIs/URLs for known namespaces and converting to QNames.
- Defining a protocol/interface for blank nodes to be emitted.

## License

Copyright © 2023 Paula Gearon

_EPLv1.0 is just the default for projects generated by `deps-new`: you are not_
_required to open source this project, nor are you required to use EPLv1.0!_
_Feel free to remove or change the `LICENSE` file and remove or update this_
_section of the `README.md` file!_

Distributed under the Eclipse Public License version 1.0.
