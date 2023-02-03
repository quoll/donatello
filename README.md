# donatello/ttl

A small library for writing TTL files.

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
  (ttl/write-triples! out :wilma {:ex/spouse :ex/fred
                                  :ex/child :ex/bambam})
```

This will create the following TTL file:
```ttl
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns> .
@prefix ex: <http://example.org/data/> .

ex:fred ex:name "Fred";
        ex:age 40.

ex:wilma ex:name "Wilma";
         ex:age 39.

ex:wilma ex:spouse ex:fred;
         ex:child ex:bambam.
```

Note how each call to `write-triples!` or `write-triples-map!` writes new subjects without
merging the data for existing subjects. This is because these functions are writing
data immediately to the stream. If merging is required, then be sure to merge them in maps.

### URLs, URIs and Multiple Objects
URLs and URIs are both supported. Also, if the Object in a triple is a collection, then
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

## License

Copyright © 2023 Paula Gearon

_EPLv1.0 is just the default for projects generated by `deps-new`: you are not_
_required to open source this project, nor are you required to use EPLv1.0!_
_Feel free to remove or change the `LICENSE` file and remove or update this_
_section of the `README.md` file!_

Distributed under the Eclipse Public License version 1.0.
