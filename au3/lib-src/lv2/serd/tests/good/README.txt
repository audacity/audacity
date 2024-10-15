These are the tests for the Turtle Terse RDF Triple Language
that must be passed by conformant systems.  See
  http://www.dajobe.org/2004/01/turtle/
for the full conformance information.

The format is a set of good tests and bad tests.

Good tests are a pair of files:
  abc.ttl abc.out
which are the input Turtle file and the expected output RDF triples,
written in N-Triples.

bad tests are of the form
  bad-XX.ttl
which must fail.

The tests should be performed with an assumed base URI
of http://www.w3.org/2001/sw/DataAccess/df1/tests/

Dave
