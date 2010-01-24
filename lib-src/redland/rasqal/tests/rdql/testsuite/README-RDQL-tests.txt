This is the RDQL/Jena test suite from the Jena distribution.
The URI for RDQL is http://jena.hpl.hp.com/2003/07/query/RDQL.

It is provided so that other implementations of RDQL can get the test suite
without needing the whole Jena distribution and to provide a bookmarkable
resource that "is" the RDQL/Jena test suite

Contents:

  Documentation: README-RDQL-tests.txt 
  Manifest file : rdql-tests.n3
  Tests : test-?-??
  Expected results by test : result-?-??.n3
  Input data : model?.*
  Data from Jena tutorial (test data): vc-db-?.rdf

These are also available in the Jena distribution in the testing/RDQL
directory.

The tests can be run with Jena by:

    java -cp ... jena.rdfquery --test rdql-tests.n3

