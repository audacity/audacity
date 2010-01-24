#include <stdio.h>
#include <raptor.h>
#include <stdlib.h>

/* rdfserialize.c: serialize 1 triple to RDF/XML-Abbrev */

int
main(int argc, char *argv[])
{
  raptor_serializer* rdf_serializer=NULL;
  unsigned char *uri_string;
  raptor_uri *base_uri;
  raptor_statement* triple;

  raptor_init();
  
  uri_string=raptor_uri_filename_to_uri_string(argv[1]);
  base_uri=raptor_new_uri(uri_string);

  rdf_serializer=raptor_new_serializer("rdfxml-abbrev");
  raptor_serialize_start_to_file_handle(rdf_serializer, base_uri, stdout);
  
  /* Make a triple with URI subject, URI predicate, literal object */
  triple=malloc(sizeof(raptor_statement));
  triple->subject=(void*)raptor_new_uri((const unsigned char*)"http://example.org/subject");
  triple->subject_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  triple->predicate=(void*)raptor_new_uri((const unsigned char*)"http://example.org/predicate");
  triple->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  triple->object="An example literal";
  triple->object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
  triple->object_literal_language=(const unsigned char*)"en";

  /* Write the triple */
  raptor_serialize_statement(rdf_serializer, triple);

  /* Delete the triple */
  raptor_free_uri((raptor_uri*)triple->subject);
  raptor_free_uri((raptor_uri*)triple->predicate);
  free(triple);

  raptor_serialize_end(rdf_serializer);
  raptor_free_serializer(rdf_serializer);
  
  raptor_free_uri(base_uri);
  raptor_free_memory(uri_string);

  raptor_finish();
  return 0;
}
