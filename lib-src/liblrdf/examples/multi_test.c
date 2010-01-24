#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

int main(int argc, char*argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:sample.rdf",
	       	"file:example.rdf",
		 NULL
	};
	lrdf_uris *ulist;
	unsigned int i;
	lrdf_statement p1, p2;

	lrdf_init();
	if (lrdf_read_files(rdf_uris)) {
                fprintf(stderr, "failed to open a file\n");
		exit(1);
        }

	printf("Matches for (?, ladspa:hasLabel, gain)\n");
	p1.subject = "?";
	p1.predicate = "http://ladspa.org/ontology#hasLabel";
	p1.object = "gain";
	p1.next = NULL;
	ulist = lrdf_match_multi(&p1);
	for (i = 0; ulist && i < ulist->count; i++) {
		printf("  %s\n", ulist->items[i]);
	}
	printf("\n");
	lrdf_free_uris(ulist);

	printf("Matches for (?, ladspa:hasLabel, freq), (?, ladspa:hasUnits, ladspa:Hz)\n");
	p1.subject = "?";
	p1.predicate = "http://ladspa.org/ontology#hasLabel";
	p1.object = "freq";
	p1.next = &p2;
	p2.subject = "?";
	p2.predicate = "http://ladspa.org/ontology#hasUnits";
	p2.object = "http://ladspa.org/ontology#Hz";
	p2.next = NULL;
	ulist = lrdf_match_multi(&p1);
	for (i = 0; ulist && i < ulist->count; i++) {
		printf("  %s\n", ulist->items[i]);
	}
	printf("\n");
	lrdf_free_uris(ulist);

	lrdf_cleanup();

	return 0;
}
