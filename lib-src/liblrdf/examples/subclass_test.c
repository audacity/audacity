#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

int main(int argc, char*argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:example.rdf",
		 NULL
	};
	lrdf_uris *ulist;
	unsigned int i;

	lrdf_init();
	if (lrdf_read_files(rdf_uris)) {
                fprintf(stderr, "failed to open a file\n");
		exit(1);
        }

	printf("Subclasses of http://ladspa.org/ontology#Plugin\n");
	ulist = lrdf_get_all_subclasses("http://ladspa.org/ontology#Plugin");
	for (i = 0; ulist && i < ulist->count; i++) {
		printf("  %s\n", ulist->items[i]);
	}
	printf("\n");
	lrdf_free_uris(ulist);

	printf("Superclasses of http://ladspa.org/ontology#Plugin\n");
	ulist = lrdf_get_all_superclasses("http://ladspa.org/ontology#Plugin");
	for (i = 0; ulist && i < ulist->count; i++) {
		printf("  %s\n", ulist->items[i]);
	}
	printf("\n");
	lrdf_free_uris(ulist);

	lrdf_cleanup();

	return 0;
}
