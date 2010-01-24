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
	const char *external_rdf_uris[] = {
	       	 NULL,
		 NULL
	};
	lrdf_statement *s;
	lrdf_statement *it;

	lrdf_init();
	if (argc == 1) {
		if (lrdf_read_files(rdf_uris)) {
			fprintf(stderr, "failed to open a file\n");
			exit(1);
		}
	} else {
		external_rdf_uris[0] = argv[1];
		if (lrdf_read_files(external_rdf_uris)) {
			fprintf(stderr, "failed to open a file\n");
			exit(1);
		}
	}

	s = lrdf_all_statements();
	for (it = s; it != NULL; it = it->next) {
		printf("(%s, %s, %s)\n", it->subject, it->predicate,
				it->object);
	}

	lrdf_cleanup();

	return 0;
}
