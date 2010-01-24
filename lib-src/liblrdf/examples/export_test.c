#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

int main(int argc, char*argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:test-in.n3",
		 NULL
	};

	lrdf_init();
	if (lrdf_read_files(rdf_uris)) {
                fprintf(stderr, "failed to open a file\n");
		exit(1);
        }

	lrdf_export_by_source("file:test-in.n3", "file:test-out.n3");

	lrdf_cleanup();

	return 0;
}
