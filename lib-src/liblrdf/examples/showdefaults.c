#include <stdio.h>
#include <stdlib.h>
#include <ladspa.h>

#include "lrdf.h"

int main(int argc, char*argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:example.rdf",
		 NULL
	};
	int uid;
	char *def_uri;
	lrdf_defaults *defs;
	int i;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <uid>\n", argv[0]);
		return 1;
	}

	lrdf_init();
	if(lrdf_read_files(rdf_uris)) {
		fprintf(stderr, "failed to parse a file\n");
		exit(1);
	}

	uid = atoi(argv[1]);
	def_uri = lrdf_get_default_uri(uid);
	if (def_uri == NULL) {
		printf("(none known)\n");
		return 1;
	}

	printf("Defaults for plugin %d: %s\n", uid,
		lrdf_get_setting_metadata(def_uri, "title"));

	defs = lrdf_get_setting_values(def_uri);
	for (i=0; i < defs->count; i++) {
		printf("\tport %d (%s) = %f\n", defs->items[i].pid,
		 defs->items[i].label, defs->items[i].value);
	}
	lrdf_free_setting_values(defs);

	lrdf_cleanup();

	return 0;
}
