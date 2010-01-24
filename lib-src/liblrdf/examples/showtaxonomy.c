#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ladspa.h>

#include "lrdf.h"

void decend(char *uri, char *base);

int main(int argc, char *argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:example.rdf",
		 NULL
	};

	lrdf_init();
	if (lrdf_read_files(rdf_uris)) {
		fprintf(stderr, "showtaxonomy: failed to parse all files\n");
		exit(1);
	}

	decend(LADSPA_BASE "Plugin", "");

	lrdf_cleanup();

	return 0;
}

void decend(char *uri, char *base)
{
	lrdf_uris *uris;
	unsigned int i;
	char *newbase;
	char *label;

	uris = lrdf_get_instances(uri);

	if (uris != NULL) {
		for (i = 0; i < uris->count; i++) {
			printf("%s/[%ld]\n", base, lrdf_get_uid(uris->items[i]));
		}
		lrdf_free_uris(uris);
	}

	uris = lrdf_get_subclasses(uri);

	if (uris != NULL) {
		for (i = 0; i < uris->count; i++) {
			label = lrdf_get_label(uris->items[i]);
			newbase = malloc(strlen(base) + strlen(label) + 2);
			sprintf(newbase, "%s/%s", base, label);
			printf("%s\n", newbase);
			decend(uris->items[i], newbase);
			free(newbase);
		}
		lrdf_free_uris(uris);
	}
}
