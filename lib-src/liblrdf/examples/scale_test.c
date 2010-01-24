#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

const float settings[] = { 0.1, 0.2, 0.3 };

int main(int argc, char *argv[])
{
    const char *rdf_uris[] = {
	"file:ladspa.rdfs",
	"file:scale-example.rdf",
	NULL
    };
    lrdf_defaults *defs;
    int i;

    lrdf_init();
    if (lrdf_read_files(rdf_uris)) {
	fprintf(stderr, "failed to open a file\n");
	exit(1);
    }

    defs = lrdf_get_scale_values(100, 1);
    for (i = 0; defs && i < defs->count; i++) {
	printf("%f = '%s'\n", defs->items[i].value, defs->items[i].label);
    }
    lrdf_free_setting_values(defs);

    /* check for bugs when looking for ports that have no scales */
    defs = lrdf_get_scale_values(999999, 1);
    if (defs) {
	printf("error: lrdf_get_scale_values(999999, 1) returned non-NULL\n");
    }

    lrdf_cleanup();

    return 0;
}
