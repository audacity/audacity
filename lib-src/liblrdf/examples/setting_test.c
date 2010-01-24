#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

const float settings[] = {0.1, 0.2, 0.3};

int main(int argc, char*argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:example.rdf",
	       	"file:preset-in.n3",
		 NULL
	};
	lrdf_defaults *defs;
	lrdf_defaults d;
	lrdf_portvalue pv[3];
	lrdf_uris * set_uris;
	int i;

	lrdf_init();
	if (lrdf_read_files(rdf_uris)) {
                fprintf(stderr, "failed to open a file\n");
		exit(1);
        }

	d.items = pv;
	d.count = 3;
	for (i=0; i<d.count; i++) {
		pv[i].pid = i + 1;
		pv[i].value = settings[i];
	}

	lrdf_add_preset("file:preset-out.n3", "test", 100, &d);

	/* check to see if it worked */

	set_uris = lrdf_get_setting_uris(100);
	defs = lrdf_get_setting_values(set_uris->items[0]);
	printf("name: %s\n", lrdf_get_label(set_uris->items[0]));
        for (i=0; i < defs->count; i++) {
                printf("\tport %d = %f\n", defs->items[i].pid,
                       defs->items[i].value);
        }
        lrdf_free_setting_values(defs);

	lrdf_export_by_source("file:preset-out.n3", "file:preset-out.n3");

	lrdf_cleanup();

	return 0;
}
