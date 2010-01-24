#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

int main(int argc, char *argv[])
{
    const char *rdf_uris[] = {
	"file:ladspa.rdfs",
	NULL
    };
    const char *src = "test:add";

    lrdf_init();
    if (lrdf_read_files(rdf_uris)) {
	fprintf(stderr, "failed to open a file\n");
	exit(1);
    }

    lrdf_add_triple(src, "test:id1", "test:foo", "1", lrdf_literal);
    lrdf_add_triple(src, "test:id1", "test:bar", "2", lrdf_literal);
    lrdf_add_triple(src, "test:id1", "test:baz", "3", lrdf_literal);
    lrdf_add_triple(src, "test:id2", "test:like", "test:id1", lrdf_uri);
    lrdf_add_triple(src, "test:id2", "test:foo", "4", lrdf_literal);
    lrdf_add_triple(src, "test:id2", "test:bar", "5", lrdf_literal);
    lrdf_add_triple(src, "test:id2", "test:baz", "6", lrdf_literal);

    lrdf_rebuild_caches();

    lrdf_add_triple(src, "test:id2", "test:bar", "5", lrdf_literal);
    lrdf_add_triple(src, "test:id2", "test:baz", "6", lrdf_literal);

    lrdf_rebuild_caches();

    lrdf_export_by_source(src, "test-out.n3");

    lrdf_cleanup();

    return 0;
}
