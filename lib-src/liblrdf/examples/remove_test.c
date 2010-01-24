#include <stdio.h>
#include <stdlib.h>

#include "lrdf.h"

int main(int argc, char*argv[])
{
	const char *rdf_uris[] = {
		"file:ladspa.rdfs",
	       	"file:///usr/local/share/ladspa/rdf/swh-plugins.rdf",
		 NULL
	};
	lrdf_uris *ulist;
	const char *src = "test:add";
	unsigned int i;
        lrdf_statement *s;
	lrdf_statement *it;
	lrdf_statement p;

	lrdf_init();
	if (lrdf_read_files(rdf_uris)) {
                fprintf(stderr, "failed to open a file\n");
		exit(1);
        }

        p.subject = "http://ladspa.org/ontology#1407";
	p.predicate = NULL;
	p.object = NULL;
	s = lrdf_one_match(&p);
	printf("First match...\n");
	printf("(%s, %s, %s)\n\n", s->subject, s->predicate, s->object);
	
	s = lrdf_matches(&p);
	printf("Triples before removing...\n");
	for (it = s; it; it = it->next) {
		printf("(%s, %s, %s)\n", it->subject, it->predicate,
				it->object);
	}
	printf("...end\n");
	lrdf_free_statements(s);

	lrdf_remove_matches(&p);

	s = lrdf_matches(&p);
	printf("Triples after removing...\n");
	for (it = s; it; it = it->next) {
		printf("(%s, %s, %s)\n", it->subject, it->predicate,
				it->object);
	}
	printf("...end\n");
	lrdf_free_statements(s);

	lrdf_add_triple(src, p.subject, "test:foo", "1", lrdf_literal);
	lrdf_add_triple(src, p.subject, "test:bar", "2", lrdf_literal);
	lrdf_add_triple(src, p.subject, "test:baz", "3", lrdf_literal);

	s = lrdf_matches(&p);
	printf("Triples after adding...\n");
	for (it = s; it; it = it->next) {
		printf("(%s, %s, %s)\n", it->subject, it->predicate,
				it->object);
	}
	lrdf_free_statements(s);

	lrdf_remove_matches(&p);

	/* Just in case */
	lrdf_rebuild_caches();

	s = lrdf_matches(&p);
	printf("Triples after removing...\n");
	for (it = s; it; it = it->next) {
		printf("(%s, %s, %s)\n", it->subject, it->predicate,
				it->object);
	}
	printf("...end\n");
	lrdf_free_statements(s);

	lrdf_add_triple(src, p.subject, p.subject, "1", lrdf_literal);
	lrdf_add_triple(src, "foo", p.subject, p.subject, lrdf_literal);
	lrdf_add_triple(src, p.subject, "pred", p.subject, lrdf_literal);
	lrdf_add_triple(src, "object", "pred", p.subject, lrdf_literal);

	lrdf_remove_uri_matches(p.subject);

	s = lrdf_matches(&p);
	printf("Triples after removing...\n");
	for (it = s; it; it = it->next) {
		printf("(%s, %s, %s)\n", it->subject, it->predicate,
				it->object);
	}
	printf("...end\n");
	lrdf_free_statements(s);

	/* Just str a fre things that should be NOPs to catch corner cases */
	lrdf_rebuild_caches();
	lrdf_remove_uri_matches(p.subject);
	lrdf_rebuild_caches();
	lrdf_remove_matches(&p);
	lrdf_remove_matches(&p);
	lrdf_remove_matches(&p);

	printf("\nend\n");
	lrdf_cleanup();

	return 0;
}
