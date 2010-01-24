#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#if !defined(WIN32)
#include <unistd.h>
#endif

#include "lrdf.h"
#include "lrdf_md5.h"

typedef struct _lrdf_uri_list {
    char *uri;
    struct _lrdf_uri_list *next;
} lrdf_uri_list;

lrdf_uris *lrdf_match_multi(lrdf_statement *patterns)
{
    lrdf_uris *ret = NULL;
    lrdf_uri_list *uris = NULL, *allocd = NULL;
    lrdf_uri_list *uit, *prev;
    lrdf_statement *it;
    lrdf_statement *matches;
    lrdf_statement *mit;
    lrdf_statement match;
    int count = 0, i, j, rept;

    for (it = patterns; it; it = it->next) {
	if (uris) {
	    /* We allready have a candidate list for the return list, so
	     * iterate over those and check to see if they are possible given
	     * the current pattern */
	    for (prev = NULL, uit = uris; uit; prev = uit, uit = uit->next) {
		match.subject = *(it->subject) == '?' ? uit->uri : it->subject;
		match.predicate = *(it->predicate) == '?' ? uit->uri :
								it->predicate;
		match.object = *(it->object) == '?' ? uit->uri : it->object;
		/* If this pattern didn't match then remove the URI from the
		 * list of candidates */
		if (!lrdf_exists_match(&match)) {
		    count--;
		    if (prev) {
			prev->next = uit->next;
		    } else {
			uris = uit->next;
		    }
		}
	    }
	} else {
	    /* We dont currently have a candidate list for the returns, so
             * build one from the matches for this pattern */
	    match.subject = *(it->subject) == '?' ? NULL : it->subject;
	    match.predicate = *(it->predicate) == '?' ? NULL : it->predicate;
	    match.object = *(it->object) == '?' ? NULL : it->object;
	    matches = lrdf_matches(&match);
	    if (matches == NULL) {
		return NULL;
	    }
	    for (count = 0, mit = matches; mit; count++, mit=mit->next) { }
	    uris = malloc(count * sizeof(lrdf_uri_list));
	    allocd = uris;
	    for (i=0, mit=matches; i<count; i++, mit=mit->next) {
		uris[i].next = &uris[i+1];
		if (*(it->subject) == '?') {
		    uris[i].uri = mit->subject;
		} else if (*(it->predicate) == '?') {
		    uris[i].uri = mit->predicate;
		} else if (*(it->object) == '?') {
		    uris[i].uri = mit->object;
		} else {
		    free(allocd);
		    allocd = NULL;
		    uris = NULL;
		    break;
		}
	    }
	    if (uris) {
		uris[count - 1].next = NULL;
	    }
	}
    }

    ret = malloc(sizeof(lrdf_uris));
    ret->size = count;
    ret->items = malloc(count * sizeof(char *));
    for (uit = uris, i=0; uit; uit=uit->next) {
	rept = 0;
	for (j=0; j<i; j++) {
	    if (!strcmp(uit->uri, ret->items[j])) {
		rept = 1;
		break;
	    }
	}
	/* If the URI has allready been added to the list. */
	if (rept) {
	    continue;
	} else {
	    ret->items[i++] = uit->uri;
	}
    }
    ret->count = i;
    free(allocd);

    return ret;
}

/* vi:set ts=8 sts=4 sw=4: */
