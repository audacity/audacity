#ifndef LRDF_TYPES_H
#define LRDF_TYPES_H

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>

enum lrdf_objtype {
	lrdf_uri,
	lrdf_literal
};

typedef int64_t lrdf_hash;

typedef struct _lrdf_statement {
	char *subject;
	char *predicate;
	char *object;
	enum lrdf_objtype object_type;
	struct _lrdf_statement *next;
	lrdf_hash shash;
	lrdf_hash phash;
	lrdf_hash ohash;
	lrdf_hash source;
} lrdf_statement;

typedef struct _lrdf_string_hash {
	lrdf_hash hash;
	char *str;
	struct _lrdf_string_hash *next;
} lrdf_string_hash;

typedef struct _lrdf_triple_hash {
	lrdf_hash hash;
	lrdf_statement *triple;
	struct _lrdf_triple_hash *next;
} lrdf_triple_hash;

typedef struct _lrdf_closure_hash {
	lrdf_hash subject;
	lrdf_hash object;
	struct _lrdf_closure_hash *next;
} lrdf_closure_hash;

typedef struct {
	unsigned long pid;
	char         *label;
	float         value;
} lrdf_portvalue;

typedef struct {
	unsigned int    count;
	lrdf_portvalue *items;
} lrdf_defaults;

typedef struct {
	unsigned int   size;
	unsigned int   count;
	char         **items;
} lrdf_uris;

#ifdef __cplusplus
}
#endif

#endif
