/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

// C99
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ZIX_INLINE
#include "zix/digest.c"
#include "zix/hash.c"
#include "zix/btree.c"

#include "sord_config.h"
#include "sord_internal.h"

#define SORD_LOG(prefix, ...) fprintf(stderr, "[Sord::" prefix "] " __VA_ARGS__)

#ifdef SORD_DEBUG_ITER
#    define SORD_ITER_LOG(...) SORD_LOG("iter", __VA_ARGS__)
#else
#    define SORD_ITER_LOG(...)
#endif
#ifdef SORD_DEBUG_SEARCH
#    define SORD_FIND_LOG(...) SORD_LOG("search", __VA_ARGS__)
#else
#    define SORD_FIND_LOG(...)
#endif
#ifdef SORD_DEBUG_WRITE
#    define SORD_WRITE_LOG(...) SORD_LOG("write", __VA_ARGS__)
#else
#    define SORD_WRITE_LOG(...)
#endif

#define NUM_ORDERS          12
#define STATEMENT_LEN       3
#define TUP_LEN             (STATEMENT_LEN + 1)
#define DEFAULT_ORDER       SPO
#define DEFAULT_GRAPH_ORDER GSPO

#define TUP_FMT         "(%s %s %s %s)"
#define TUP_FMT_ELEM(e) ((e) ? sord_node_get_string(e) : (const uint8_t*)"*")
#define TUP_FMT_ARGS(t) \
	TUP_FMT_ELEM((t)[0]), \
	TUP_FMT_ELEM((t)[1]), \
	TUP_FMT_ELEM((t)[2]), \
	TUP_FMT_ELEM((t)[3])

#define TUP_S 0
#define TUP_P 1
#define TUP_O 2
#define TUP_G 3

/** Triple ordering */
typedef enum {
	SPO,   ///<         Subject,   Predicate, Object
	SOP,   ///<         Subject,   Object,    Predicate
	OPS,   ///<         Object,    Predicate, Subject
	OSP,   ///<         Object,    Subject,   Predicate
	PSO,   ///<         Predicate, Subject,   Object
	POS,   ///<         Predicate, Object,    Subject
	GSPO,  ///< Graph,  Subject,   Predicate, Object
	GSOP,  ///< Graph,  Subject,   Object,    Predicate
	GOPS,  ///< Graph,  Object,    Predicate, Subject
	GOSP,  ///< Graph,  Object,    Subject,   Predicate
	GPSO,  ///< Graph,  Predicate, Subject,   Object
	GPOS   ///< Graph,  Predicate, Object,    Subject
} SordOrder;

#ifdef SORD_DEBUG_SEARCH
/** String name of each ordering (array indexed by SordOrder) */
static const char* const order_names[NUM_ORDERS] = {
	"spo",  "sop",  "ops",  "osp",  "pso",  "pos",
	"gspo", "gsop", "gops", "gosp", "gpso", "gpos"
};
#endif

/**
   Quads of indices for each order, from most to least significant
   (array indexed by SordOrder)
*/
static const int orderings[NUM_ORDERS][TUP_LEN] = {
	{ 0, 1, 2, 3 }, { 0, 2, 1, 3 },  // SPO, SOP
	{ 2, 1, 0, 3 }, { 2, 0, 1, 3 },  // OPS, OSP
	{ 1, 0, 2, 3 }, { 1, 2, 0, 3 },  // PSO, POS
	{ 3, 0, 1, 2 }, { 3, 0, 2, 1 },  // GSPO, GSOP
	{ 3, 2, 1, 0 }, { 3, 2, 0, 1 },  // GOPS, GOSP
	{ 3, 1, 0, 2 }, { 3, 1, 2, 0 }   // GPSO, GPOS
};

/** World */
struct SordWorldImpl {
	ZixHash*      nodes;
	SerdErrorSink error_sink;
	void*         error_handle;
};

/** Store */
struct SordModelImpl {
	SordWorld* world;

	/** Index for each possible triple ordering (may or may not exist).
	 * Each index is a tree of SordQuad with the appropriate ordering.
	 */
	ZixBTree* indices[NUM_ORDERS];

	size_t n_quads;
	size_t n_iters;
};

/** Mode for searching or iteration */
typedef enum {
	ALL,           ///< Iterate over entire store
	SINGLE,        ///< Iteration over a single element (exact search)
	RANGE,         ///< Iterate over range with equal prefix
	FILTER_RANGE,  ///< Iterate over range with equal prefix, filtering
	FILTER_ALL     ///< Iterate to end of store, filtering
} SearchMode;

/** Iterator over some range of a store */
struct SordIterImpl {
	const SordModel* sord;               ///< Model being iterated over
	ZixBTreeIter*    cur;                ///< Current DB cursor
	SordQuad         pat;                ///< Pattern (in ordering order)
	SordOrder        order;              ///< Store order (which index)
	SearchMode       mode;               ///< Iteration mode
	int              n_prefix;           ///< Prefix for RANGE and FILTER_RANGE
	bool             end;                ///< True iff reached end
	bool             skip_graphs;        ///< Iteration should ignore graphs
};

static uint32_t
sord_node_hash(const void* n)
{
	const SordNode* node = (const SordNode*)n;
	uint32_t        hash = zix_digest_start();
	hash = zix_digest_add(hash, node->node.buf, node->node.n_bytes);
	hash = zix_digest_add(hash, &node->node.type, sizeof(node->node.type));
	if (node->node.type == SERD_LITERAL) {
		hash = zix_digest_add(hash, &node->meta.lit, sizeof(node->meta.lit));
	}
	return hash;
}

static bool
sord_node_hash_equal(const void* a, const void* b)
{
	const SordNode* a_node = (const SordNode*)a;
	const SordNode* b_node = (const SordNode*)b;
	return (a_node == b_node)
		|| ((a_node->node.type == b_node->node.type) &&
		    (a_node->node.type != SERD_LITERAL ||
		     (a_node->meta.lit.datatype == b_node->meta.lit.datatype &&
		      !strncmp(a_node->meta.lit.lang,
		               b_node->meta.lit.lang,
		               sizeof(a_node->meta.lit.lang)))) &&
		    (serd_node_equals(&a_node->node, &b_node->node)));
}

static void
error(SordWorld* world, SerdStatus st, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const SerdError e = { st, NULL, 0, 0, fmt, &args };
	if (world->error_sink) {
		world->error_sink(world->error_handle, &e);
	} else {
		fprintf(stderr, "error: ");
		vfprintf(stderr, fmt, args);
	}
	va_end(args);
}

SordWorld*
sord_world_new(void)
{
	SordWorld* world = (SordWorld*)malloc(sizeof(SordWorld));
	world->error_sink   = NULL;
	world->error_handle = NULL;

	world->nodes = zix_hash_new(
		sord_node_hash, sord_node_hash_equal, sizeof(SordNode));

	return world;
}

static void
free_node_entry(void* value, void* user_data)
{
	SordNode* node = (SordNode*)value;
	if (node->node.type == SERD_LITERAL) {
		sord_node_free((SordWorld*)user_data, node->meta.lit.datatype);
	}
	free((uint8_t*)node->node.buf);
}

void
sord_world_free(SordWorld* world)
{
	zix_hash_foreach(world->nodes, free_node_entry, world);
	zix_hash_free(world->nodes);
	free(world);
}

void
sord_world_set_error_sink(SordWorld*    world,
                          SerdErrorSink error_sink,
                          void*         handle)
{
	world->error_sink   = error_sink;
	world->error_handle = handle;
}

/** Compare nodes, considering NULL a wildcard match. */
static inline int
sord_node_compare(const SordNode* a, const SordNode* b)
{
	if (a == b || !a || !b) {
		return 0;  // Exact or wildcard match
	} else if (a->node.type != b->node.type) {
		return a->node.type - b->node.type;
	}

	int cmp = 0;
	switch (a->node.type) {
	case SERD_URI:
	case SERD_BLANK:
		return strcmp((const char*)a->node.buf, (const char*)b->node.buf);
	case SERD_LITERAL:
		cmp = strcmp((const char*)sord_node_get_string(a),
		             (const char*)sord_node_get_string(b));
		if (cmp == 0) {
			// Note: Can't use sord_node_compare here since it does wildcards
			if (!a->meta.lit.datatype || !b->meta.lit.datatype) {
				cmp = a->meta.lit.datatype - b->meta.lit.datatype;
			} else {
				cmp = strcmp((const char*)a->meta.lit.datatype->node.buf,
				             (const char*)b->meta.lit.datatype->node.buf);
			}
		}
		if (cmp == 0) {
			cmp = strcmp(a->meta.lit.lang, b->meta.lit.lang);
		}
	default:
		break;
	}
	return cmp;
}

bool
sord_node_equals(const SordNode* a, const SordNode* b)
{
	return a == b;  // Nodes are interned
}

/** Return true iff IDs are equivalent, or one is a wildcard */
static inline bool
sord_id_match(const SordNode* a, const SordNode* b)
{
	return !a || !b || (a == b);
}

static inline bool
sord_quad_match_inline(const SordQuad x, const SordQuad y)
{
	return sord_id_match(x[0], y[0])
		&& sord_id_match(x[1], y[1])
		&& sord_id_match(x[2], y[2])
		&& sord_id_match(x[3], y[3]);
}

bool
sord_quad_match(const SordQuad x, const SordQuad y)
{
	return sord_quad_match_inline(x, y);
}

/**
   Compare two quad IDs lexicographically.
   NULL IDs (equal to 0) are treated as wildcards, always less than every
   other possible ID, except itself.
*/
static int
sord_quad_compare(const void* x_ptr, const void* y_ptr, void* user_data)
{
	const int* const           ordering = (const int*)user_data;
	const SordNode*const*const x        = (const SordNode*const*)x_ptr;
	const SordNode*const*const y        = (const SordNode*const*)y_ptr;

	for (int i = 0; i < TUP_LEN; ++i) {
		const int idx = ordering[i];
		const int cmp = sord_node_compare(x[idx], y[idx]);
		if (cmp) {
			return cmp;
		}
	}

	return 0;
}

static inline bool
sord_iter_forward(SordIter* iter)
{
	if (!iter->skip_graphs) {
		zix_btree_iter_increment(iter->cur);
		return zix_btree_iter_is_end(iter->cur);
	}

	SordNode**     key     = (SordNode**)zix_btree_get(iter->cur);
	const SordQuad initial = { key[0], key[1], key[2], key[3] };
	zix_btree_iter_increment(iter->cur);
	while (!zix_btree_iter_is_end(iter->cur)) {
		key = (SordNode**)zix_btree_get(iter->cur);
		for (int i = 0; i < 3; ++i) {
			if (key[i] != initial[i]) {
				return false;
			}
		}

		zix_btree_iter_increment(iter->cur);
	}

	return true;
}

/**
   Seek forward as necessary until `iter` points at a match.
   @return true iff iterator reached end of valid range.
*/
static inline bool
sord_iter_seek_match(SordIter* iter)
{
	for (iter->end = true;
	     !zix_btree_iter_is_end(iter->cur);
	     sord_iter_forward(iter)) {
		const SordNode** const key = (const SordNode**)zix_btree_get(iter->cur);
		if (sord_quad_match_inline(key, iter->pat)) {
			return (iter->end = false);
		}
	}
	return true;
}

/**
   Seek forward as necessary until `iter` points at a match, or the prefix
   no longer matches iter->pat.
   @return true iff iterator reached end of valid range.
*/
static inline bool
sord_iter_seek_match_range(SordIter* iter)
{
	assert(!iter->end);

	do {
		const SordNode** key = (const SordNode**)zix_btree_get(iter->cur);

		if (sord_quad_match_inline(key, iter->pat)) {
			return false;  // Found match
		}

		for (int i = 0; i < iter->n_prefix; ++i) {
			const int idx = orderings[iter->order][i];
			if (!sord_id_match(key[idx], iter->pat[idx])) {
				iter->end = true;  // Reached end of valid range
				return true;
			}
		}
	} while (!sord_iter_forward(iter));

	return (iter->end = true);  // Reached end
}

static SordIter*
sord_iter_new(const SordModel* sord, ZixBTreeIter* cur, const SordQuad pat,
              SordOrder order, SearchMode mode, int n_prefix)
{
	SordIter* iter = (SordIter*)malloc(sizeof(SordIter));
	iter->sord        = sord;
	iter->cur         = cur;
	iter->order       = order;
	iter->mode        = mode;
	iter->n_prefix    = n_prefix;
	iter->end         = false;
	iter->skip_graphs = order < GSPO;
	for (int i = 0; i < TUP_LEN; ++i) {
		iter->pat[i] = pat[i];
	}

	switch (iter->mode) {
	case ALL:
	case SINGLE:
	case RANGE:
		assert(
			sord_quad_match_inline((const SordNode**)zix_btree_get(iter->cur),
			                       iter->pat));
		break;
	case FILTER_RANGE:
		sord_iter_seek_match_range(iter);
		break;
	case FILTER_ALL:
		sord_iter_seek_match(iter);
		break;
	}

#ifdef SORD_DEBUG_ITER
	SordQuad value;
	sord_iter_get(iter, value);
	SORD_ITER_LOG("New %p pat=" TUP_FMT " cur=" TUP_FMT " end=%d skip=%d\n",
	              (void*)iter, TUP_FMT_ARGS(pat), TUP_FMT_ARGS(value),
	              iter->end, iter->skip_graphs);
#endif

	++((SordModel*)sord)->n_iters;
	return iter;
}

const SordModel*
sord_iter_get_model(SordIter* iter)
{
	return iter->sord;
}

void
sord_iter_get(const SordIter* iter, SordQuad tup)
{
	SordNode** key = (SordNode**)zix_btree_get(iter->cur);
	for (int i = 0; i < TUP_LEN; ++i) {
		tup[i] = key[i];
	}
}

const SordNode*
sord_iter_get_node(const SordIter* iter, SordQuadIndex index)
{
	return (!sord_iter_end(iter)
	        ? ((SordNode**)zix_btree_get(iter->cur))[index]
	        : NULL);
}

static bool
sord_iter_scan_next(SordIter* iter)
{
	if (iter->end) {
		return true;
	}

	const SordNode** key;
	if (!iter->end) {
		switch (iter->mode) {
		case ALL:
			// At the end if the cursor is (assigned above)
			break;
		case SINGLE:
			iter->end = true;
			SORD_ITER_LOG("%p reached single end\n", (void*)iter);
			break;
		case RANGE:
			SORD_ITER_LOG("%p range next\n", (void*)iter);
			// At the end if the MSNs no longer match
			key = (const SordNode**)zix_btree_get(iter->cur);
			assert(key);
			for (int i = 0; i < iter->n_prefix; ++i) {
				const int idx = orderings[iter->order][i];
				if (!sord_id_match(key[idx], iter->pat[idx])) {
					iter->end = true;
					SORD_ITER_LOG("%p reached non-match end\n", (void*)iter);
					break;
				}
			}
			break;
		case FILTER_RANGE:
			// Seek forward to next match, stopping if prefix changes
			sord_iter_seek_match_range(iter);
			break;
		case FILTER_ALL:
			// Seek forward to next match
			sord_iter_seek_match(iter);
			break;
		}
	} else {
		SORD_ITER_LOG("%p reached index end\n", (void*)iter);
	}

	if (iter->end) {
		SORD_ITER_LOG("%p Reached end\n", (void*)iter);
		return true;
	} else {
#ifdef SORD_DEBUG_ITER
		SordQuad tup;
		sord_iter_get(iter, tup);
		SORD_ITER_LOG("%p Increment to " TUP_FMT "\n",
		              (void*)iter, TUP_FMT_ARGS(tup));
#endif
		return false;
	}
}

bool
sord_iter_next(SordIter* iter)
{
	if (iter->end) {
		return true;
	}

	iter->end = sord_iter_forward(iter);
	return sord_iter_scan_next(iter);
}

bool
sord_iter_end(const SordIter* iter)
{
	return !iter || iter->end;
}

void
sord_iter_free(SordIter* iter)
{
	SORD_ITER_LOG("%p Free\n", (void*)iter);
	if (iter) {
		--((SordModel*)iter->sord)->n_iters;
		zix_btree_iter_free(iter->cur);
		free(iter);
	}
}

/**
   Return true iff `sord` has an index for `order`.
   If `graphs` is true, `order` will be modified to be the
   corresponding order with a G prepended (so G will be the MSN).
*/
static inline bool
sord_has_index(SordModel* model, SordOrder* order, int* n_prefix, bool graphs)
{
	if (graphs) {
		*order     = (SordOrder)(*order + GSPO);
		*n_prefix += 1;
	}

	return model->indices[*order];
}

/**
   Return the best available index for a pattern.
   @param pat Pattern in standard (S P O G) order
   @param mode Set to the (best) iteration mode for iterating over results
   @param n_prefix Set to the length of the range prefix
   (for `mode` == RANGE and `mode` == FILTER_RANGE)
*/
static inline SordOrder
sord_best_index(SordModel*     sord,
                const SordQuad pat,
                SearchMode*    mode,
                int*           n_prefix)
{
	const bool graph_search = (pat[TUP_G] != 0);

	const unsigned sig
		= (pat[0] ? 1 : 0) * 0x100
		+ (pat[1] ? 1 : 0) * 0x010
		+ (pat[2] ? 1 : 0) * 0x001;

	SordOrder good[2] = { (SordOrder)-1, (SordOrder)-1 };

#define PAT_CASE(sig, m, g0, g1, np) \
	case sig: \
		*mode     = m; \
		good[0]   = g0; \
		good[1]   = g1; \
		*n_prefix = np; \
		break

	// Good orderings that don't require filtering
	*mode     = RANGE;
	*n_prefix = 0;
	switch (sig) {
	case 0x000:
		assert(graph_search);
		*mode     = RANGE;
		*n_prefix = 1;
		return DEFAULT_GRAPH_ORDER;
	case 0x111:
		*mode = SINGLE;
		return graph_search ? DEFAULT_GRAPH_ORDER : DEFAULT_ORDER;

		PAT_CASE(0x001, RANGE, OPS, OSP, 1);
		PAT_CASE(0x010, RANGE, POS, PSO, 1);
		PAT_CASE(0x011, RANGE, OPS, POS, 2);
		PAT_CASE(0x100, RANGE, SPO, SOP, 1);
		PAT_CASE(0x101, RANGE, SOP, OSP, 2);
		PAT_CASE(0x110, RANGE, SPO, PSO, 2);
	}

	if (*mode == RANGE) {
		if (sord_has_index(sord, &good[0], n_prefix, graph_search)) {
			return good[0];
		} else if (sord_has_index(sord, &good[1], n_prefix, graph_search)) {
			return good[1];
		}
	}

	// Not so good orderings that require filtering, but can
	// still be constrained to a range
	switch (sig) {
		PAT_CASE(0x011, FILTER_RANGE, OSP, PSO, 1);
		PAT_CASE(0x101, FILTER_RANGE, SPO, OPS, 1);
		// SPO is always present, so 0x110 is never reached here
	default: break;
	}

	if (*mode == FILTER_RANGE) {
		if (sord_has_index(sord, &good[0], n_prefix, graph_search)) {
			return good[0];
		} else if (sord_has_index(sord, &good[1], n_prefix, graph_search)) {
			return good[1];
		}
	}

	if (graph_search) {
		*mode = FILTER_RANGE;
		*n_prefix = 1;
		return DEFAULT_GRAPH_ORDER;
	} else {
		*mode = FILTER_ALL;
		return DEFAULT_ORDER;
	}
}

SordModel*
sord_new(SordWorld* world, unsigned indices, bool graphs)
{
	SordModel* model = (SordModel*)malloc(sizeof(struct SordModelImpl));
	model->world   = world;
	model->n_quads = 0;
	model->n_iters = 0;

	for (unsigned i = 0; i < (NUM_ORDERS / 2); ++i) {
		const int* const ordering   = orderings[i];
		const int* const g_ordering = orderings[i + (NUM_ORDERS / 2)];

		if (indices & (1 << i)) {
			model->indices[i] = zix_btree_new(
				sord_quad_compare, (void*)ordering, NULL);
			if (graphs) {
				model->indices[i + (NUM_ORDERS / 2)] = zix_btree_new(
					sord_quad_compare, (void*)g_ordering, NULL);
			} else {
				model->indices[i + (NUM_ORDERS / 2)] = NULL;
			}
		} else {
			model->indices[i] = NULL;
			model->indices[i + (NUM_ORDERS / 2)] = NULL;
		}
	}

	if (!model->indices[DEFAULT_ORDER]) {
		model->indices[DEFAULT_ORDER] = zix_btree_new(
			sord_quad_compare, (void*)orderings[DEFAULT_ORDER], NULL);
	}
	if (graphs && !model->indices[DEFAULT_GRAPH_ORDER]) {
		model->indices[DEFAULT_GRAPH_ORDER] = zix_btree_new(
			sord_quad_compare, (void*)orderings[DEFAULT_GRAPH_ORDER], NULL);
	}

	return model;
}

static void
sord_node_free_internal(SordWorld* world, SordNode* node)
{
	assert(node->refs == 0);

	// Cache pointer to buffer to free after node removal and destruction
	const uint8_t* const buf = node->node.buf;

	// Remove node from hash (which frees the node)
	if (zix_hash_remove(world->nodes, node)) {
		error(world, SERD_ERR_INTERNAL, "failed to remove node from hash\n");
	}

	// Free buffer
	free((uint8_t*)buf);
}

static void
sord_add_quad_ref(SordModel* model, const SordNode* node, SordQuadIndex i)
{
	if (node) {
		assert(node->refs > 0);
		++((SordNode*)node)->refs;
		if (node->node.type != SERD_LITERAL && i == SORD_OBJECT) {
			++((SordNode*)node)->meta.res.refs_as_obj;
		}
	}
}

static void
sord_drop_quad_ref(SordModel* model, const SordNode* node, SordQuadIndex i)
{
	if (!node) {
		return;
	}

	assert(node->refs > 0);
	if (node->node.type != SERD_LITERAL && i == SORD_OBJECT) {
		assert(node->meta.res.refs_as_obj > 0);
		--((SordNode*)node)->meta.res.refs_as_obj;
	}
	if (--((SordNode*)node)->refs == 0) {
		sord_node_free_internal(sord_get_world(model), (SordNode*)node);
	}
}

void
sord_free(SordModel* model)
{
	if (!model) {
		return;
	}

	// Free nodes
	SordQuad tup;
	SordIter* i = sord_begin(model);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		sord_iter_get(i, tup);
		for (int t = 0; t < TUP_LEN; ++t) {
			sord_drop_quad_ref(model, tup[t], (SordQuadIndex)t);
		}
	}
	sord_iter_free(i);

	// Free quads
	ZixBTreeIter* t = zix_btree_begin(model->indices[DEFAULT_ORDER]);
	for (; !zix_btree_iter_is_end(t); zix_btree_iter_increment(t)) {
		free(zix_btree_get(t));
	}
	zix_btree_iter_free(t);

	// Free indices
	for (unsigned o = 0; o < NUM_ORDERS; ++o) {
		if (model->indices[o]) {
			zix_btree_free(model->indices[o]);
		}
	}

	free(model);
}

SordWorld*
sord_get_world(SordModel* model)
{
	return model->world;
}

size_t
sord_num_quads(const SordModel* model)
{
	return model->n_quads;
}

size_t
sord_num_nodes(const SordWorld* world)
{
	return zix_hash_size(world->nodes);
}

SordIter*
sord_begin(const SordModel* model)
{
	if (sord_num_quads(model) == 0) {
		return NULL;
	} else {
		ZixBTreeIter* cur = zix_btree_begin(model->indices[DEFAULT_ORDER]);
		SordQuad      pat = { 0, 0, 0, 0 };
		return sord_iter_new(model, cur, pat, DEFAULT_ORDER, ALL, 0);
	}
}

SordIter*
sord_find(SordModel* model, const SordQuad pat)
{
	if (!pat[0] && !pat[1] && !pat[2] && !pat[3]) {
		return sord_begin(model);
	}

	SearchMode      mode;
	int             n_prefix;
	const SordOrder index_order = sord_best_index(model, pat, &mode, &n_prefix);

	SORD_FIND_LOG("Find " TUP_FMT "  index=%s  mode=%d  n_prefix=%d\n",
	              TUP_FMT_ARGS(pat), order_names[index_order], mode, n_prefix);

	if (pat[0] && pat[1] && pat[2] && pat[3]) {
		mode = SINGLE;  // No duplicate quads (Sord is a set)
	}

	ZixBTree* const db  = model->indices[index_order];
	ZixBTreeIter*   cur = NULL;
	zix_btree_lower_bound(db, pat, &cur);
	if (zix_btree_iter_is_end(cur)) {
		SORD_FIND_LOG("No match found\n");
		zix_btree_iter_free(cur);
		return NULL;
	}
	const SordNode** const key = (const SordNode**)zix_btree_get(cur);
	if (!key || ( (mode == RANGE || mode == SINGLE)
	              && !sord_quad_match_inline(pat, key) )) {
		SORD_FIND_LOG("No match found\n");
		zix_btree_iter_free(cur);
		return NULL;
	}

	return sord_iter_new(model, cur, pat, index_order, mode, n_prefix);
}

SordIter*
sord_search(SordModel*      model,
            const SordNode* s,
            const SordNode* p,
            const SordNode* o,
            const SordNode* g)
{
	SordQuad pat = { s, p, o, g };
	return sord_find(model, pat);
}

SordNode*
sord_get(SordModel*      model,
         const SordNode* s,
         const SordNode* p,
         const SordNode* o,
         const SordNode* g)
{
	if ((bool)s + (bool)p + (bool)o != 2) {
		return NULL;
	}

	SordIter* i   = sord_search(model, s, p, o, g);
	SordNode* ret = NULL;
	if (!s) {
		ret = sord_node_copy(sord_iter_get_node(i, SORD_SUBJECT));
	} else if (!p) {
		ret = sord_node_copy(sord_iter_get_node(i, SORD_PREDICATE));
	} else if (!o) {
		ret = sord_node_copy(sord_iter_get_node(i, SORD_OBJECT));
	}

	sord_iter_free(i);
	return ret;
}

bool
sord_ask(SordModel*      model,
         const SordNode* s,
         const SordNode* p,
         const SordNode* o,
         const SordNode* g)
{
	SordQuad pat = { s, p, o, g };
	return sord_contains(model, pat);
}

uint64_t
sord_count(SordModel*      model,
           const SordNode* s,
           const SordNode* p,
           const SordNode* o,
           const SordNode* g)
{
	SordIter* i = sord_search(model, s, p, o, g);
	uint64_t  n = 0;
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		++n;
	}
	sord_iter_free(i);
	return n;
}

bool
sord_contains(SordModel* model, const SordQuad pat)
{
	SordIter* iter = sord_find(model, pat);
	bool      ret  = (iter != NULL);
	sord_iter_free(iter);
	return ret;
}

static uint8_t*
sord_strndup(const uint8_t* str, size_t len)
{
	uint8_t* dup = (uint8_t*)malloc(len + 1);
	memcpy(dup, str, len + 1);
	return dup;
}

SordNodeType
sord_node_get_type(const SordNode* node)
{
	switch (node->node.type) {
	case SERD_URI:
		return SORD_URI;
	case SERD_BLANK:
		return SORD_BLANK;
	default:
		return SORD_LITERAL;
	}
	SORD_UNREACHABLE();
}

const uint8_t*
sord_node_get_string(const SordNode* node)
{
	return node->node.buf;
}

const uint8_t*
sord_node_get_string_counted(const SordNode* node, size_t* bytes)
{
	*bytes = node->node.n_bytes;
	return node->node.buf;
}

const uint8_t*
sord_node_get_string_measured(const SordNode* node,
                              size_t*         bytes,
                              size_t*         chars)
{
	*bytes = node->node.n_bytes;
	*chars = node->node.n_chars;
	return node->node.buf;
}

const char*
sord_node_get_language(const SordNode* node)
{
	if (node->node.type != SERD_LITERAL || !node->meta.lit.lang[0]) {
		return NULL;
	}
	return node->meta.lit.lang;
}

SordNode*
sord_node_get_datatype(const SordNode* node)
{
	return (node->node.type == SERD_LITERAL) ? node->meta.lit.datatype : NULL;
}

SerdNodeFlags
sord_node_get_flags(const SordNode* node)
{
	return node->node.flags;
}

bool
sord_node_is_inline_object(const SordNode* node)
{
	return (node->node.type == SERD_BLANK) && (node->meta.res.refs_as_obj == 1);
}

static SordNode*
sord_insert_node(SordWorld* world, const SordNode* key, bool copy)
{
	SordNode* node = NULL;
	ZixStatus st   = zix_hash_insert(world->nodes, key, (const void**)&node);
	switch (st) {
	case ZIX_STATUS_EXISTS:
		++node->refs;
		break;
	case ZIX_STATUS_SUCCESS:
		assert(node->refs == 1);
		if (copy) {
			node->node.buf = sord_strndup(node->node.buf, node->node.n_bytes);
		}
		if (node->node.type == SERD_LITERAL) {
			node->meta.lit.datatype = sord_node_copy(node->meta.lit.datatype);
		}
		return node;
	default:
		error(world, SERD_ERR_INTERNAL,
		      "error inserting node `%s'\n", key->node.buf);
	}

	if (!copy) {
		// Free the buffer we would have copied if a new node was created
		free((uint8_t*)key->node.buf);
	}

	return node;
}

static SordNode*
sord_new_uri_counted(SordWorld* world, const uint8_t* str,
                     size_t n_bytes, size_t n_chars, bool copy)
{
	if (!serd_uri_string_has_scheme(str)) {
		error(world, SERD_ERR_BAD_ARG,
		      "attempt to map invalid URI `%s'\n", str);
		return NULL;  // Can't intern relative URIs
	}

	const SordNode key = {
		{ str, n_bytes, n_chars, 0, SERD_URI }, 1, { { 0 } }
	};

	return sord_insert_node(world, &key, copy);
}

SordNode*
sord_new_uri(SordWorld* world, const uint8_t* uri)
{
	const SerdNode node = serd_node_from_string(SERD_URI, uri);
	return sord_new_uri_counted(world, uri, node.n_bytes, node.n_chars, true);
}

SordNode*
sord_new_relative_uri(SordWorld*     world,
                      const uint8_t* uri,
                      const uint8_t* base_uri)
{
	if (serd_uri_string_has_scheme(uri)) {
		return sord_new_uri(world, uri);
	}
	SerdURI  buri = SERD_URI_NULL;
	SerdNode base = serd_node_new_uri_from_string(base_uri, NULL, &buri);
	SerdNode node = serd_node_new_uri_from_string(uri, &buri, NULL);

	SordNode* ret = sord_new_uri_counted(
		world, node.buf, node.n_bytes, node.n_chars, false);

	serd_node_free(&base);
	return ret;
}

static SordNode*
sord_new_blank_counted(SordWorld* world, const uint8_t* str,
                       size_t n_bytes, size_t n_chars)
{
	const SordNode key = {
		{ str, n_bytes, n_chars, 0, SERD_BLANK }, 1, { { 0 } }
	};

	return sord_insert_node(world, &key, true);
}

SordNode*
sord_new_blank(SordWorld* world, const uint8_t* str)
{
	const SerdNode node = serd_node_from_string(SERD_URI, str);
	return sord_new_blank_counted(world, str, node.n_bytes, node.n_chars);
}

static SordNode*
sord_new_literal_counted(SordWorld*     world,
                         SordNode*      datatype,
                         const uint8_t* str,
                         size_t         n_bytes,
                         size_t         n_chars,
                         SerdNodeFlags  flags,
                         const char*    lang)
{
	SordNode key = {
		{ str, n_bytes, n_chars, flags, SERD_LITERAL }, 1, { { 0 } }
	};
	key.meta.lit.datatype = sord_node_copy(datatype);
	memset(key.meta.lit.lang, 0, sizeof(key.meta.lit.lang));
	if (lang) {
		strncpy(key.meta.lit.lang, lang, sizeof(key.meta.lit.lang));
	}

	return sord_insert_node(world, &key, true);
}

SordNode*
sord_new_literal(SordWorld* world, SordNode* datatype,
                 const uint8_t* str, const char* lang)
{
	SerdNodeFlags flags   = 0;
	size_t        n_bytes = 0;
	size_t        n_chars = serd_strlen(str, &n_bytes, &flags);
	return sord_new_literal_counted(world, datatype,
	                                str, n_bytes, n_chars, flags,
	                                lang);
}

SordNode*
sord_node_from_serd_node(SordWorld*      world,
                         SerdEnv*        env,
                         const SerdNode* node,
                         const SerdNode* datatype,
                         const SerdNode* lang)
{
	if (!node) {
		return NULL;
	}

	SordNode* datatype_node = NULL;
	SordNode* ret           = NULL;
	switch (node->type) {
	case SERD_NOTHING:
		return NULL;
	case SERD_LITERAL:
		datatype_node = sord_node_from_serd_node(
			world, env, datatype, NULL, NULL),
		ret = sord_new_literal_counted(
			world,
			datatype_node,
			node->buf,
			node->n_bytes,
			node->n_chars,
			node->flags,
			lang ? (const char*)lang->buf : NULL);
		sord_node_free(world, datatype_node);
		return ret;
	case SERD_URI:
		if (serd_uri_string_has_scheme(node->buf)) {
			return sord_new_uri_counted(
				world, node->buf, node->n_bytes, node->n_chars, true);
		} else {
			SerdURI base_uri;
			serd_env_get_base_uri(env, &base_uri);
			SerdURI  abs_uri;
			SerdNode abs_uri_node = serd_node_new_uri_from_node(
				node, &base_uri, &abs_uri);
			ret = sord_new_uri_counted(world,
			                           abs_uri_node.buf,
			                           abs_uri_node.n_bytes,
			                           abs_uri_node.n_chars,
			                           true);
			serd_node_free(&abs_uri_node);
			return ret;
		}
	case SERD_CURIE: {
		SerdChunk uri_prefix;
		SerdChunk uri_suffix;
		if (serd_env_expand(env, node, &uri_prefix, &uri_suffix)) {
			error(world, SERD_ERR_BAD_CURIE,
			      "failed to expand CURIE `%s'\n", node->buf);
			return NULL;
		}
		const size_t uri_len = uri_prefix.len + uri_suffix.len;
		uint8_t*     buf     = (uint8_t*)malloc(uri_len + 1);
		memcpy(buf,                  uri_prefix.buf, uri_prefix.len);
		memcpy(buf + uri_prefix.len, uri_suffix.buf, uri_suffix.len);
		buf[uri_len] = '\0';
		ret = sord_new_uri_counted(
			world, buf, uri_len, serd_strlen(buf, NULL, NULL), false);
		return ret;
	}
	case SERD_BLANK:
		return sord_new_blank_counted(
			world, node->buf, node->n_bytes, node->n_chars);
	}
	return NULL;
}

const SerdNode*
sord_node_to_serd_node(const SordNode* node)
{
	return node ? &node->node : &SERD_NODE_NULL;
}

void
sord_node_free(SordWorld* world, SordNode* node)
{
	if (!node) {
		return;
	} else if (node->refs == 0) {
		error(world, SERD_ERR_BAD_ARG, "attempt to free garbage node\n");
	} else if (--node->refs == 0) {
		sord_node_free_internal(world, node);
	}
}

SordNode*
sord_node_copy(const SordNode* node)
{
	SordNode* copy = (SordNode*)node;
	if (copy) {
		++copy->refs;
	}
	return copy;
}

static inline bool
sord_add_to_index(SordModel* model, const SordNode** tup, SordOrder order)
{
	return !zix_btree_insert(model->indices[order], tup);
}

bool
sord_add(SordModel* model, const SordQuad tup)
{
	SORD_WRITE_LOG("Add " TUP_FMT "\n", TUP_FMT_ARGS(tup));
	if (!tup[0] || !tup[1] || !tup[2]) {
		error(model->world, SERD_ERR_BAD_ARG,
		      "attempt to add quad with NULL field\n");
		return false;
	} else if (model->n_iters > 0) {
		error(model->world, SERD_ERR_BAD_ARG, "added tuple during iteration\n");
	}

	const SordNode** quad = (const SordNode**)malloc(sizeof(SordQuad));
	memcpy(quad, tup, sizeof(SordQuad));

	for (unsigned i = 0; i < NUM_ORDERS; ++i) {
		if (model->indices[i] && (i < GSPO || tup[3])) {
			if (!sord_add_to_index(model, quad, (SordOrder)i)) {
				assert(i == 0);  // Assuming index coherency
				free(quad);
				return false;  // Quad already stored, do nothing
			}
		}
	}

	for (int i = 0; i < TUP_LEN; ++i) {
		sord_add_quad_ref(model, tup[i], (SordQuadIndex)i);
	}

	++model->n_quads;
	return true;
}

void
sord_remove(SordModel* model, const SordQuad tup)
{
	SORD_WRITE_LOG("Remove " TUP_FMT "\n", TUP_FMT_ARGS(tup));
	if (model->n_iters > 0) {
		error(model->world, SERD_ERR_BAD_ARG, "remove with iterator\n");
	}

	SordNode* quad = NULL;
	for (unsigned i = 0; i < NUM_ORDERS; ++i) {
		if (model->indices[i] && (i < GSPO || tup[3])) {
			if (zix_btree_remove(model->indices[i], tup, (void**)&quad, NULL)) {
				assert(i == 0);  // Assuming index coherency
				return;  // Quad not found, do nothing
			}
		}
	}

	free(quad);

	for (int i = 0; i < TUP_LEN; ++i) {
		sord_drop_quad_ref(model, tup[i], (SordQuadIndex)i);
	}

	--model->n_quads;
}

SerdStatus
sord_erase(SordModel* model, SordIter* iter)
{
	if (model->n_iters > 1) {
		error(model->world, SERD_ERR_BAD_ARG, "erased with many iterators\n");
		return SERD_ERR_BAD_ARG;
	}

	SordQuad tup;
	sord_iter_get(iter, tup);

	SORD_WRITE_LOG("Remove " TUP_FMT "\n", TUP_FMT_ARGS(tup));

	SordNode* quad = NULL;
	for (unsigned i = 0; i < NUM_ORDERS; ++i) {
		if (model->indices[i] && (i < GSPO || tup[3])) {
			if (zix_btree_remove(model->indices[i], tup, (void**)&quad,
			                     i == iter->order ? &iter->cur : NULL)) {
				return (i == 0) ? SERD_ERR_NOT_FOUND : SERD_ERR_INTERNAL;
			}
		}
	}
	iter->end = zix_btree_iter_is_end(iter->cur);
	sord_iter_scan_next(iter);

	free(quad);

	for (int i = 0; i < TUP_LEN; ++i) {
		sord_drop_quad_ref(model, tup[i], (SordQuadIndex)i);
	}

	--model->n_quads;
	return SERD_SUCCESS;
}
