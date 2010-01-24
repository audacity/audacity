#ifndef LIBRDF_AVLTREE_H
#define LIBRDF_AVLTREE_H

#ifndef LIBRDF_OBJC_FRAMEWORK
#include <rdf_uri.h>
#else
#include <Redland/rdf_uri.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define LIBRDF_AVLTREE_ENOMEM -1
#define LIBRDF_AVLTREE_EXISTS 1

typedef struct librdf_avltree_s librdf_avltree;

typedef int (*librdf_avltree_data_compare_function)(const void* data1, const void* data2);
typedef void (*librdf_avltree_data_free_function)(void* data);
typedef int (*librdf_avltree_visit_function)(int depth, void* data, void *user_data);
typedef void (*librdf_avltree_data_print_function)(FILE* handle, const void* data);

/* constructor / destructor */
librdf_avltree* librdf_new_avltree(librdf_avltree_data_compare_function compare_fn, librdf_avltree_data_free_function free_fn/*, unsigned int flags*/);
void librdf_free_avltree(librdf_avltree* tree);

/* methods */
int librdf_avltree_add(librdf_avltree* tree, void* p_user);
void* librdf_avltree_remove(librdf_avltree* tree, void* p_data);
int librdf_avltree_delete(librdf_avltree* tree, void* p_user);
void* librdf_avltree_search(librdf_avltree* tree, const void* p_user);
int librdf_avltree_visit(librdf_avltree* tree, librdf_avltree_visit_function visit_fn, void* user_data);
int librdf_avltree_size(librdf_avltree* tree);
void librdf_avltree_set_print_handler(librdf_avltree* tree, librdf_avltree_data_print_function print_fn);
void librdf_avltree_print(librdf_world* world, librdf_avltree* tree, FILE* stream,
                          librdf_avltree_data_print_function print_fn);

#ifdef LIBRDF_DEBUG
int librdf_avltree_dump(librdf_avltree* tree, FILE* stream,
		                librdf_avltree_data_print_function print_fn);
void librdf_avltree_check(librdf_avltree* tree);
#endif
#if 0
int librdf_avltree_cursor_first(librdf_avltree* tree);
int librdf_avltree_cursor_last(librdf_avltree* tree);
int librdf_avltree_cursor_prev(librdf_avltree* tree);
int librdf_avltree_cursor_next(librdf_avltree* tree);
void* librdf_avltree_cursor_get(librdf_avltree* tree);
#endif

librdf_iterator* librdf_avltree_get_iterator_start(librdf_world* world,
                                                   librdf_avltree* tree, void* range,
                                                   librdf_avltree_data_free_function range_free_fn);


#ifdef __cplusplus
}
#endif

#endif
