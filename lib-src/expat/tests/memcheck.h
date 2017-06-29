/* Copyright (c) 2017 The Expat Maintainers
 * Copying is permitted under the MIT license.  See the file COPYING
 * for details.
 *
 * memcheck.h
 *
 * Interface to allocation functions that will track what has or has
 * not been freed.
*/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XML_MEMCHECK_H
#define XML_MEMCHECK_H 1

/* Allocation declarations */

void *tracking_malloc(size_t size);
void tracking_free(void *ptr);
void *tracking_realloc(void *ptr, size_t size);

/* End-of-test check to see if unfreed allocations remain. Returns
 * TRUE (1) if there is nothing, otherwise prints a report of the
 * remaining allocations and returns FALSE (0).
 */
int tracking_report(void);

#endif /* XML_MEMCHECK_H */

#ifdef __cplusplus
}
#endif
