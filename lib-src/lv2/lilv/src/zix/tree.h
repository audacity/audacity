/*
  Copyright 2011-2019 David Robillard <http://drobilla.net>

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

#ifndef ZIX_TREE_H
#define ZIX_TREE_H

#include "zix/common.h"

#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
   @addtogroup zix
   @{
   @name Tree
   @{
*/

/**
   A balanced binary search tree.
*/
typedef struct ZixTreeImpl ZixTree;

/**
   An iterator over a ZixTree.
*/
typedef struct ZixTreeNodeImpl ZixTreeIter;

/**
   Create a new (empty) tree.
*/
ZIX_API ZixTree*
zix_tree_new(bool           allow_duplicates,
             ZixComparator  cmp,
             void*          cmp_data,
             ZixDestroyFunc destroy);

/**
   Free `t`.
*/
ZIX_API void
zix_tree_free(ZixTree* t);

/**
   Return the number of elements in `t`.
*/
ZIX_API size_t
zix_tree_size(const ZixTree* t);

/**
   Insert the element `e` into `t` and point `ti` at the new element.
*/
ZIX_API ZixStatus
zix_tree_insert(ZixTree* t, void* e, ZixTreeIter** ti);

/**
   Remove the item pointed at by `ti` from `t`.
*/
ZIX_API ZixStatus
zix_tree_remove(ZixTree* t, ZixTreeIter* ti);

/**
   Set `ti` to an element equal to `e` in `t`.
   If no such item exists, `ti` is set to NULL.
*/
ZIX_API ZixStatus
zix_tree_find(const ZixTree* t, const void* e, ZixTreeIter** ti);

/**
   Return the data associated with the given tree item.
*/
ZIX_API void*
zix_tree_get(const ZixTreeIter* ti);

/**
   Return an iterator to the first (smallest) element in `t`.
*/
ZIX_API ZixTreeIter*
zix_tree_begin(ZixTree* t);

/**
   Return an iterator the the element one past the last element in `t`.
*/
ZIX_API ZixTreeIter*
zix_tree_end(ZixTree* t);

/**
   Return true iff `i` is an iterator to the end of its tree.
*/
ZIX_API bool
zix_tree_iter_is_end(const ZixTreeIter* i);

/**
   Return an iterator to the last (largest) element in `t`.
*/
ZIX_API ZixTreeIter*
zix_tree_rbegin(ZixTree* t);

/**
   Return an iterator the the element one before the first element in `t`.
*/
ZIX_API ZixTreeIter*
zix_tree_rend(ZixTree* t);

/**
   Return true iff `i` is an iterator to the reverse end of its tree.
*/
ZIX_API bool
zix_tree_iter_is_rend(const ZixTreeIter* i);

/**
   Return an iterator that points to the element one past `i`.
*/
ZIX_API ZixTreeIter*
zix_tree_iter_next(ZixTreeIter* i);

/**
   Return an iterator that points to the element one before `i`.
*/
ZIX_API ZixTreeIter*
zix_tree_iter_prev(ZixTreeIter* i);

/**
   @}
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* ZIX_TREE_H */
