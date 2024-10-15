/*
  Copyright 2011-2015 David Robillard <http://drobilla.net>

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

#ifndef ZIX_HASH_H
#define ZIX_HASH_H

#include <stddef.h>
#include <stdint.h>

#include "zix/common.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
   @addtogroup zix
   @{
   @name Hash
   @{
*/

typedef struct ZixHashImpl ZixHash;

/**
   Function for computing the hash of an element.
*/
typedef uint32_t (*ZixHashFunc)(const void* value);

/**
   Function to visit a hash element.
*/
typedef void (*ZixHashVisitFunc)(void* value,
                                 void* user_data);

/**
   Create a new hash table.

   To minimize space overhead, unlike many hash tables this stores a single
   value, not a key and a value.  Any size of value can be stored, but all the
   values in the hash table must be the same size, and the values must be safe
   to copy with memcpy.  To get key:value behaviour, simply insert a struct
   with a key and value into the hash.

   @param hash_func The hashing function.
   @param equal_func A function to test value equality.
   @param value_size The size of the values to be stored.
*/
ZIX_API ZixHash*
zix_hash_new(ZixHashFunc  hash_func,
             ZixEqualFunc equal_func,
             size_t       value_size);

/**
   Free `hash`.
*/
ZIX_API void
zix_hash_free(ZixHash* hash);

/**
   Return the number of elements in `hash`.
*/
ZIX_API size_t
zix_hash_size(const ZixHash* hash);

/**
   Insert an item into `hash`.

   If no matching value is found, ZIX_STATUS_SUCCESS will be returned, and @p
   inserted will be pointed to the copy of `value` made in the new hash node.

   If a matching value already exists, ZIX_STATUS_EXISTS will be returned, and
   `inserted` will be pointed to the existing value.

   @param hash The hash table.
   @param value The value to be inserted.
   @param inserted The copy of `value` in the hash table.
   @return ZIX_STATUS_SUCCESS, ZIX_STATUS_EXISTS, or ZIX_STATUS_NO_MEM.
*/
ZIX_API ZixStatus
zix_hash_insert(ZixHash*     hash,
                const void*  value,
                const void** inserted);

/**
   Remove an item from `hash`.

   @param hash The hash table.
   @param value The value to remove.
   @return ZIX_STATUS_SUCCES or ZIX_STATUS_NOT_FOUND.
*/
ZIX_API ZixStatus
zix_hash_remove(ZixHash*    hash,
                const void* value);

/**
   Search for an item in `hash`.

   @param hash The hash table.
   @param value The value to search for.
*/
ZIX_API const void*
zix_hash_find(const ZixHash* hash,
              const void*    value);

/**
   Call `f` on each value in `hash`.

   @param hash The hash table.
   @param f The function to call on each value.
   @param user_data The user_data parameter passed to `f`.
*/
ZIX_API void
zix_hash_foreach(ZixHash*         hash,
                 ZixHashVisitFunc f,
                 void*            user_data);

/**
   @}
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* ZIX_HASH_H */
