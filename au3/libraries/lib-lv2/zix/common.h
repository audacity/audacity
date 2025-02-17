/*
  Copyright 2011 David Robillard <http://drobilla.net>

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

#ifndef ZIX_COMMON_H
#define ZIX_COMMON_H

/**
   @addtogroup zix
   @{
*/

/** @cond */
#ifdef ZIX_SHARED
#    ifdef _WIN32
#        define ZIX_LIB_IMPORT __declspec(dllimport)
#        define ZIX_LIB_EXPORT __declspec(dllexport)
#    else
#        define ZIX_LIB_IMPORT __attribute__((visibility("default")))
#        define ZIX_LIB_EXPORT __attribute__((visibility("default")))
#    endif
#    ifdef ZIX_INTERNAL
#        define ZIX_API ZIX_LIB_EXPORT
#    else
#        define ZIX_API ZIX_LIB_IMPORT
#    endif
#    define ZIX_PRIVATE static
#elif defined(ZIX_INLINE)
#    define ZIX_API     static inline
#    define ZIX_PRIVATE static inline
#else
#    define ZIX_API
#    define ZIX_PRIVATE static
#endif
/** @endcond */

#ifdef __cplusplus
extern "C" {
#else
#    include <stdbool.h>
#endif

typedef enum {
    ZIX_STATUS_SUCCESS,
    ZIX_STATUS_ERROR,
    ZIX_STATUS_NO_MEM,
    ZIX_STATUS_NOT_FOUND,
    ZIX_STATUS_EXISTS,
    ZIX_STATUS_BAD_ARG,
    ZIX_STATUS_BAD_PERMS,
} ZixStatus;

static inline const char*
zix_strerror(const ZixStatus status)
{
    switch (status) {
    case ZIX_STATUS_SUCCESS:   return "Success";
    case ZIX_STATUS_ERROR:     return "Unknown error";
    case ZIX_STATUS_NO_MEM:    return "Out of memory";
    case ZIX_STATUS_NOT_FOUND: return "Not found";
    case ZIX_STATUS_EXISTS:    return "Exists";
    case ZIX_STATUS_BAD_ARG:   return "Bad argument";
    case ZIX_STATUS_BAD_PERMS: return "Bad permissions";
    }
    return "Unknown error";
}

/**
   Function for comparing two elements.
*/
typedef int (* ZixComparator)(const void* a, const void* b, void* user_data);

/**
   Function for testing equality of two elements.
*/
typedef bool (* ZixEqualFunc)(const void* a, const void* b);

/**
   Function to destroy an element.
*/
typedef void (* ZixDestroyFunc)(void* ptr);

/**
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* ZIX_COMMON_H */
