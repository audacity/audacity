/*
  Copyright 2018 David Robillard <http://drobilla.net>

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

#ifndef LV2_CORE_ATTRIBUTES_H
#define LV2_CORE_ATTRIBUTES_H

/**
   @defgroup attributes Attributes

   Macros for source code attributes.

   @{
*/

#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
#define LV2_DEPRECATED __attribute__((__deprecated__))
#else
#define LV2_DEPRECATED
#endif

#if defined(__clang__)
#define LV2_DISABLE_DEPRECATION_WARNINGS \
	_Pragma("clang diagnostic push") \
	_Pragma("clang diagnostic ignored \"-Wdeprecated-declarations\"")
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#define LV2_DISABLE_DEPRECATION_WARNINGS \
	_Pragma("GCC diagnostic push") \
	_Pragma("GCC diagnostic ignored \"-Wdeprecated-declarations\"")
#else
#define LV2_DISABLE_DEPRECATION_WARNINGS
#endif

#if defined(__clang__)
#define LV2_RESTORE_WARNINGS _Pragma("clang diagnostic pop")
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#define LV2_RESTORE_WARNINGS _Pragma("GCC diagnostic pop")
#else
#define LV2_RESTORE_WARNINGS
#endif

/**
   @}
*/

#endif /* LV2_CORE_ATTRIBUTES_H */
