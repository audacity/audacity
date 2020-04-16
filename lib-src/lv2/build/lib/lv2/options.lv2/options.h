/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

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

/**
   @defgroup options Options

   Instantiation time options, see <http://lv2plug.in/ns/ext/options> for
   details.

   @{
*/

#ifndef LV2_OPTIONS_H
#define LV2_OPTIONS_H

#include "lv2/core/lv2.h"
#include "lv2/urid/urid.h"

#include <stdint.h>

#define LV2_OPTIONS_URI    "http://lv2plug.in/ns/ext/options"  ///< http://lv2plug.in/ns/ext/options
#define LV2_OPTIONS_PREFIX LV2_OPTIONS_URI "#"                 ///< http://lv2plug.in/ns/ext/options#

#define LV2_OPTIONS__Option          LV2_OPTIONS_PREFIX "Option"           ///< http://lv2plug.in/ns/ext/options#Option
#define LV2_OPTIONS__interface       LV2_OPTIONS_PREFIX "interface"        ///< http://lv2plug.in/ns/ext/options#interface
#define LV2_OPTIONS__options         LV2_OPTIONS_PREFIX "options"          ///< http://lv2plug.in/ns/ext/options#options
#define LV2_OPTIONS__requiredOption  LV2_OPTIONS_PREFIX "requiredOption"   ///< http://lv2plug.in/ns/ext/options#requiredOption
#define LV2_OPTIONS__supportedOption LV2_OPTIONS_PREFIX "supportedOption"  ///< http://lv2plug.in/ns/ext/options#supportedOption

#ifdef __cplusplus
extern "C" {
#endif

/**
   The context of an Option, which defines the subject it applies to.
*/
typedef enum {
	/**
	   This option applies to the instance itself.  The subject must be
	   ignored.
	*/
	LV2_OPTIONS_INSTANCE,

	/**
	   This option applies to some named resource.  The subject is a URI mapped
	   to an integer (a LV2_URID, like the key)
	*/
	LV2_OPTIONS_RESOURCE,

	/**
	   This option applies to some blank node.  The subject is a blank node
	   identifier, which is valid only within the current local scope.
	*/
	LV2_OPTIONS_BLANK,

	/**
	   This option applies to a port on the instance.  The subject is the
	   port's index.
	*/
	LV2_OPTIONS_PORT
} LV2_Options_Context;

/**
   An option.

   This is a property with a subject, also known as a triple or statement.

   This struct is useful anywhere a statement needs to be passed where no
   memory ownership issues are present (since the value is a const pointer).

   Options can be passed to an instance via the feature LV2_OPTIONS__options
   with data pointed to an array of options terminated by a zeroed option, or
   accessed/manipulated using LV2_Options_Interface.
*/
typedef struct _LV2_Options_Option {
	LV2_Options_Context context;  /**< Context (type of subject). */
	uint32_t            subject;  /**< Subject. */
	LV2_URID            key;      /**< Key (property). */
	uint32_t            size;     /**< Size of value in bytes. */
	LV2_URID            type;     /**< Type of value (datatype). */
	const void*         value;    /**< Pointer to value (object). */
} LV2_Options_Option;

/** A status code for option functions. */
typedef enum {
	LV2_OPTIONS_SUCCESS         = 0,       /**< Completed successfully. */
	LV2_OPTIONS_ERR_UNKNOWN     = 1,       /**< Unknown error. */
	LV2_OPTIONS_ERR_BAD_SUBJECT = 1 << 1,  /**< Invalid/unsupported subject. */
	LV2_OPTIONS_ERR_BAD_KEY     = 1 << 2,  /**< Invalid/unsupported key. */
	LV2_OPTIONS_ERR_BAD_VALUE   = 1 << 3   /**< Invalid/unsupported value. */
} LV2_Options_Status;

/**
   Interface for dynamically setting options (LV2_OPTIONS__interface).
*/
typedef struct _LV2_Options_Interface {
	/**
	   Get the given options.

	   Each element of the passed options array MUST have type, subject, and
	   key set.  All other fields (size, type, value) MUST be initialised to
	   zero, and are set to the option value if such an option is found.

	   This function is in the "instantiation" LV2 threading class, so no other
	   instance functions may be called concurrently.

	   @return Bitwise OR of LV2_Options_Status values.
	*/
	uint32_t (*get)(LV2_Handle          instance,
	                LV2_Options_Option* options);

	/**
	   Set the given options.

	   This function is in the "instantiation" LV2 threading class, so no other
	   instance functions may be called concurrently.

	   @return Bitwise OR of LV2_Options_Status values.
	*/
	uint32_t (*set)(LV2_Handle                instance,
	                const LV2_Options_Option* options);
} LV2_Options_Interface;

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_OPTIONS_H */

/**
   @}
*/
