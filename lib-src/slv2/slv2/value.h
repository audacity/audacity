/* SLV2
 * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
 *  
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef __SLV2_VALUE_H__
#define __SLV2_VALUE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <slv2/types.h>

/** \addtogroup slv2_data
 * @{
 */


/** Create a new URI value.
 *
 * Returned value must be freed by called with slv2_value_free.
 */
SLV2Value
slv2_value_new_uri(SLV2World world, const char* uri);


/** Free an SLV2Value.
 */
void
slv2_value_free(SLV2Value val);


/** Duplicate an SLV2Value.
 */
SLV2Value
slv2_value_duplicate(SLV2Value val);


/** Return whether two values are equivalent.
 */
bool
slv2_value_equals(SLV2Value value, SLV2Value other);


/** Return this value as a Turtle/SPARQL token.
 * Examples:
 * 	<http://example.org/foo>
 * 	doap:name
 * 	"this is a string"
 * 	1.0
 * 	1
 *
 * Returned string is newly allocation and must be freed by caller.
 */
char*
slv2_value_get_turtle_token(SLV2Value value);


/** Return whether the value is a URI (resource).
 *
 * Time = O(1)
 */
bool
slv2_value_is_uri(SLV2Value value);


/** Return this value as a URI string, e.g. "http://example.org/foo".
 * 
 * Valid to call only if slv2_value_is_uri(\a value) returns true.
 * Returned value is owned by \a value and must not be freed by caller.
 * 
 * Time = O(1)
 */
const char*
slv2_value_as_uri(SLV2Value value);


#if 0
/** Return whether the value is a QName ("qualified name", a prefixed URI).
 *
 * A QName will return true for both this, and slv2_value_is_uri.
 * slv2_value_as_uri and slv2_value_as_qname will both return appropriately.
 *
 * Time = O(1)
 */
bool
slv2_value_is_qname(SLV2Value value);


/** Return this value as a QName string, e.g. "lv2:Plugin".
 * 
 * Valid to call only if slv2_value_is_qname(\a value) returns true.
 * Returned value is owned by \a value and must not be freed by caller.
 * 
 * Time = O(1)
 */
const char*
slv2_value_as_qname(SLV2Value value);
#endif


/** Return whether this value is a literal (i.e. not a URI).
 *
 * Returns true if \a value is a string or numeric value.
 *
 * Time = O(1)
 */
bool
slv2_value_is_literal(SLV2Value value);


/** Return whether this value is a string literal.
 *
 * Returns true if \a value is a string (but not  numeric) value.
 *
 * Time = O(1)
 */
bool
slv2_value_is_string(SLV2Value value);


/** Return whether this value is a string literal.
 *
 * Time = O(1)
 */
const char*
slv2_value_as_string(SLV2Value value);


/** Return whether this value is a decimal literal.
 *
 * Time = O(1)
 */
bool
slv2_value_is_float(SLV2Value value);


/** Return \a value as a float.
 * 
 * Valid to call only if slv2_value_is_float(\a value) or
 * slv2_value_is_int(\a value) returns true.
 *
 * Time = O(1)
 */
float
slv2_value_as_float(SLV2Value value);


/** Return whether this value is an integer literal.
 * 
 * Time = O(1)
 */
bool
slv2_value_is_int(SLV2Value value);


/** Return \a value as an integer.
 * 
 * Valid to call only if slv2_value_is_int(\a value) returns true.
 *
 * Time = O(1)
 */
int
slv2_value_as_int(SLV2Value value);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_VALUE_H__ */
