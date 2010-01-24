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

#define _XOPEN_SOURCE 500

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <locale.h>
#include <raptor.h>
#include <slv2/value.h>
#include "slv2_internal.h"


/* private */
SLV2Value
slv2_value_new(SLV2World world, SLV2ValueType type, const char* str)
{
	SLV2Value val = (SLV2Value)malloc(sizeof(struct _SLV2Value));
	val->type = type;

	if (type == SLV2_VALUE_URI) {
		val->val.uri_val = librdf_new_uri(world->world, (const unsigned char*)str);
		if (val->val.uri_val)
			val->str_val = (char*)librdf_uri_as_string(val->val.uri_val);
		else
			return NULL;
	} else {
		val->str_val = strdup(str);
	}

	slv2_value_set_numerics_from_string(val);

	return val;
}


/* private */
void
slv2_value_set_numerics_from_string(SLV2Value val)
{
	if (!val)
		return;

	// FIXME: locale kludges to work around librdf bug
	char* locale = strdup(setlocale(LC_NUMERIC, NULL));

	if (val->type == SLV2_VALUE_INT) {
		char* endptr = 0;
		setlocale(LC_NUMERIC, "POSIX");
		val->val.int_val = strtol(val->str_val, &endptr, 10);
		setlocale(LC_NUMERIC, locale);
	} else if (val->type == SLV2_VALUE_FLOAT) {
		char* endptr = 0;
		setlocale(LC_NUMERIC, "POSIX");
		val->val.float_val = strtod(val->str_val, &endptr);
		setlocale(LC_NUMERIC, locale);
	}
	
	free(locale);
}


/* private */
SLV2Value
slv2_value_new_librdf_node(SLV2World world, librdf_node* node)
{
	SLV2Value val = (SLV2Value)malloc(sizeof(struct _SLV2Value));
	val->type = SLV2_VALUE_STRING;
	val->str_val = NULL;
	
	librdf_uri* datatype_uri = NULL;

	switch (librdf_node_get_type(node)) {
	case LIBRDF_NODE_TYPE_RESOURCE:
		val->type = SLV2_VALUE_URI;
		val->val.uri_val = librdf_node_get_uri(node);
		val->str_val = (char*)librdf_uri_as_string(val->val.uri_val);
		break;
	case LIBRDF_NODE_TYPE_LITERAL:
		datatype_uri = librdf_node_get_literal_value_datatype_uri(node);
		if (datatype_uri) {
			if (librdf_uri_equals(datatype_uri, librdf_node_get_uri(world->xsd_integer_node)))
				val->type = SLV2_VALUE_INT;
			else if (librdf_uri_equals(datatype_uri, librdf_node_get_uri(world->xsd_decimal_node)))
				val->type = SLV2_VALUE_FLOAT;
			else
				fprintf(stderr, "Unknown datatype %s\n", librdf_uri_as_string(datatype_uri));
		}
		val->str_val = strdup((char*)librdf_node_get_literal_value(node));
		break;
	case LIBRDF_NODE_TYPE_BLANK:
	case LIBRDF_NODE_TYPE_UNKNOWN:
	default:
		fprintf(stderr, "slv2_value_new_librdf_node error: Unknown node type.");
		free(val);
		val = NULL;
		break;
	}
	
	slv2_value_set_numerics_from_string(val);
	return val;
}


/* private */
SLV2Value
slv2_value_new_librdf_uri(SLV2World world, librdf_uri* uri)
{
	if (!uri)
		return NULL;

	SLV2Value val = (SLV2Value)malloc(sizeof(struct _SLV2Value));
	val->type = SLV2_VALUE_URI;
	val->val.uri_val = librdf_new_uri_from_uri(uri);
	val->str_val = (char*)librdf_uri_as_string(val->val.uri_val);
	return val;
}


SLV2Value
slv2_value_new_uri(SLV2World world, const char* uri)
{
	return slv2_value_new(world, SLV2_VALUE_URI, uri);
}


SLV2Value
slv2_value_duplicate(SLV2Value val)
{
	SLV2Value result = (SLV2Value)malloc(sizeof(struct _SLV2Value));
	result->type = val->type;

	if (val->type == SLV2_VALUE_URI) {
		result->val.uri_val = librdf_new_uri_from_uri(val->val.uri_val);
		result->str_val = (char*)librdf_uri_as_string(val->val.uri_val);
	} else {
		result->str_val = strdup(val->str_val);
		result->val = val->val;
	}

	return result;
}


void
slv2_value_free(SLV2Value val)
{
	if (val) {
		if (val->type == SLV2_VALUE_URI)
			librdf_free_uri(val->val.uri_val);
		else
			free(val->str_val);

		free(val);
	}
}


bool
slv2_value_equals(SLV2Value value, SLV2Value other)
{
	if (value == NULL && other == NULL)
		return true;
	else if (value == NULL || other == NULL)
		return false;
	else if (value->type != other->type)
		return false;

	switch (value->type) {
	case SLV2_VALUE_URI:
		return (librdf_uri_equals(value->val.uri_val, other->val.uri_val) != 0);
	case SLV2_VALUE_QNAME:
	case SLV2_VALUE_STRING:
		return ! strcmp(value->str_val, other->str_val);
	case SLV2_VALUE_INT:
		return (value->val.int_val == other->val.int_val);
	case SLV2_VALUE_FLOAT:
		return (value->val.float_val == other->val.float_val);
	}

	return false; /* shouldn't get here */
}


char*
slv2_value_get_turtle_token(SLV2Value value)
{
	size_t len    = 0;
	char*  result = NULL;
	char*  locale = strdup(setlocale(LC_NUMERIC, NULL));
		
	// FIXME: locale kludges to work around librdf bug

	switch (value->type) {
	case SLV2_VALUE_URI:
		len = strlen(value->str_val) + 3;
		result = calloc(len, sizeof(char));
		snprintf(result, len, "<%s>", value->str_val);
		break;
	case SLV2_VALUE_QNAME:
	case SLV2_VALUE_STRING:
		result = strdup(value->str_val);
		break;
	case SLV2_VALUE_INT:
	    // INT64_MAX is 9223372036854775807 (19 digits) + 1 for sign
		len = 20;
		result = calloc(len, sizeof(char));
		setlocale(LC_NUMERIC, "POSIX");
		snprintf(result, len, "%d", value->val.int_val);
		setlocale(LC_NUMERIC, locale);
		break;
	case SLV2_VALUE_FLOAT:
		len = 20; // FIXME: proper maximum value?
		result = calloc(len, sizeof(char));
		setlocale(LC_NUMERIC, "POSIX");
		snprintf(result, len, ".1%f", value->val.float_val);
		setlocale(LC_NUMERIC, locale);
		break;
	}

	free(locale);
	
	return result;
}


bool
slv2_value_is_uri(SLV2Value value)
{
	return (value && value->type == SLV2_VALUE_URI);
}


const char*
slv2_value_as_uri(SLV2Value value)
{
	assert(slv2_value_is_uri(value));
	return value->str_val;
}

	
/* private */
librdf_uri*
slv2_value_as_librdf_uri(SLV2Value value)
{
	assert(slv2_value_is_uri(value));
	return value->val.uri_val;
}


bool
slv2_value_is_literal(SLV2Value value)
{
	// No blank nodes
	return (value && value->type != SLV2_VALUE_URI);
}


bool
slv2_value_is_string(SLV2Value value)
{
	return (value && value->type == SLV2_VALUE_STRING);
}


const char*
slv2_value_as_string(SLV2Value value)
{
	return value->str_val;
}


bool
slv2_value_is_int(SLV2Value value)
{
	return (value && value->type == SLV2_VALUE_INT);
}


int
slv2_value_as_int(SLV2Value value)
{
	assert(value);
	assert(slv2_value_is_int(value));
	return value->val.int_val;
}


bool
slv2_value_is_float(SLV2Value value)
{
	return (value && value->type == SLV2_VALUE_FLOAT);
}


float
slv2_value_as_float(SLV2Value value)
{
	assert(slv2_value_is_float(value) || slv2_value_is_int(value));
	if (slv2_value_is_float(value))
		return value->val.float_val;
	else // slv2_value_is_int(value)
		return (float)value->val.int_val;
}

