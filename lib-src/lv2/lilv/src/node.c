/*
  Copyright 2007-2019 David Robillard <http://drobilla.net>

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

#include "lilv_internal.h"

#include "lilv/lilv.h"
#include "serd/serd.h"
#include "sord/sord.h"

#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
lilv_node_set_numerics_from_string(LilvNode* val)
{
	const char* str = (const char*)sord_node_get_string(val->node);

	switch (val->type) {
	case LILV_VALUE_URI:
	case LILV_VALUE_BLANK:
	case LILV_VALUE_STRING:
	case LILV_VALUE_BLOB:
		break;
	case LILV_VALUE_INT:
		val->val.int_val = strtol(str, NULL, 10);
		break;
	case LILV_VALUE_FLOAT:
		val->val.float_val = serd_strtod(str, NULL);
		break;
	case LILV_VALUE_BOOL:
		val->val.bool_val = !strcmp(str, "true");
		break;
	}
}

/** Note that if `type` is numeric or boolean, the returned value is corrupt
 * until lilv_node_set_numerics_from_string is called.  It is not
 * automatically called from here to avoid overhead and imprecision when the
 * exact string value is known.
 */
LilvNode*
lilv_node_new(LilvWorld* world, LilvNodeType type, const char* str)
{
	LilvNode* val = (LilvNode*)malloc(sizeof(LilvNode));
	val->world = world;
	val->type  = type;

	const uint8_t* ustr = (const uint8_t*)str;
	switch (type) {
	case LILV_VALUE_URI:
		val->node = sord_new_uri(world->world, ustr);
		break;
	case LILV_VALUE_BLANK:
		val->node = sord_new_blank(world->world, ustr);
		break;
	case LILV_VALUE_STRING:
		val->node = sord_new_literal(world->world, NULL, ustr, NULL);
		break;
	case LILV_VALUE_INT:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_integer, ustr, NULL);
		break;
	case LILV_VALUE_FLOAT:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_decimal, ustr, NULL);
		break;
	case LILV_VALUE_BOOL:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_boolean, ustr, NULL);
		break;
	case LILV_VALUE_BLOB:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_base64Binary, ustr, NULL);
		break;
	}

	if (!val->node) {
		free(val);
		return NULL;
	}

	return val;
}

/** Create a new LilvNode from `node`, or return NULL if impossible */
LilvNode*
lilv_node_new_from_node(LilvWorld* world, const SordNode* node)
{
	if (!node) {
		return NULL;
	}

	LilvNode*    result       = NULL;
	SordNode*    datatype_uri = NULL;
	LilvNodeType type         = LILV_VALUE_STRING;

	switch (sord_node_get_type(node)) {
	case SORD_URI:
		result        = (LilvNode*)malloc(sizeof(LilvNode));
		result->world = world;
		result->type  = LILV_VALUE_URI;
		result->node  = sord_node_copy(node);
		break;
	case SORD_BLANK:
		result        = (LilvNode*)malloc(sizeof(LilvNode));
		result->world = world;
		result->type  = LILV_VALUE_BLANK;
		result->node  = sord_node_copy(node);
		break;
	case SORD_LITERAL:
		datatype_uri = sord_node_get_datatype(node);
		if (datatype_uri) {
			if (sord_node_equals(datatype_uri, world->uris.xsd_boolean)) {
				type = LILV_VALUE_BOOL;
			} else if (sord_node_equals(datatype_uri, world->uris.xsd_decimal) ||
			           sord_node_equals(datatype_uri, world->uris.xsd_double)) {
				type = LILV_VALUE_FLOAT;
			} else if (sord_node_equals(datatype_uri, world->uris.xsd_integer)) {
				type = LILV_VALUE_INT;
			} else if (sord_node_equals(datatype_uri,
			                            world->uris.xsd_base64Binary)) {
				type = LILV_VALUE_BLOB;
			} else {
				LILV_ERRORF("Unknown datatype `%s'\n",
				            sord_node_get_string(datatype_uri));
			}
		}
		result = lilv_node_new(
			world, type, (const char*)sord_node_get_string(node));
		lilv_node_set_numerics_from_string(result);
		break;
	}

	return result;
}

LILV_API LilvNode*
lilv_new_uri(LilvWorld* world, const char* uri)
{
	return lilv_node_new(world, LILV_VALUE_URI, uri);
}

LILV_API LilvNode*
lilv_new_file_uri(LilvWorld* world, const char* host, const char* path)
{
	char*    abs_path = lilv_path_absolute(path);
	SerdNode s        = serd_node_new_file_uri(
		(const uint8_t*)abs_path, (const uint8_t*)host, NULL, true);

	LilvNode* ret = lilv_node_new(world, LILV_VALUE_URI, (const char*)s.buf);
	serd_node_free(&s);
	free(abs_path);
	return ret;
}

LILV_API LilvNode*
lilv_new_string(LilvWorld* world, const char* str)
{
	return lilv_node_new(world, LILV_VALUE_STRING, str);
}

LILV_API LilvNode*
lilv_new_int(LilvWorld* world, int val)
{
	char str[32];
	snprintf(str, sizeof(str), "%d", val);
	LilvNode* ret = lilv_node_new(world, LILV_VALUE_INT, str);
	ret->val.int_val = val;
	return ret;
}

LILV_API LilvNode*
lilv_new_float(LilvWorld* world, float val)
{
	char str[32];
	snprintf(str, sizeof(str), "%f", val);
	LilvNode* ret = lilv_node_new(world, LILV_VALUE_FLOAT, str);
	ret->val.float_val = val;
	return ret;
}

LILV_API LilvNode*
lilv_new_bool(LilvWorld* world, bool val)
{
	LilvNode* ret = lilv_node_new(world, LILV_VALUE_BOOL,
	                              val ? "true" : "false");
	ret->val.bool_val = val;
	return ret;
}

LILV_API LilvNode*
lilv_node_duplicate(const LilvNode* val)
{
	if (!val) {
		return NULL;
	}

	LilvNode* result = (LilvNode*)malloc(sizeof(LilvNode));
	result->world = val->world;
	result->node  = sord_node_copy(val->node);
	result->val   = val->val;
	result->type  = val->type;
	return result;
}

LILV_API void
lilv_node_free(LilvNode* val)
{
	if (val) {
		sord_node_free(val->world->world, val->node);
		free(val);
	}
}

LILV_API bool
lilv_node_equals(const LilvNode* value, const LilvNode* other)
{
	if (value == NULL && other == NULL) {
		return true;
	} else if (value == NULL || other == NULL) {
		return false;
	} else if (value->type != other->type) {
		return false;
	}

	switch (value->type) {
	case LILV_VALUE_URI:
	case LILV_VALUE_BLANK:
	case LILV_VALUE_STRING:
	case LILV_VALUE_BLOB:
		return sord_node_equals(value->node, other->node);
	case LILV_VALUE_INT:
		return (value->val.int_val == other->val.int_val);
	case LILV_VALUE_FLOAT:
		return (value->val.float_val == other->val.float_val);
	case LILV_VALUE_BOOL:
		return (value->val.bool_val == other->val.bool_val);
	}

	return false; /* shouldn't get here */
}

LILV_API char*
lilv_node_get_turtle_token(const LilvNode* value)
{
	const char* str    = (const char*)sord_node_get_string(value->node);
	size_t      len    = 0;
	char*       result = NULL;
	SerdNode    node;

	switch (value->type) {
	case LILV_VALUE_URI:
		len    = strlen(str) + 3;
		result = (char*)calloc(len, 1);
		snprintf(result, len, "<%s>", str);
		break;
	case LILV_VALUE_BLANK:
		len    = strlen(str) + 3;
		result = (char*)calloc(len, 1);
		snprintf(result, len, "_:%s", str);
		break;
	case LILV_VALUE_STRING:
	case LILV_VALUE_BOOL:
	case LILV_VALUE_BLOB:
		result = lilv_strdup(str);
		break;
	case LILV_VALUE_INT:
		node   = serd_node_new_integer(value->val.int_val);
		result = lilv_strdup((char*)node.buf);
		serd_node_free(&node);
		break;
	case LILV_VALUE_FLOAT:
		node   = serd_node_new_decimal(value->val.float_val, 8);
		result = lilv_strdup((char*)node.buf);
		serd_node_free(&node);
		break;
	}

	return result;
}

LILV_API bool
lilv_node_is_uri(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_URI);
}

LILV_API const char*
lilv_node_as_uri(const LilvNode* value)
{
	return (lilv_node_is_uri(value)
	        ? (const char*)sord_node_get_string(value->node)
	        : NULL);
}

LILV_API bool
lilv_node_is_blank(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_BLANK);
}

LILV_API const char*
lilv_node_as_blank(const LilvNode* value)
{
	return (lilv_node_is_blank(value)
	        ? (const char*)sord_node_get_string(value->node)
	        : NULL);
}

LILV_API bool
lilv_node_is_literal(const LilvNode* value)
{
	if (!value) {
		return false;
	}

	switch (value->type) {
	case LILV_VALUE_STRING:
	case LILV_VALUE_INT:
	case LILV_VALUE_FLOAT:
	case LILV_VALUE_BLOB:
		return true;
	default:
		return false;
	}
}

LILV_API bool
lilv_node_is_string(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_STRING);
}

LILV_API const char*
lilv_node_as_string(const LilvNode* value)
{
	return value ? (const char*)sord_node_get_string(value->node) : NULL;
}

LILV_API bool
lilv_node_is_int(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_INT);
}

LILV_API int
lilv_node_as_int(const LilvNode* value)
{
	return lilv_node_is_int(value) ? value->val.int_val : 0;
}

LILV_API bool
lilv_node_is_float(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_FLOAT);
}

LILV_API float
lilv_node_as_float(const LilvNode* value)
{
	if (lilv_node_is_float(value)) {
		return value->val.float_val;
	} else if (lilv_node_is_int(value)) {
		return (float)value->val.int_val;
	}
	return NAN;
}

LILV_API bool
lilv_node_is_bool(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_BOOL);
}

LILV_API bool
lilv_node_as_bool(const LilvNode* value)
{
	return lilv_node_is_bool(value) ? value->val.bool_val : false;
}

LILV_API char*
lilv_node_get_path(const LilvNode* value, char** hostname)
{
	if (lilv_node_is_uri(value)) {
		return lilv_file_uri_parse(lilv_node_as_uri(value), hostname);
	}
	return NULL;
}
