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

#include <stdlib.h>

/** Ownership of value and label is taken */
LilvScalePoint*
lilv_scale_point_new(LilvNode* value, LilvNode* label)
{
	LilvScalePoint* point = (LilvScalePoint*)malloc(sizeof(LilvScalePoint));
	point->value = value;
	point->label = label;
	return point;
}

void
lilv_scale_point_free(LilvScalePoint* point)
{
	if (point) {
		lilv_node_free(point->value);
		lilv_node_free(point->label);
		free(point);
	}
}

LILV_API const LilvNode*
lilv_scale_point_get_value(const LilvScalePoint* point)
{
	return point->value;
}

LILV_API const LilvNode*
lilv_scale_point_get_label(const LilvScalePoint* point)
{
	return point->label;
}
