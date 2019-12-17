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
#include "zix/tree.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

LilvUI*
lilv_ui_new(LilvWorld* world,
            LilvNode* uri,
            LilvNode* type_uri,
            LilvNode* binary_uri)
{
	assert(uri);
	assert(type_uri);
	assert(binary_uri);

	LilvUI* ui = (LilvUI*)malloc(sizeof(LilvUI));
	ui->world      = world;
	ui->uri        = uri;
	ui->binary_uri = binary_uri;

	// FIXME: kludge
	char* bundle     = lilv_strdup(lilv_node_as_string(ui->binary_uri));
	char* last_slash = strrchr(bundle, '/') + 1;
	*last_slash = '\0';
	ui->bundle_uri = lilv_new_uri(world, bundle);
	free(bundle);

	ui->classes = lilv_nodes_new();
	zix_tree_insert((ZixTree*)ui->classes, type_uri, NULL);

	return ui;
}

void
lilv_ui_free(LilvUI* ui)
{
	lilv_node_free(ui->uri);
	lilv_node_free(ui->bundle_uri);
	lilv_node_free(ui->binary_uri);
	lilv_nodes_free(ui->classes);
	free(ui);
}

LILV_API const LilvNode*
lilv_ui_get_uri(const LilvUI* ui)
{
	return ui->uri;
}

LILV_API unsigned
lilv_ui_is_supported(const LilvUI*       ui,
                     LilvUISupportedFunc supported_func,
                     const LilvNode*     container_type,
                     const LilvNode**    ui_type)
{
	const LilvNodes* classes = lilv_ui_get_classes(ui);
	LILV_FOREACH(nodes, c, classes) {
		const LilvNode* type = lilv_nodes_get(classes, c);
		const unsigned  q    = supported_func(lilv_node_as_uri(container_type),
		                                      lilv_node_as_uri(type));
		if (q) {
			if (ui_type) {
				*ui_type = type;
			}
			return q;
		}
	}

	return 0;
}

LILV_API const LilvNodes*
lilv_ui_get_classes(const LilvUI* ui)
{
	return ui->classes;
}

LILV_API bool
lilv_ui_is_a(const LilvUI* ui, const LilvNode* class_uri)
{
	return lilv_nodes_contains(ui->classes, class_uri);
}

LILV_API const LilvNode*
lilv_ui_get_bundle_uri(const LilvUI* ui)
{
	return ui->bundle_uri;
}

LILV_API const LilvNode*
lilv_ui_get_binary_uri(const LilvUI* ui)
{
	return ui->binary_uri;
}
