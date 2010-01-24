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

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <slv2/values.h>
#include "slv2_internal.h"


/* private */
SLV2UI
slv2_ui_new(SLV2World   world,
            librdf_uri* uri,
            librdf_uri* type_uri,
            librdf_uri* binary_uri)
{
	assert(uri);
	assert(type_uri);
	assert(binary_uri);

	struct _SLV2UI* ui = malloc(sizeof(struct _SLV2UI));
	ui->world = world;
	ui->uri = slv2_value_new_librdf_uri(world, uri);
	ui->binary_uri = slv2_value_new_librdf_uri(world, binary_uri);
	
	assert(ui->binary_uri);
	
	// FIXME: kludge
	char* bundle = strdup(slv2_value_as_string(ui->binary_uri));
	char* last_slash = strrchr(bundle, '/') + 1;
	*last_slash = '\0';
	ui->bundle_uri = slv2_value_new_uri(world, bundle);
	free(bundle);

	ui->classes = slv2_values_new();
	raptor_sequence_push(ui->classes, slv2_value_new_librdf_uri(world, type_uri));

	return ui;
}


/* private */
void
slv2_ui_free(SLV2UI ui)
{
	slv2_value_free(ui->uri);
	ui->uri = NULL;
	
	slv2_value_free(ui->bundle_uri);
	ui->bundle_uri = NULL;
	
	slv2_value_free(ui->binary_uri);
	ui->binary_uri = NULL;

	slv2_values_free(ui->classes);
	
	free(ui);
}


SLV2Value
slv2_ui_get_uri(SLV2UI ui)
{
	assert(ui);
	assert(ui->uri);
	return ui->uri;
}


SLV2Values
slv2_ui_get_classes(SLV2UI ui)
{
	return ui->classes;
}


bool
slv2_ui_is_a(SLV2UI ui, SLV2Value ui_class_uri)
{
	return slv2_values_contains(ui->classes, ui_class_uri);
}


SLV2Value
slv2_ui_get_bundle_uri(SLV2UI ui)
{
	assert(ui);
	assert(ui->bundle_uri);
	return ui->bundle_uri;
}


SLV2Value
slv2_ui_get_binary_uri(SLV2UI ui)
{
	assert(ui);
	assert(ui->binary_uri);
	return ui->binary_uri;
}

