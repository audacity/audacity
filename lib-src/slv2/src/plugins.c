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
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <librdf.h>
#include <slv2/types.h>
#include <slv2/plugin.h>
#include <slv2/plugins.h>
#include <slv2/values.h>
#include <slv2/util.h>
#include "slv2_internal.h"

	
SLV2Plugins
slv2_plugins_new()
{
	//return raptor_new_sequence((void (*)(void*))&slv2_plugin_free, NULL);
	return raptor_new_sequence(NULL, NULL);
}


void
slv2_plugins_free(SLV2World world, SLV2Plugins list)
{
	if (list && list != world->plugins)
		raptor_free_sequence(list);
}

#if 0
void
slv2_plugins_filter(SLV2Plugins dest, SLV2Plugins source, bool (*include)(SLV2Plugin))
{
	assert(dest);

	for (int i=0; i < raptor_sequence_size(source); ++i) {
		SLV2Plugin p = raptor_sequence_get_at(source, i);
		if (include(p))
			raptor_sequence_push(dest, slv2_plugin_duplicate(p));
	}
}
#endif


unsigned
slv2_plugins_size(SLV2Plugins list)
{
	return (list ? raptor_sequence_size(list) : 0);
}


SLV2Plugin
slv2_plugins_get_by_uri(SLV2Plugins list, SLV2Value uri)
{
	// good old fashioned binary search
	
	int lower = 0;
	int upper = raptor_sequence_size(list) - 1;
	int i;
	
	while (upper >= lower) {
		i = lower + ((upper - lower) / 2);

		SLV2Plugin p = raptor_sequence_get_at(list, i);

		const int cmp = strcmp(slv2_value_as_uri(slv2_plugin_get_uri(p)),
		                       slv2_value_as_uri(uri));

		if (cmp == 0)
			return p;
		else if (cmp > 0)
			upper = i - 1;
		else
			lower = i + 1;
	}

	return NULL;
}


SLV2Plugin
slv2_plugins_get_at(SLV2Plugins list, unsigned index)
{	
	if (index > INT_MAX)
		return NULL;
	else
		return (SLV2Plugin)raptor_sequence_get_at(list, (int)index);
}

