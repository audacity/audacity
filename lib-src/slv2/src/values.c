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

#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <raptor.h>
#include <slv2/values.h>
#include "slv2_internal.h"


SLV2Values
slv2_values_new()
{
	return raptor_new_sequence((void (*)(void*))&slv2_value_free, NULL);
}


void
slv2_values_free(SLV2Values list)
{
	if (list)
		raptor_free_sequence(list);
}


unsigned
slv2_values_size(SLV2Values list)
{
	return (list ? raptor_sequence_size(list) : 0);
}


SLV2Value
slv2_values_get_at(SLV2Values list, unsigned index)
{
	if (index > INT_MAX)
		return NULL;
	else
		return (SLV2Value)raptor_sequence_get_at(list, (int)index);
}


/* private */
void
slv2_values_set_at(SLV2Values list, unsigned index, void* value)
{
	if (index <= INT_MAX)
		raptor_sequence_set_at(list, index, value);
}


bool
slv2_values_contains(SLV2Values list, SLV2Value value)
{
	for (unsigned i=0; i < slv2_values_size(list); ++i)
		if (slv2_value_equals(slv2_values_get_at(list, i), value))
			return true;
	
	return false;
}

