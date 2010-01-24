/* lv2_list - List system installed LV2 plugins.
 * Copyright (C) 2007 Dave Robillard <drobilla.net>
 *  
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include CONFIG_H_PATH
#include <stdio.h>
#include <string.h>
#include <slv2/slv2.h>


void
list_plugins(SLV2Plugins list)
{
	for (unsigned i=0; i < slv2_plugins_size(list); ++i) {
		SLV2Plugin p = slv2_plugins_get_at(list, i);
		printf("%s\n", slv2_value_as_uri(slv2_plugin_get_uri(p)));
	}
}


void
print_version()
{
	printf("lv2_list (slv2) " PACKAGE_VERSION "\n");
	printf("Copyright (C) 2007 Dave Robillard <dave@drobilla.net>\n");
	printf("License: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\n");
	printf("This is free software: you are free to change and redistribute it.\n");
	printf("There is NO WARRANTY, to the extent permitted by law.\n");
}


void
print_usage()
{
	printf("Usage: lv2_list\n");
	printf("List all installed LV2 plugins.\n");
	printf("The environment variable LV2_PATH can be used to control where\n");
	printf("this (and all other slv2 based LV2 hosts) will search for plugins.\n");
}


int
main(int argc, char** argv)
{
	if (argc > 1) {
		if (argc == 2 && !strcmp(argv[1], "--version")) {
			print_version();
			return 0;
		} else if (argc == 2 && !strcmp(argv[1], "--help")) {
			print_usage();
			return 0;
		} else {
			print_usage();
			return -1;
		}
	}

	SLV2World world = slv2_world_new();
	slv2_world_load_all(world);

	SLV2Plugins plugins = slv2_world_get_all_plugins(world);

	list_plugins(plugins);
	
	slv2_plugins_free(world, plugins);
	slv2_world_free(world);

	return 0;
}

