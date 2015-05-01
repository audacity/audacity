/* metaflac - Command-line FLAC metadata editor
 * Copyright (C) 2001-2009  Josh Coalson
 * Copyright (C) 2011-2014  Xiph.Org Foundation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "operations.h"
#include "options.h"
#include <locale.h>
#include <stdlib.h>
#include "share/compat.h"

int main(int argc, char *argv[])
{
	CommandLineOptions options;
	int ret = 0;

#ifdef __EMX__
	_response(&argc, &argv);
	_wildcard(&argc, &argv);
#endif
#ifdef _WIN32
	if (get_utf8_argv(&argc, &argv) != 0) {
		fputs("ERROR: failed to convert command line parameters to UTF-8\n", stderr);
		return 1;
	}
#endif

#ifdef _WIN32
	{
		const char *var;
		var = getenv("LC_ALL");
		if (!var)
			var = getenv("LC_NUMERIC");
		if (!var)
			var = getenv("LANG");
		if (!var || strcmp(var, "C") != 0)
			setlocale(LC_ALL, "");
	}
#else
	setlocale(LC_ALL, "");
#endif
	init_options(&options);

	if ((ret = parse_options(argc, argv, &options)) == 0)
		ret = !do_operations(&options);
	else
		ret = 1;

	free_options(&options);

	return ret;
}
