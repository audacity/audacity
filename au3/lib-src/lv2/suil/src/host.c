/*
  Copyright 2011-2017 David Robillard <http://drobilla.net>
  Copyright 2017 Stefan Westerfeld <stefan@space.twc.de>

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

#include "./suil_internal.h"

int    suil_argc = 0;
char** suil_argv = NULL;

SUIL_API
SuilHost*
suil_host_new(SuilPortWriteFunc       write_func,
              SuilPortIndexFunc       index_func,
              SuilPortSubscribeFunc   subscribe_func,
              SuilPortUnsubscribeFunc unsubscribe_func)
{
	SuilHost* host = (SuilHost*)calloc(1, sizeof(struct SuilHostImpl));
	host->write_func       = write_func;
	host->index_func       = index_func;
	host->subscribe_func   = subscribe_func;
	host->unsubscribe_func = unsubscribe_func;
	host->argc             = suil_argc;
	host->argv             = suil_argv;
	return host;
}

SUIL_API
void
suil_host_set_touch_func(SuilHost*     host,
                         SuilTouchFunc touch_func)
{
	host->touch_func = touch_func;
}

SUIL_API
void
suil_host_free(SuilHost* host)
{
	if (host) {
		if (host->gtk_lib) {
			dlclose(host->gtk_lib);
		}
		free(host);
	}
}

#ifdef SUIL_WITH_X11
static void
suil_load_init_module(const char* module_name)
{
	void* const lib = suil_open_module(module_name);
	if (!lib) {
		return;
	}

	SuilVoidFunc init_func = suil_dlfunc(lib, "suil_host_init");
	if (init_func) {
		(*init_func)();
	} else {
		SUIL_ERRORF("Corrupt init module %s\n", module_name);
	}

	dlclose(lib);
}
#endif

SUIL_API
void
suil_init(int* argc, char*** argv, SuilArg key, ...)
{
	suil_argc = argc ? *argc : 0;
	suil_argv = argv ? *argv : NULL;

#ifdef SUIL_WITH_X11
	suil_load_init_module("suil_x11");
#endif
}
