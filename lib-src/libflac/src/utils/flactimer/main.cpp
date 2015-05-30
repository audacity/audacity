/* flactimer - Runs a command and prints timing information
 * Copyright (C) 2007-2009  Josh Coalson
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
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <windows.h>
#include "share/compat.h"
#include "share/safe_str.h"

static inline uint64_t time2nsec(const FILETIME &t)
{
	uint64_t n = t.dwHighDateTime;
	n <<= 32;
	n |= (uint64_t)t.dwLowDateTime;
	return n * 100;
}

static void printtime(FILE *fout, uint64_t nsec, uint64_t total)
{
	unsigned pct = (unsigned)(100.0 * ((double)nsec / (double)total));
	uint64_t msec = nsec / 1000000; nsec -= msec * 1000000;
	uint64_t sec = msec / 1000; msec -= sec * 1000;
	uint64_t min = sec / 60; sec -= min * 60;
	uint64_t hour = min / 60; min -= hour * 60;
	fprintf(fout, " %5u.%03u = %02u:%02u:%02u.%03u = %3u%%\n",
		(unsigned)((hour*60+min)*60+sec),
		(unsigned)msec,
		(unsigned)hour,
		(unsigned)min,
		(unsigned)sec,
		(unsigned)msec,
		pct
	);
}

int main(int argc, char *argv[])
{
	const char *usage = "usage: flactimer [-1 | -2 | -o outputfile] command\n";
	FILE *fout = stderr;

	if(argc == 1 || (argc > 1 && 0 == strcmp(argv[1], "-h"))) {
		fprintf(stderr, usage);
		return 0;
	}
	argv++;
	argc--;
	if(0 == strcmp(argv[0], "-1") || 0 == strcmp(argv[0], "/1")) {
		fout = stdout;
		argv++;
		argc--;
	}
	else if(0 == strcmp(argv[0], "-2") || 0 == strcmp(argv[0], "/2")) {
		fout = stdout;
		argv++;
		argc--;
	}
	else if(0 == strcmp(argv[0], "-o")) {
		if(argc < 2) {
			fprintf(stderr, usage);
			return 1;
		}
		fout = fopen(argv[1], "w");
		if(!fout) {
			fprintf(fout, "ERROR opening file %s for writing\n", argv[1]);
			return 1;
		}
		argv += 2;
		argc -= 2;
	}
	if(argc <= 0) {
		fprintf(fout, "ERROR, no command!\n\n");
		fprintf(fout, usage);
		fclose(fout);
		return 1;
	}

	// improvement: double-quote all args
	int i, n = 0;
	for(i = 0; i < argc; i++) {
		if(i > 0)
			n++;
		n += strlen(argv[i]);
	}
	char *args = (char*)malloc(n+1);
	if(!args) {
		fprintf(fout, "ERROR, no memory\n");
		fclose(fout);
		return 1;
	}
	args[0] = '\0';
	for(i = 0; i < argc; i++) {
		if(i > 0)
			safe_strncat(args, " ", sizeof(args));
		safe_strncat(args, argv[i], sizeof(args));
	}

	//fprintf(stderr, "@@@ cmd=[%s] args=[%s]\n", argv[0], args);

	STARTUPINFO si;
	GetStartupInfo(&si);

	DWORD wallclock_msec = GetTickCount();

	PROCESS_INFORMATION pi;
	BOOL ok = CreateProcess(
		argv[0], // lpApplicationName
		args, // lpCommandLine
		NULL, // lpProcessAttributes
		NULL, // lpThreadAttributes
		FALSE, // bInheritHandles
		0, // dwCreationFlags
		NULL, // lpEnvironment
		NULL, // lpCurrentDirectory
		&si, // lpStartupInfo (inherit from this proc?)
		&pi // lpProcessInformation
	);

	if(!ok) {
		fprintf(fout, "ERROR running command\n");
		free(args); //@@@ ok to free here or have to wait to wait till process is reaped?
		fclose(fout);
		return 1;
	}

	//fprintf(stderr, "@@@ waiting...\n");
	WaitForSingleObject(pi.hProcess, INFINITE);
	//fprintf(stderr, "@@@ done\n");

	wallclock_msec = GetTickCount() - wallclock_msec;

	FILETIME creation_time;
	FILETIME exit_time;
	FILETIME kernel_time;
	FILETIME user_time;
	if(!GetProcessTimes(pi.hProcess, &creation_time, &exit_time, &kernel_time, &user_time)) {
		fprintf(fout, "ERROR getting time info\n");
		free(args); //@@@ ok to free here or have to wait to wait till process is reaped?
		fclose(fout);
		return 1;
	}
	uint64_t kernel_nsec = time2nsec(kernel_time);
	uint64_t user_nsec = time2nsec(user_time);

	fprintf(fout, "Kernel Time  = "); printtime(fout, kernel_nsec, (uint64_t)wallclock_msec * 1000000);
	fprintf(fout, "User Time    = "); printtime(fout, user_nsec, (uint64_t)wallclock_msec * 1000000);
	fprintf(fout, "Process Time = "); printtime(fout, kernel_nsec+user_nsec, (uint64_t)wallclock_msec * 1000000);
	fprintf(fout, "Global Time  = "); printtime(fout, (uint64_t)wallclock_msec * 1000000, (uint64_t)wallclock_msec * 1000000);

	CloseHandle(pi.hThread);
	CloseHandle(pi.hProcess);

	free(args); //@@@ always causes crash, maybe CreateProcess takes ownership?
	fclose(fout);
	return 0;
}
