/* libFLAC - Free Lossless Audio Codec library
 * Copyright (C) 2013-2014  Xiph.Org Foundation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of the Xiph.org Foundation nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <sys/stat.h>
#include <sys/utime.h>
#include <io.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h> /* for WideCharToMultiByte and MultiByteToWideChar */

#include "share/win_utf8_io.h"

#define UTF8_BUFFER_SIZE 32768

static
int local_vsnprintf(char *str, size_t size, const char *fmt, va_list va)
{
	int rc;

#if defined _MSC_VER
	if (size == 0)
		return 1024;
	rc = vsnprintf_s (str, size, _TRUNCATE, fmt, va);
	if (rc < 0)
		rc = size - 1;
#elif defined __MINGW32__
	rc = __mingw_vsnprintf (str, size, fmt, va);
#else
	rc = vsnprintf (str, size, fmt, va);
#endif

	return rc;
}

static UINT win_utf8_io_codepage = CP_ACP;

/* convert WCHAR stored Unicode string to UTF-8. Caller is responsible for freeing memory */
static
char *utf8_from_wchar(const wchar_t *wstr)
{
	char *utf8str;
	int len;

	if (!wstr) return NULL;
	if ((len = WideCharToMultiByte(CP_UTF8, 0, wstr, -1, NULL, 0, NULL, NULL)) == 0) return NULL;
	if ((utf8str = (char *)malloc(++len)) == NULL) return NULL;
	if (WideCharToMultiByte(CP_UTF8, 0, wstr, -1, utf8str, len, NULL, NULL) == 0) {
		free(utf8str);
		utf8str = NULL;
	}

	return utf8str;
}

/* convert UTF-8 back to WCHAR. Caller is responsible for freeing memory */
static
wchar_t *wchar_from_utf8(const char *str)
{
	wchar_t *widestr;
	int len;

	if (!str) return NULL;
	len=(int)strlen(str)+1;
	if ((widestr = (wchar_t *)malloc(len*sizeof(wchar_t))) != NULL) {
		if (MultiByteToWideChar(win_utf8_io_codepage, 0, str, len, widestr, len) == 0) {
			if (MultiByteToWideChar(CP_ACP, 0, str, len, widestr, len) == 0) { /* try conversion from Ansi in case the initial UTF-8 conversion had failed */
				free(widestr);
				widestr = NULL;
			}
		}
	}

	return widestr;
}

/* retrieve WCHAR commandline, expand wildcards and convert everything to UTF-8 */
int get_utf8_argv(int *argc, char ***argv)
{
	typedef int (__cdecl *wgetmainargs_t)(int*, wchar_t***, wchar_t***, int, int*);
	wgetmainargs_t wgetmainargs;
	HMODULE handle;
	int wargc;
	wchar_t **wargv;
	wchar_t **wenv;
	char **utf8argv;
	int ret, i;

	if ((handle = LoadLibrary("msvcrt.dll")) == NULL) return 1;
	if ((wgetmainargs = (wgetmainargs_t)GetProcAddress(handle, "__wgetmainargs")) == NULL) return 1;
	i = 0;
	/* if __wgetmainargs expands wildcards then it also erroneously converts \\?\c:\path\to\file.flac to \\file.flac */
	if (wgetmainargs(&wargc, &wargv, &wenv, 1, &i) != 0) return 1;
	if ((utf8argv = (char **)calloc(wargc, sizeof(char*))) == NULL) return 1;
	ret = 0;

	for (i=0; i<wargc; i++) {
		if ((utf8argv[i] = utf8_from_wchar(wargv[i])) == NULL) {
			ret = 1;
			break;
		}
	}

	FreeLibrary(handle);

	if (ret == 0) {
		win_utf8_io_codepage = CP_UTF8;
		*argc = wargc;
		*argv = utf8argv;
	} else {
		for (i=0; i<wargc; i++)
			free(utf8argv[i]);
		free(utf8argv);
	}

	return ret;
}

/* return number of characters in the UTF-8 string */
size_t strlen_utf8(const char *str)
{
	size_t len;
	if ((len = MultiByteToWideChar(win_utf8_io_codepage, 0, str, -1, NULL, 0)) == 0)
		len = strlen(str);
	return len;
}

/* get the console width in characters */
int win_get_console_width(void)
{
	int width = 80;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
	if (GetConsoleScreenBufferInfo(hOut, &csbi) != 0) width = csbi.dwSize.X;
	return width;
}

/* print functions */

int print_console(FILE *stream, const wchar_t *text, size_t len)
{
	static HANDLE hOut;
	static HANDLE hErr;
	DWORD out;
	hOut = GetStdHandle(STD_OUTPUT_HANDLE);
	hErr = GetStdHandle(STD_ERROR_HANDLE);
	if (stream == stdout && hOut != INVALID_HANDLE_VALUE && GetFileType(hOut) == FILE_TYPE_CHAR) {
		if (WriteConsoleW(hOut, text, len, &out, NULL) == 0) return -1;
		return out;
	} else if (stream == stderr && hErr != INVALID_HANDLE_VALUE && GetFileType(hErr) == FILE_TYPE_CHAR) {
		if (WriteConsoleW(hErr, text, len, &out, NULL) == 0) return -1;
		return out;
	} else {
		int ret = fputws(text, stream);
		if (ret < 0) return ret;
		return len;
	}
}

int printf_utf8(const char *format, ...)
{
	char *utmp = NULL;
	wchar_t *wout = NULL;
	int ret = -1;

	while (1) {
		va_list argptr;
		if (!(utmp = (char *)malloc(UTF8_BUFFER_SIZE*sizeof(char)))) break;
		va_start(argptr, format);
		ret = local_vsnprintf(utmp, UTF8_BUFFER_SIZE, format, argptr);
		va_end(argptr);
		if (ret < 0) break;
		if (!(wout = wchar_from_utf8(utmp))) {
			ret = -1;
			break;
		}
		ret = print_console(stdout, wout, wcslen(wout));
		break;
	}
	if (utmp) free(utmp);
	if (wout) free(wout);

	return ret;
}

int fprintf_utf8(FILE *stream, const char *format, ...)
{
	char *utmp = NULL;
	wchar_t *wout = NULL;
	int ret = -1;

	while (1) {
		va_list argptr;
		if (!(utmp = (char *)malloc(UTF8_BUFFER_SIZE*sizeof(char)))) break;
		va_start(argptr, format);
		ret = local_vsnprintf(utmp, UTF8_BUFFER_SIZE, format, argptr);
		va_end(argptr);
		if (ret < 0) break;
		if (!(wout = wchar_from_utf8(utmp))) {
			ret = -1;
			break;
		}
		ret = print_console(stream, wout, wcslen(wout));
		break;
	}
	if (utmp) free(utmp);
	if (wout) free(wout);

	return ret;
}

int vfprintf_utf8(FILE *stream, const char *format, va_list argptr)
{
	char *utmp = NULL;
	wchar_t *wout = NULL;
	int ret = -1;

	while (1) {
		if (!(utmp = (char *)malloc(UTF8_BUFFER_SIZE*sizeof(char)))) break;
		if ((ret = local_vsnprintf(utmp, UTF8_BUFFER_SIZE, format, argptr)) < 0) break;
		if (!(wout = wchar_from_utf8(utmp))) {
			ret = -1;
			break;
		}
		ret = print_console(stream, wout, wcslen(wout));
		break;
	}
	if (utmp) free(utmp);
	if (wout) free(wout);

	return ret;
}

/* file functions */

FILE *fopen_utf8(const char *filename, const char *mode)
{
	wchar_t *wname = NULL;
	wchar_t *wmode = NULL;
	FILE *f = NULL;

	while (1) {
		if (!(wname = wchar_from_utf8(filename))) break;
		if (!(wmode = wchar_from_utf8(mode))) break;
		f = _wfopen(wname, wmode);
		break;
	}
	if (wname) free(wname);
	if (wmode) free(wmode);

	return f;
}

int _stat64_utf8(const char *path, struct __stat64 *buffer)
{
	wchar_t *wpath;
	int ret;

	if (!(wpath = wchar_from_utf8(path))) return -1;
	ret = _wstat64(wpath, buffer);
	free(wpath);

	return ret;
}

int chmod_utf8(const char *filename, int pmode)
{
	wchar_t *wname;
	int ret;

	if (!(wname = wchar_from_utf8(filename))) return -1;
	ret = _wchmod(wname, pmode);
	free(wname);

	return ret;
}

int utime_utf8(const char *filename, struct utimbuf *times)
{
	wchar_t *wname;
	struct __utimbuf64 ut;
	int ret;

	if (sizeof(*times) == sizeof(ut)) {
		memcpy(&ut, times, sizeof(ut));
	} else {
		ut.actime = times->actime;
		ut.modtime = times->modtime;
	}

	if (!(wname = wchar_from_utf8(filename))) return -1;
	ret = _wutime64(wname, &ut);
	free(wname);

	return ret;
}

int unlink_utf8(const char *filename)
{
	wchar_t *wname;
	int ret;

	if (!(wname = wchar_from_utf8(filename))) return -1;
	ret = _wunlink(wname);
	free(wname);

	return ret;
}

int rename_utf8(const char *oldname, const char *newname)
{
	wchar_t *wold = NULL;
	wchar_t *wnew = NULL;
	int ret = -1;

	while (1) {
		if (!(wold = wchar_from_utf8(oldname))) break;
		if (!(wnew = wchar_from_utf8(newname))) break;
		ret = _wrename(wold, wnew);
		break;
	}
	if (wold) free(wold);
	if (wnew) free(wnew);

	return ret;
}

HANDLE WINAPI CreateFile_utf8(const char *lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile)
{
	wchar_t *wname;
	HANDLE handle = INVALID_HANDLE_VALUE;

	if ((wname = wchar_from_utf8(lpFileName)) != NULL) {
		handle = CreateFileW(wname, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
		free(wname);
	}

	return handle;
}
