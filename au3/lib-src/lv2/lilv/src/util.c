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

#define _POSIX_C_SOURCE 200809L  /* for fileno */
#define _BSD_SOURCE     1        /* for realpath, symlink */
#define _DEFAULT_SOURCE 1        /* for realpath, symlink */

#ifdef __APPLE__
#    define _DARWIN_C_SOURCE 1  /* for flock */
#endif

#include "lilv_config.h"
#include "lilv_internal.h"

#include "lilv/lilv.h"
#include "serd/serd.h"

#ifdef _WIN32
#ifndef _WIN32_WINNT
#    define _WIN32_WINNT 0x0600  /* for CreateSymbolicLink */
#endif
#    include <windows.h>
#    include <direct.h>
#    include <io.h>
#    define F_OK 0
#    define mkdir(path, flags) _mkdir(path)
#    if (defined(_MSC_VER) && _MSC_VER <= 1400) || defined(__MINGW64__) || defined(__MINGW32__)
/** Implement 'CreateSymbolicLink()' for MSVC 8 or earlier */
#ifdef __cplusplus
extern "C"
#endif
static BOOLEAN WINAPI
CreateSymbolicLink(LPCTSTR linkpath, LPCTSTR targetpath, DWORD flags)
{
	typedef BOOLEAN (WINAPI* PFUNC)(LPCTSTR, LPCTSTR, DWORD);

	PFUNC pfn = (PFUNC)GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
	                                  "CreateSymbolicLinkA");
	return pfn ? pfn(linkpath, targetpath, flags) : 0;
}
#    endif
#else
#    include <dirent.h>
#    include <unistd.h>
#endif

#if defined(HAVE_FLOCK) && defined(HAVE_FILENO)
#    include <sys/file.h>
#endif

#include <sys/stat.h>
#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef PAGE_SIZE
#    define PAGE_SIZE 4096
#endif

void
lilv_free(void* ptr)
{
	free(ptr);
}

char*
lilv_strjoin(const char* first, ...)
{
	size_t  len    = strlen(first);
	char*   result = (char*)malloc(len + 1);

	memcpy(result, first, len);

	va_list args;
	va_start(args, first);
	while (1) {
		const char* const s = va_arg(args, const char *);
		if (s == NULL) {
			break;
		}

		const size_t this_len   = strlen(s);
		char*        new_result = (char*)realloc(result, len + this_len + 1);
		if (!new_result) {
			free(result);
			return NULL;
		}

		result = new_result;
		memcpy(result + len, s, this_len);
		len += this_len;
	}
	va_end(args);

	result[len] = '\0';

	return result;
}

char*
lilv_strdup(const char* str)
{
	if (!str) {
		return NULL;
	}

	const size_t len  = strlen(str);
	char*        copy = (char*)malloc(len + 1);
	memcpy(copy, str, len + 1);
	return copy;
}

const char*
lilv_uri_to_path(const char* uri)
{
	return (const char*)serd_uri_to_path((const uint8_t*)uri);
}

char*
lilv_file_uri_parse(const char* uri, char** hostname)
{
	return (char*)serd_file_uri_parse((const uint8_t*)uri, (uint8_t**)hostname);
}

/** Return the current LANG converted to Turtle (i.e. RFC3066) style.
 * For example, if LANG is set to "en_CA.utf-8", this returns "en-ca".
 */
char*
lilv_get_lang(void)
{
	const char* const env_lang = getenv("LANG");
	if (!env_lang || !strcmp(env_lang, "")
	    || !strcmp(env_lang, "C") || !strcmp(env_lang, "POSIX")) {
		return NULL;
	}

	const size_t env_lang_len = strlen(env_lang);
	char* const  lang         = (char*)malloc(env_lang_len + 1);
	for (size_t i = 0; i < env_lang_len + 1; ++i) {
		if (env_lang[i] == '_') {
			lang[i] = '-';  // Convert _ to -
		} else if (env_lang[i] >= 'A' && env_lang[i] <= 'Z') {
			lang[i] = env_lang[i] + ('a' - 'A');  // Convert to lowercase
		} else if (env_lang[i] >= 'a' && env_lang[i] <= 'z') {
			lang[i] = env_lang[i];  // Lowercase letter, copy verbatim
		} else if (env_lang[i] >= '0' && env_lang[i] <= '9') {
			lang[i] = env_lang[i];  // Digit, copy verbatim
		} else if (env_lang[i] == '\0' || env_lang[i] == '.') {
			// End, or start of suffix (e.g. en_CA.utf-8), finished
			lang[i] = '\0';
			break;
		} else {
			LILV_ERRORF("Illegal LANG `%s' ignored\n", env_lang);
			free(lang);
			return NULL;
		}
	}

	return lang;
}

#ifndef _WIN32

/** Append suffix to dst, update dst_len, and return the realloc'd result. */
static char*
strappend(char* dst, size_t* dst_len, const char* suffix, size_t suffix_len)
{
	dst = (char*)realloc(dst, *dst_len + suffix_len + 1);
	memcpy(dst + *dst_len, suffix, suffix_len);
	dst[(*dst_len += suffix_len)] = '\0';
	return dst;
}

/** Append the value of the environment variable var to dst. */
static char*
append_var(char* dst, size_t* dst_len, const char* var)
{
	// Get value from environment
	const char* val = getenv(var);
	if (val) {  // Value found, append it
		return strappend(dst, dst_len, val, strlen(val));
	} else {  // No value found, append variable reference as-is
		return strappend(strappend(dst, dst_len, "$", 1),
		                 dst_len, var, strlen(var));
	}
}

#endif

/** Expand variables (e.g. POSIX ~ or $FOO, Windows %FOO%) in `path`. */
char*
lilv_expand(const char* path)
{
#ifdef _WIN32
	char* out = (char*)malloc(MAX_PATH);
	ExpandEnvironmentStrings(path, out, MAX_PATH);
#else
	char*  out = NULL;
	size_t len = 0;

	const char* start = path;  // Start of current chunk to copy
	for (const char* s = path; *s;) {
		if (*s == '$') {
			// Hit $ (variable reference, e.g. $VAR_NAME)
			for (const char* t = s + 1; ; ++t) {
				if (!*t || (!isupper(*t) && !isdigit(*t) && *t != '_')) {
					// Append preceding chunk
					out = strappend(out, &len, start, s - start);

					// Append variable value (or $VAR_NAME if not found)
					char* var = (char*)calloc(t - s, 1);
					memcpy(var, s + 1, t - s - 1);
					out = append_var(out, &len, var);
					free(var);

					// Continue after variable reference
					start = s = t;
					break;
				}
			}
		} else if (*s == '~' && (*(s + 1) == '/' || !*(s + 1))) {
			// Hit ~ before slash or end of string (home directory reference)
			out = strappend(out, &len, start, s - start);
			out = append_var(out, &len, "HOME");
			start = ++s;
		} else {
			++s;
		}
	}

	if (*start) {
		out = strappend(out, &len, start, strlen(start));
	}
#endif

	return out;
}

static bool
lilv_is_dir_sep(const char c)
{
	return c == '/' || c == LILV_DIR_SEP[0];
}

char*
lilv_dirname(const char* path)
{
	const char* s = path + strlen(path) - 1;  // Last character
	for (; s > path && lilv_is_dir_sep(*s); --s) {}  // Last non-slash
	for (; s > path && !lilv_is_dir_sep(*s); --s) {}  // Last internal slash
	for (; s > path && lilv_is_dir_sep(*s); --s) {}  // Skip duplicates

	if (s == path) {  // Hit beginning
		return lilv_is_dir_sep(*s) ? lilv_strdup("/") : lilv_strdup(".");
	} else {  // Pointing to the last character of the result (inclusive)
		char* dirname = (char*)malloc(s - path + 2);
		memcpy(dirname, path, s - path + 1);
		dirname[s - path + 1] = '\0';
		return dirname;
	}
}

bool
lilv_path_exists(const char* path, const void* ignored)
{
#ifdef HAVE_LSTAT
	struct stat st;
	return !lstat(path, &st);
#else
	return !access(path, F_OK);
#endif
}

char*
lilv_find_free_path(const char* in_path,
                    bool (*exists)(const char*, const void*),
                    const void* user_data)
{
	const size_t in_path_len = strlen(in_path);
	char*        path        = (char*)malloc(in_path_len + 7);
	memcpy(path, in_path, in_path_len + 1);

	for (int i = 2; i < 1000000; ++i) {
		if (!exists(path, user_data)) {
			return path;
		}
		snprintf(path, in_path_len + 7, "%s.%u", in_path, i);
	}

	return NULL;
}

int
lilv_copy_file(const char* src, const char* dst)
{
	FILE* in = fopen(src, "r");
	if (!in) {
		return errno;
	}

	FILE* out = fopen(dst, "w");
	if (!out) {
		fclose(in);
		return errno;
	}

	char*  page   = (char*)malloc(PAGE_SIZE);
	size_t n_read = 0;
	int    st     = 0;
	while ((n_read = fread(page, 1, PAGE_SIZE, in)) > 0) {
		if (fwrite(page, 1, n_read, out) != n_read) {
			st = errno;
			break;
		}
	}

	if (!st && (ferror(in) || ferror(out))) {
		st = EBADF;
	}

	free(page);
	fclose(in);
	fclose(out);

	return st;
}

static inline bool
is_windows_path(const char* path)
{
	return (isalpha(path[0]) && (path[1] == ':' || path[1] == '|') &&
	        (path[2] == '/' || path[2] == '\\'));
}

bool
lilv_path_is_absolute(const char* path)
{
	if (lilv_is_dir_sep(path[0])) {
		return true;
	}

#ifdef _WIN32
	if (is_windows_path(path)) {
		return true;
	}
#endif

	return false;
}

char*
lilv_path_absolute(const char* path)
{
	if (lilv_path_is_absolute(path)) {
		return lilv_strdup(path);
	} else {
		char* cwd      = getcwd(NULL, 0);
		char* abs_path = lilv_path_join(cwd, path);
		free(cwd);
		return abs_path;
	}
}

char*
lilv_path_join(const char* a, const char* b)
{
	if (!a) {
		return lilv_strdup(b);
	}

	const size_t a_len   = strlen(a);
	const size_t b_len   = b ? strlen(b) : 0;
	const size_t pre_len = a_len - (lilv_is_dir_sep(a[a_len - 1]) ? 1 : 0);
	char*        path    = (char*)calloc(1, a_len + b_len + 2);
	memcpy(path, a, pre_len);
	path[pre_len] = '/';
	if (b) {
		memcpy(path + pre_len + 1,
		       b + (lilv_is_dir_sep(b[0]) ? 1 : 0),
		       lilv_is_dir_sep(b[0]) ? b_len - 1 : b_len);
	}
	return path;
}

typedef struct {
	char*  pattern;
	time_t time;
	char*  latest;
} Latest;

static void
update_latest(const char* path, const char* name, void* data)
{
	Latest*  latest     = (Latest*)data;
	char*    entry_path = lilv_path_join(path, name);
	unsigned num;
	if (sscanf(entry_path, latest->pattern, &num) == 1) {
		struct stat st;
		if (!stat(entry_path, &st)) {
			if (st.st_mtime >= latest->time) {
				free(latest->latest);
				latest->latest = entry_path;
			}
		} else {
			LILV_ERRORF("stat(%s) (%s)\n", path, strerror(errno));
		}
	}
	if (entry_path != latest->latest) {
		free(entry_path);
	}
}

/** Return the latest copy of the file at `path` that is newer. */
char*
lilv_get_latest_copy(const char* path, const char* copy_path)
{
	char*  copy_dir = lilv_dirname(copy_path);
	Latest latest   = { lilv_strjoin(copy_path, ".%u", NULL), 0, NULL };

	struct stat st;
	if (!stat(path, &st)) {
		latest.time = st.st_mtime;
	} else {
		LILV_ERRORF("stat(%s) (%s)\n", path, strerror(errno));
	}

	lilv_dir_for_each(copy_dir, &latest, update_latest);

	free(latest.pattern);
	free(copy_dir);
	return latest.latest;
}

char*
lilv_realpath(const char* path)
{
	if (!path) {
		return NULL;
	}

#if defined(_WIN32)
	char* out = (char*)malloc(MAX_PATH);
	GetFullPathName(path, MAX_PATH, out, NULL);
	return out;
#else
	char* real_path = realpath(path, NULL);
	return real_path ? real_path : lilv_strdup(path);
#endif
}

int
lilv_symlink(const char* oldpath, const char* newpath)
{
	int ret = 0;
	if (strcmp(oldpath, newpath)) {
#ifdef _WIN32
		ret = !CreateSymbolicLink(newpath, oldpath, 0);
		if (ret) {
			ret = !CreateHardLink(newpath, oldpath, 0);
		}
#else
		ret = symlink(oldpath, newpath);
#endif
	}
	if (ret) {
		LILV_ERRORF("Failed to link %s => %s (%s)\n",
		            newpath, oldpath, strerror(errno));
	}
	return ret;
}

char*
lilv_path_relative_to(const char* path, const char* base)
{
	const size_t path_len = strlen(path);
	const size_t base_len = strlen(base);
	const size_t min_len  = (path_len < base_len) ? path_len : base_len;

	// Find the last separator common to both paths
	size_t last_shared_sep = 0;
	for (size_t i = 0; i < min_len && path[i] == base[i]; ++i) {
		if (lilv_is_dir_sep(path[i])) {
			last_shared_sep = i;
		}
	}

	if (last_shared_sep == 0) {
		// No common components, return path
		return lilv_strdup(path);
	}

	// Find the number of up references ("..") required
	size_t up = 0;
	for (size_t i = last_shared_sep + 1; i < base_len; ++i) {
		if (lilv_is_dir_sep(base[i])) {
			++up;
		}
	}

	// Write up references
	const size_t suffix_len = path_len - last_shared_sep;
	char*        rel        = (char*)calloc(1, suffix_len + (up * 3) + 1);
	for (size_t i = 0; i < up; ++i) {
		memcpy(rel + (i * 3), "../", 3);
	}

	// Write suffix
	memcpy(rel + (up * 3), path + last_shared_sep + 1, suffix_len);
	return rel;
}

bool
lilv_path_is_child(const char* path, const char* dir)
{
	if (path && dir) {
		const size_t path_len = strlen(path);
		const size_t dir_len  = strlen(dir);
		return dir && path_len >= dir_len && !strncmp(path, dir, dir_len);
	}
	return false;
}

int
lilv_flock(FILE* file, bool lock)
{
#if defined(HAVE_FLOCK) && defined(HAVE_FILENO)
	return flock(fileno(file), lock ? LOCK_EX : LOCK_UN);
#else
	return 0;
#endif
}

void
lilv_dir_for_each(const char* path,
                  void*       data,
                  void (*f)(const char* path, const char* name, void* data))
{
#ifdef _WIN32
	char*           pat = lilv_path_join(path, "*");
	WIN32_FIND_DATA fd;
	HANDLE          fh  = FindFirstFile(pat, &fd);
	if (fh != INVALID_HANDLE_VALUE) {
		do {
			f(path, fd.cFileName, data);
		} while (FindNextFile(fh, &fd));
	}
	free(pat);
#else
	DIR* dir = opendir(path);
	if (dir) {
		for (struct dirent* entry; (entry = readdir(dir));) {
			f(path, entry->d_name, data);
		}
		closedir(dir);
	}
#endif
}

int
lilv_mkdir_p(const char* dir_path)
{
	char*        path     = lilv_strdup(dir_path);
	const size_t path_len = strlen(path);
	size_t       i        = 1;

#ifdef _WIN32
	if (is_windows_path(dir_path)) {
		i = 3;
	}
#endif

	for (; i <= path_len; ++i) {
		const char c = path[i];
		if (c == LILV_DIR_SEP[0] || c == '/' || c == '\0') {
			path[i] = '\0';
			if (mkdir(path, 0755) && errno != EEXIST) {
				free(path);
				return errno;
			}
			path[i] = c;
		}
	}

	free(path);
	return 0;
}

static off_t
lilv_file_size(const char* path)
{
	struct stat buf;
	if (stat(path, &buf)) {
		LILV_ERRORF("stat(%s) (%s)\n", path, strerror(errno));
		return 0;
	}
	return buf.st_size;
}

bool
lilv_file_equals(const char* a_path, const char* b_path)
{
	if (!strcmp(a_path, b_path)) {
		return true;  // Paths match
	}

	bool        match  = false;
	FILE*       a_file = NULL;
	FILE*       b_file = NULL;
	char* const a_real = lilv_realpath(a_path);
	char* const b_real = lilv_realpath(b_path);
	if (!strcmp(a_real, b_real)) {
		match = true;  // Real paths match
	} else if (lilv_file_size(a_path) != lilv_file_size(b_path)) {
		match = false;  // Sizes differ
	} else if (!(a_file = fopen(a_real, "rb")) ||
	           !(b_file = fopen(b_real, "rb"))) {
		match = false;  // Missing file matches nothing
	} else {
		// TODO: Improve performance by reading chunks
		match = true;
		while (!feof(a_file) && !feof(b_file)) {
			if (fgetc(a_file) != fgetc(b_file)) {
				match = false;
				break;
			}
		}
	}

	if (a_file) {
		fclose(a_file);
	}
	if (b_file) {
		fclose(b_file);
	}
	free(a_real);
	free(b_real);
	return match;
}
