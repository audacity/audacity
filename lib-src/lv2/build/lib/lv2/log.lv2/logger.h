/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

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

/**
   @defgroup logger Logger
   @ingroup log

   Convenience API for easy logging in plugin code.  This API provides simple
   wrappers for logging from a plugin, which automatically fall back to
   printing to stderr if host support is unavailabe.

   @{
*/

#ifndef LV2_ATOM_LOGGER_H
#define LV2_ATOM_LOGGER_H

#include "lv2/log/log.h"

#include <stdio.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
   Logger convenience API state.
*/
typedef struct {
	LV2_Log_Log* log;

	LV2_URID Error;
	LV2_URID Note;
	LV2_URID Trace;
	LV2_URID Warning;
} LV2_Log_Logger;

/**
   Set `map` as the URI map for `logger`.

   This affects the message type URIDs (Error, Warning, etc) which are passed
   to the log's print functions.
*/
static inline void
lv2_log_logger_set_map(LV2_Log_Logger* logger, LV2_URID_Map* map)
{
	if (map) {
		logger->Error   = map->map(map->handle, LV2_LOG__Error);
		logger->Note    = map->map(map->handle, LV2_LOG__Note);
		logger->Trace   = map->map(map->handle, LV2_LOG__Trace);
		logger->Warning = map->map(map->handle, LV2_LOG__Warning);
	} else {
		logger->Error = logger->Note = logger->Trace = logger->Warning = 0;
	}
}

/**
   Initialise `logger`.

   URIs will be mapped using `map` and stored, a reference to `map` itself is
   not held.  Both `map` and `log` may be NULL when unsupported by the host,
   in which case the implementation will fall back to printing to stderr.
*/
static inline void
lv2_log_logger_init(LV2_Log_Logger* logger,
                    LV2_URID_Map*   map,
                    LV2_Log_Log*    log)
{
	logger->log = log;
	lv2_log_logger_set_map(logger, map);
}

/**
   Log a message to the host, or stderr if support is unavailable.
*/
LV2_LOG_FUNC(3, 0)
static inline int
lv2_log_vprintf(LV2_Log_Logger* logger,
                LV2_URID        type,
                const char*     fmt,
                va_list         args)
{
	return ((logger && logger->log)
	        ? logger->log->vprintf(logger->log->handle, type, fmt, args)
	        : vfprintf(stderr, fmt, args));
}

/** Log an error via lv2_log_vprintf(). */
LV2_LOG_FUNC(2, 3)
static inline int
lv2_log_error(LV2_Log_Logger* logger, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const int ret = lv2_log_vprintf(logger, logger->Error, fmt, args);
	va_end(args);
	return ret;
}

/** Log a note via lv2_log_vprintf(). */
LV2_LOG_FUNC(2, 3)
static inline int
lv2_log_note(LV2_Log_Logger* logger, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const int ret = lv2_log_vprintf(logger, logger->Note, fmt, args);
	va_end(args);
	return ret;
}

/** Log a trace via lv2_log_vprintf(). */
LV2_LOG_FUNC(2, 3)
static inline int
lv2_log_trace(LV2_Log_Logger* logger, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const int ret = lv2_log_vprintf(logger, logger->Trace, fmt, args);
	va_end(args);
	return ret;
}

/** Log a warning via lv2_log_vprintf(). */
LV2_LOG_FUNC(2, 3)
static inline int
lv2_log_warning(LV2_Log_Logger* logger, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const int ret = lv2_log_vprintf(logger, logger->Warning, fmt, args);
	va_end(args);
	return ret;
}

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_LOG_LOGGER_H */

/**
   @}
*/
