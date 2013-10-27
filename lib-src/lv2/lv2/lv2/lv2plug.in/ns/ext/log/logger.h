/*
  Copyright 2012 David Robillard <http://drobilla.net>

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
   @file logger.h Convenience API for easy logging in plugin code.

   This file provides simple wrappers for the most common log operations for
   use in plugin implementations.  If host support for logging is not
   available, then these functions will print to stderr instead.

   This header is non-normative, it is provided for convenience.
*/

#ifndef LV2_ATOM_LOGGER_H
#define LV2_ATOM_LOGGER_H

#include <stdio.h>

#include "lv2/lv2plug.in/ns/ext/log/log.h"

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
   Initialise @p logger.

   URIs will be mapped using @p map and stored, a reference to @p map itself is
   not held.  Both @p map and @p log may be NULL when unsupported by the host,
   in which case the implementation will fall back to printing to stderr.
*/
static inline void
lv2_log_logger_init(LV2_Log_Logger* logger,
                    LV2_URID_Map*   map,
                    LV2_Log_Log*    log)
{
	memset(logger, 0, sizeof(LV2_Log_Logger));
	logger->log = log;
	if (map) {
		logger->Error   = map->map(map->handle, LV2_LOG__Error);
		logger->Note    = map->map(map->handle, LV2_LOG__Note);
		logger->Trace   = map->map(map->handle, LV2_LOG__Trace);
		logger->Warning = map->map(map->handle, LV2_LOG__Warning);
	}
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
	if (logger->log) {
		return logger->log->vprintf(logger->log->handle, type, fmt, args);
	} else {
		return vfprintf(stderr, fmt, args);
	}
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

/**
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_LOG_LOGGER_H */
