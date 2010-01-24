/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_log.c - Kids love log
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2004, University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */


#ifdef HAVE_CONFIG_H
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef WITH_THREADS
#include <pthread.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif

/* for gettimeofday */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#include <redland.h>


static const char * const log_level_names[LIBRDF_LOG_LAST+1]={
  "none", "debug", "info", "warning", "error", "fatal"
};


/**
 * librdf_log_simple:
 * @world: redland world object or NULL
 * @code: error code
 * @level: #librdf_log_level log level
 * @facility: #librdf_log_facility log facility
 * @locator: raptor_locator if available or NULL
 * @message: message to record
 *
 * INTERNAL - Log a message.
 *
 * If world is NULL, the error ocurred in redland startup before
 * the world was created.
 **/
void
librdf_log_simple(librdf_world* world, int code, 
                  librdf_log_level level, librdf_log_facility facility,
                  void *locator, const char *message)
{
  if(level > LIBRDF_LOG_LAST)
    level=LIBRDF_LOG_NONE;
  
  if(facility > LIBRDF_FROM_LAST)
    facility=LIBRDF_FROM_NONE;
  
  if(world) {
    if(world->log_handler) {
      world->log.code=code;
      world->log.level=level;
      world->log.facility=facility;
      world->log.message=message;
      world->log.locator=(raptor_locator*)locator;

      if(world->log_handler(world->log_user_data, &world->log))
        return;

    } else {
      va_list null_valist;
      memset(&null_valist, '\0', sizeof(va_list));

      switch(level) {
        case LIBRDF_LOG_ERROR:
          if(world->error_handler) {
            if(world->error_handler(world->error_user_data, message, null_valist))
               return;
          }
          break;

        case LIBRDF_LOG_WARN:
          if(world->warning_handler) {
            if(world->warning_handler(world->warning_user_data, message, null_valist))
              return;
          }
          break;

        case LIBRDF_LOG_NONE:
        case LIBRDF_LOG_DEBUG:
        case LIBRDF_LOG_INFO:
        case LIBRDF_LOG_FATAL:

        default:
          break;
      }
    }
  }

  fputs("librdf ", stderr);
  fputs(log_level_names[level], stderr);
  if(locator) {
    int locator_len=raptor_format_locator(NULL, 0, (raptor_locator*)locator);
    if(locator_len>0) {
      char *buffer=(char*)LIBRDF_MALLOC(cstring, locator_len+2);
      *buffer=' ';
      raptor_format_locator(buffer+1, locator_len, (raptor_locator*)locator);
      fputs(buffer, stderr);
      LIBRDF_FREE(cstring, buffer);
    }
  }
  
  fputs(" - ", stderr);
  fputs((message ? message : "(no message)"), stderr);
  fputc('\n', stderr);
}


/**
 * librdf_log:
 * @world: redland world object or NULL
 * @code: error code
 * @level: #librdf_log_level log level
 * @facility: #librdf_log_facility log facility
 * @locator: raptor_locator if available or NULL
 * @message: message to record
 *
 * INTERNAL - Log a message.
 *
 * If world is NULL, the error ocurred in redland startup before
 * the world was created.
 **/
void
librdf_log(librdf_world* world, int code, 
           librdf_log_level level, librdf_log_facility facility,
           void *locator, const char *message, ...)
{
  va_list arguments;
  char *buffer;
  
  va_start(arguments, message);

  buffer=raptor_vsnprintf(message, arguments);
  librdf_log_simple(world, code, level, facility, locator, buffer);
  if(buffer)
    raptor_free_memory(buffer);

  va_end(arguments);
}


/**
 * librdf_fatal:
 * @world: redland world object or NULL
 * @message: message arguments
 *
 * INTERNAL - Fatal error.
 *
 * If world is NULL, the error ocurred in redland startup before
 * the world was created.
 **/
void
librdf_fatal(librdf_world* world, int facility,
             const char *file, int line, const char *function,
             const char *message)
{
  char empty_buffer[1];
  char *buffer;

  /* Not passing NULL to snprintf since that seems to not be portable  */
  size_t length=snprintf(empty_buffer, 1, "%s:%d:%s: fatal error: %s", 
                         file, line, function, message);
  
  length++; /* add the length 1 passed in */
  buffer=(char*)LIBRDF_MALLOC(cstring, length+1); /* for \0 */
  if(!buffer)
    abort();
  
  snprintf(buffer, length, "%s:%d:%s: fatal error: %s", 
           file, line, function, message);
  librdf_log(world, 0, LIBRDF_LOG_FATAL, (librdf_log_facility)facility, NULL,
             "%s", buffer);
  LIBRDF_FREE(cstring, buffer);
  abort();
}


/**
 * librdf_log_message_code:
 * @message: log message
 *
 * Retrieve error code from log message.
 *
 * Return value: int error code
 **/
int
librdf_log_message_code(librdf_log_message *message)
{
  return message->code;
}


/**
 * librdf_log_message_level:
 * @message: log message
 *
 * Retrieve severity of log message.
 *
 * The log message severity level is defined in rdf_log.h as values
 * of enum #librdf_log_level
 *
 * Return value: severity level
 **/
librdf_log_level
librdf_log_message_level(librdf_log_message *message)
{
  return message->level;
}


/**
 * librdf_log_message_facility:
 * @message: log message
 *
 * Retrieve facility that generated the message.
 *
 * The log message facility is defined in rdf_log.h as values
 * of enum #librdf_log_facility
 *
 * Return value: ID of Redland facility that generated the log message.
 **/
librdf_log_facility
librdf_log_message_facility(librdf_log_message *message)
{
  return message->facility;
}


/**
 * librdf_log_message_message:
 * @message: log message
 *
 * Retrieve text message from log entry.
 *
 * The string returned is shared and must be copied by the caller
 * if required to be retained.
 *
 * Return value: shared pointer to the log message string
 **/
const char *
librdf_log_message_message(librdf_log_message *message)
{
  return message->message;
}


/**
 * librdf_log_message_locator:
 * @message: log message
 *
 * Retrieve locator of log entry.
 *
 * Return value: pointer to an appropriate raptor_locator* or NULL if not available
 **/
raptor_locator*
librdf_log_message_locator(librdf_log_message *message)
{
  return message->locator;
}


/* prototypes for testing errors only - NOT PART OF API */
void
librdf_test_error(librdf_world* world, const char *message) 
{
  librdf_log_simple(world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_NONE, NULL, message);
}

void
librdf_test_warning(librdf_world* world, const char *message)
{
  librdf_log_simple(world, 0, LIBRDF_LOG_WARN, LIBRDF_FROM_NONE, NULL, message);
}

