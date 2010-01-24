/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_iostream.c - Raptor I/O-stream class for abstracting I/O
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004, University of Bristol, UK http://www.bristol.ac.uk/
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
 */


#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


#ifndef STANDALONE

#define RAPTOR_IOSTREAM_MODE_READ  1
#define RAPTOR_IOSTREAM_MODE_WRITE 2

#define RAPTOR_IOSTREAM_FLAGS_EOF           1
#define RAPTOR_IOSTREAM_FLAGS_FREE_HANDLER  2

struct raptor_iostream_s
{
  void *user_data;
  const raptor_iostream_handler2* handler;
  unsigned long offset;
  unsigned int mode;
  int flags;
};



/* prototypes for local functions */


static int
raptor_iostream_calculate_modes(const raptor_iostream_handler2 * const handler2)
{
  int mode = 0;

  /* API V1 checks */
  if((handler2->version >= 1) && 
     handler2->read_bytes)
    mode |= RAPTOR_IOSTREAM_MODE_READ;

  /* API V2 checks */
  if((handler2->version >= 2) &&
     (handler2->write_byte || handler2->write_bytes))
    mode |= RAPTOR_IOSTREAM_MODE_WRITE;

  return mode;
}


/* Return non-0 if handler is legal and OK for given mode (if not 0=ANY) */
static int
raptor_iostream_check_handler(const raptor_iostream_handler2 * const handler2,
                              unsigned int user_mode)
{
  int mode;

  if(handler2->version < 1 || handler2->version > 2)
    return 0;

  mode = raptor_iostream_calculate_modes(handler2);
  if(user_mode && !(user_mode & mode))
    return 0;
  
  return (mode != 0);
}


/**
 * raptor_new_iostream_from_handler2:
 * @user_data: pointer to context information to pass in to calls
 * @handler2: pointer to handler methods
 *
 * Create a new iostream over a user-defined handler
 *
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_from_handler2(void *user_data,
                                  const raptor_iostream_handler2 * const handler2)
{
  raptor_iostream* iostr;

  if(!raptor_iostream_check_handler(handler2, 0))
    return NULL;

  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1,
                                        sizeof(raptor_iostream));
  if(!iostr)
    return NULL;

  iostr->handler=handler2;
  iostr->user_data=(void*)user_data;
  iostr->mode = raptor_iostream_calculate_modes(handler2);
  
  if(iostr->handler->init && 
     iostr->handler->init(iostr->user_data)) {
    RAPTOR_FREE(raptor_iostream, iostr);
    return NULL;
  }
  return iostr;
}



#ifndef RAPTOR_DISABLE_DEPRECATED
/**
 * raptor_new_iostream_from_handler:
 * @context: pointer to context information to pass in to calls
 * @handler: pointer to handler methods
 *
 * Create a new iostream over a user-defined handler.
 *
 * @deprecated: Use raptor_new_iostream_from_handler2() instead
 *
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_from_handler(void *user_data,
                                 const raptor_iostream_handler *handler)
{
  raptor_iostream* iostr=NULL;
  raptor_iostream_handler2 *handler2;

  if(!handler)
    return NULL;

  handler2=(raptor_iostream_handler2*)RAPTOR_CALLOC(raptor_iostream_handler2, 1,
                                                   sizeof(raptor_iostream_handler2*));
  if(!handler2)
    return NULL;
  
  /* Copy V1 functions to V2 structure */
  handler2->init        = handler->init;
  handler2->finish      = handler->finish;
  handler2->write_byte  = handler->write_byte;
  handler2->write_bytes = handler->write_bytes;
  handler2->write_end   = handler->write_end;

  iostr=raptor_new_iostream_from_handler2(user_data, handler2);
  if(iostr) {
    /* Ensure newly alloced structure is freed on iostream destruction */
    iostr->flags |= RAPTOR_IOSTREAM_FLAGS_FREE_HANDLER;
  } else
    /* failure: so delete it now */
    RAPTOR_FREE(raptor_iostream_handler2, handler2);

  return iostr;
}
#endif


/* Local handlers for reading/writing to/from a sink */

static int
raptor_sink_iostream_write_byte(void *user_data, const int byte)
{
  return 0;
}

static int
raptor_sink_iostream_write_bytes(void *user_data, const void *ptr,
                                 size_t size, size_t nmemb)
{
  return 0;
}

static int
raptor_sink_iostream_read_bytes(void *user_data, void *ptr,
                                size_t size, size_t nmemb)
{
  return 0;
}

static int
raptor_sink_iostream_read_eof(void *user_data)
{
  return 1; /* EOF always */
}

static const raptor_iostream_handler2 raptor_iostream_sink_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ NULL,
  /* .write_byte  = */ raptor_sink_iostream_write_byte,
  /* .write_bytes = */ raptor_sink_iostream_write_bytes,
  /* .write_end   = */ NULL,
  /* .read_bytes  = */ raptor_sink_iostream_read_bytes,
  /* .read_eof    = */ raptor_sink_iostream_read_eof
};


/**
 * raptor_new_iostream_to_sink:
 *
 * Create a new write iostream to a sink.
 * 
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_to_sink(void)
{
  return raptor_new_iostream_from_handler2(NULL, &raptor_iostream_sink_handler);
}


/* Local handlers for reading/writing from a filename */

static void
raptor_filename_iostream_finish(void *user_data)
{
  FILE* handle=(FILE*)user_data;
  fclose(handle);
}

static int
raptor_filename_iostream_write_byte(void *user_data, const int byte)
{
  FILE* handle=(FILE*)user_data;
  return (fputc(byte, handle) == byte);
}

static int
raptor_filename_iostream_write_bytes(void *user_data,
                                     const void *ptr, size_t size, size_t nmemb)
{
  FILE* handle=(FILE*)user_data;
  return (fwrite(ptr, size, nmemb, handle) == nmemb);
}

static void
raptor_filename_iostream_write_end(void *user_data)
{
  FILE* handle=(FILE*)user_data;
  fclose(handle);
}

static int
raptor_filename_iostream_read_bytes(void *user_data,
                                    void *ptr, size_t size, size_t nmemb)
{
  FILE* handle=(FILE*)user_data;
  return fread(ptr, size, nmemb, handle);
}

static int
raptor_filename_iostream_read_eof(void *user_data)
{
  FILE* handle=(FILE*)user_data;
  return feof(handle);
}

static const raptor_iostream_handler2 raptor_iostream_write_filename_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ raptor_filename_iostream_finish,
  /* .write_byte  = */ raptor_filename_iostream_write_byte,
  /* .write_bytes = */ raptor_filename_iostream_write_bytes,
  /* .write_end   = */ raptor_filename_iostream_write_end,
  /* .read_bytes  = */ NULL,
  /* .read_eof    = */ NULL
};


/**
 * raptor_new_iostream_to_filename:
 * @filename: Output filename to open and write to
 *
 * Constructor - create a new iostream writing to a filename.
 * 
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_to_filename(const char *filename)
{
  FILE *handle;
  raptor_iostream* iostr;
  const raptor_iostream_handler2* handler2=&raptor_iostream_write_filename_handler;
  const unsigned int mode=RAPTOR_IOSTREAM_MODE_WRITE;
  
  if(!raptor_iostream_check_handler(handler2, mode))
    return NULL;

  handle=fopen(filename, "wb");
  if(!handle)
    return NULL;
  
  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1, sizeof(raptor_iostream));
  if(!iostr) {
    fclose(handle);
    return NULL;
  }

  iostr->handler=handler2;
  iostr->user_data=(void*)handle;
  iostr->mode=mode;

  if(iostr->handler->init && 
     iostr->handler->init(iostr->user_data)) {
    raptor_free_iostream(iostr);
    return NULL;
  }
  return iostr;
}


static const raptor_iostream_handler2 raptor_iostream_write_file_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ NULL,
  /* .write_byte  = */ raptor_filename_iostream_write_byte,
  /* .write_bytes = */ raptor_filename_iostream_write_bytes,
  /* .write_end   = */ NULL,
  /* .read_bytes  = */ NULL,
  /* .read_eof    = */ NULL
};


/**
 * raptor_new_iostream_to_file_handle:
 * @handle: FILE* handle to write to
 *
 * Constructor - create a new iostream writing to a FILE*.
 * 
 * The @handle must already be open for writing.
 * NOTE: This does not fclose the @handle when it is finished.
 *
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_to_file_handle(FILE *handle)
{
  raptor_iostream* iostr;
  const raptor_iostream_handler2* handler2=&raptor_iostream_write_file_handler;
  const unsigned int mode=RAPTOR_IOSTREAM_MODE_WRITE;

  if(!handle)
    return NULL;

  if(!raptor_iostream_check_handler(handler2, mode))
    return NULL;

  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1,
                                        sizeof(raptor_iostream));
  if(!iostr)
    return NULL;

  iostr->handler=handler2;
  iostr->user_data=(void*)handle;
  iostr->mode=mode;

  if(iostr->handler->init && iostr->handler->init(iostr->user_data)) {
    RAPTOR_FREE(raptor_iostream, iostr);
    return NULL;
  }
  return iostr;
}



struct raptor_write_string_iostream_context {
  raptor_stringbuffer *sb;
  void *(*malloc_handler)(size_t size);
  void **string_p;
  size_t *length_p;
};


/* Local handlers for writing to a string */

static void
raptor_write_string_iostream_finish(void *user_data) 
{
  struct raptor_write_string_iostream_context* con;
  size_t len;
  void *str=NULL;

  con=(struct raptor_write_string_iostream_context*)user_data;
  len=raptor_stringbuffer_length(con->sb);

  *con->string_p=NULL;
  if(con->length_p)
    *con->length_p=len;
  
  str=(void*)con->malloc_handler(len+1);
  if(str) {
    if(len)
      raptor_stringbuffer_copy_to_string(con->sb, (unsigned char*)str, len+1);
    else
      *(char*)str='\0';
    *con->string_p=str;
  }

  if(!str && con->length_p)
    *con->length_p=0;
  
  raptor_free_stringbuffer(con->sb);
  RAPTOR_FREE(raptor_write_string_iostream_context, con);
  return;
}

static int
raptor_write_string_iostream_write_byte(void *user_data, const int byte)
{
  struct raptor_write_string_iostream_context* con;
  unsigned char buf=(unsigned char)byte;

  con=(struct raptor_write_string_iostream_context*)user_data;
  return raptor_stringbuffer_append_counted_string(con->sb, &buf, 1, 1);
}


static int
raptor_write_string_iostream_write_bytes(void *user_data, const void *ptr, 
                                         size_t size, size_t nmemb)
{
  struct raptor_write_string_iostream_context* con;

  con=(struct raptor_write_string_iostream_context*)user_data;
  return raptor_stringbuffer_append_counted_string(con->sb, 
                 (const unsigned char*)ptr, size * nmemb, 1);
}

static const raptor_iostream_handler2 raptor_iostream_write_string_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ raptor_write_string_iostream_finish,
  /* .write_byte  = */ raptor_write_string_iostream_write_byte,
  /* .write_bytes = */ raptor_write_string_iostream_write_bytes,
  /* .write_end   = */ NULL,
  /* .read_bytes  = */ NULL,
  /* .read_eof    = */ NULL
};


/**
 * raptor_new_iostream_to_string:
 * @string_p: pointer to location to hold string
 * @length_p: pointer to location to hold length of string (or NULL)
 * @malloc_handler: pointer to malloc to use to make string (or NULL)
 *
 * Constructor - create a new iostream writing to a string.
 *
 * If @malloc_handler is null, raptor will allocate it using it's
 * own memory allocator.  *@string_p is set to NULL on failure (and
 * *@length_p to 0 if @length_p is not NULL).
 * 
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_to_string(void **string_p, size_t *length_p,
                              void *(*malloc_handler)(size_t size))
{
  raptor_iostream* iostr;
  struct raptor_write_string_iostream_context* con;
  const raptor_iostream_handler2* handler2=&raptor_iostream_write_string_handler;
  const unsigned int mode=RAPTOR_IOSTREAM_MODE_WRITE;
  
  if(!raptor_iostream_check_handler(handler2, mode))
    return NULL;

  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1,
                                        sizeof(raptor_iostream));
  if(!iostr)
    return NULL;

  con=(struct raptor_write_string_iostream_context*)RAPTOR_CALLOC(raptor_write_string_iostream_context, 1, sizeof(struct raptor_write_string_iostream_context));
  if(!con) {
    RAPTOR_FREE(raptor_iostream, iostr);
    return NULL;
  }

  con->sb=raptor_new_stringbuffer();
  if(!con->sb) {
    RAPTOR_FREE(raptor_iostream, iostr);
    RAPTOR_FREE(raptor_write_string_iostream_context, con);
    return NULL;
  }

  con->string_p=string_p;
  *string_p=NULL;

  con->length_p=length_p;  
  if(length_p)
    *length_p=0;

  if(malloc_handler)
    con->malloc_handler=malloc_handler;
  else
    con->malloc_handler=raptor_alloc_memory;
  
  iostr->handler=handler2;
  iostr->user_data=(void*)con;
  iostr->mode = mode;

  if(iostr->handler->init && iostr->handler->init(iostr->user_data)) {
    raptor_free_iostream(iostr);
    return NULL;
  }
  return iostr;
}


/**
 * raptor_new_iostream_from_sink:
 *
 * Create a new read iostream from a sink.
 * 
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_from_sink(void)
{
  return raptor_new_iostream_from_handler2(NULL, &raptor_iostream_sink_handler);
}


static const raptor_iostream_handler2 raptor_iostream_read_filename_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ raptor_filename_iostream_finish,
  /* .write_byte  = */ NULL,
  /* .write_bytes = */ NULL,
  /* .write_end   = */ NULL,
  /* .read_bytes  = */ raptor_filename_iostream_read_bytes,
  /* .read_eof    = */ raptor_filename_iostream_read_eof
};


/**
 * raptor_new_iostream_from_filename:
 * @filename: Input filename to open and read from
 *
 * Constructor - create a new iostream reading from a filename.
 * 
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_from_filename(const char *filename)
{
  FILE *handle;
  raptor_iostream* iostr;
  const raptor_iostream_handler2* handler2=&raptor_iostream_read_filename_handler;
  const unsigned int mode=RAPTOR_IOSTREAM_MODE_READ;

  if(!filename)
    return NULL;
  
  if(!raptor_iostream_check_handler(handler2, mode))
    return NULL;

  handle=fopen(filename, "rb");
  if(!handle)
    return NULL;
  
  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1,
                                        sizeof(raptor_iostream));
  if(!iostr) {
    fclose(handle);
    return NULL;
  }

  iostr->handler=handler2;
  iostr->user_data=(void*)handle;
  iostr->mode = mode;

  if(iostr->handler->init && 
     iostr->handler->init(iostr->user_data)) {
    raptor_free_iostream(iostr);
    return NULL;
  }
  return iostr;
}


static const raptor_iostream_handler2 raptor_iostream_read_file_handle_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ NULL,
  /* .write_byte  = */ NULL,
  /* .write_bytes = */ NULL,
  /* .write_end   = */ NULL,
  /* .read_bytes  = */ raptor_filename_iostream_read_bytes,
  /* .read_eof    = */ raptor_filename_iostream_read_eof
};


/**
 * raptor_new_iostream_from_file_handle:
 * @handle: Input file_handle to open and read from
 *
 * Constructor - create a new iostream reading from a file_handle.
 * 
 * The @handle must already be open for reading.
 * NOTE: This does not fclose the @handle when it is finished.
 *
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_from_file_handle(FILE *handle)
{
  raptor_iostream* iostr;
  const raptor_iostream_handler2* handler2=&raptor_iostream_read_file_handle_handler;
  const unsigned int mode=RAPTOR_IOSTREAM_MODE_READ;

  if(!handle)
    return NULL;
  
  if(!raptor_iostream_check_handler(handler2, mode))
    return NULL;

  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1,
                                        sizeof(raptor_iostream));
  if(!iostr)
    return NULL;

  iostr->handler=handler2;
  iostr->user_data=(void*)handle;
  iostr->mode = mode;

  if(iostr->handler->init && 
     iostr->handler->init(iostr->user_data)) {
    RAPTOR_FREE(raptor_iostream, iostr);
    return NULL;
  }
  return iostr;
}


/**
 * raptor_free_iostream:
 * @iostr: iostream object
 *
 * Destructor - destroy an iostream.
 **/
void
raptor_free_iostream(raptor_iostream *iostr)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(iostr, raptor_iostream);
  
  if(iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF)
    raptor_iostream_write_end(iostr);

  if(iostr->handler->finish)
    iostr->handler->finish(iostr->user_data);

  if((iostr->flags & RAPTOR_IOSTREAM_FLAGS_FREE_HANDLER))
    RAPTOR_FREE(raptor_iostream_handler2, iostr->handler);

  RAPTOR_FREE(raptor_iostream, iostr);
}



/**
 * raptor_iostream_write_byte:
 * @iostr: raptor iostream
 * @byte: byte to write
 *
 * Write a byte to the iostream.
 *
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_byte(raptor_iostream *iostr, 
                           const int byte)
{
  iostr->offset++;

  if(iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF)
    return 1;
  if(!iostr->handler->write_byte)
    return 1;
  if(!(iostr->mode & RAPTOR_IOSTREAM_MODE_WRITE))
    return 1;
  return iostr->handler->write_byte(iostr->user_data, byte);
}


/**
 * raptor_iostream_write_bytes:
 * @iostr: raptor iostream
 * @ptr: start of objects to write
 * @size: size of object
 * @nmemb: number of objects
 *
 * Write bytes to the iostream.
 *
 * Return value: number of objects written or less than nmemb or 0 on failure
 **/
int
raptor_iostream_write_bytes(raptor_iostream *iostr,
                            const void *ptr, size_t size, size_t nmemb)
{
  iostr->offset += (size*nmemb);
  
  if(iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF)
    return 1;
  if(!iostr->handler->write_bytes)
    return 0;
  if(!(iostr->mode & RAPTOR_IOSTREAM_MODE_WRITE))
    return 1;
  return iostr->handler->write_bytes(iostr->user_data, ptr, size, nmemb);
}


/**
 * raptor_iostream_write_string:
 * @iostr: raptor iostream
 * @string: string
 *
 * Write a NULL-terminated string to the iostream.
 *
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_string(raptor_iostream *iostr, const void *string)
{
  size_t len=strlen((const char*)string);
  return (raptor_iostream_write_bytes(iostr, string, 1, len) != (int)len);
}


/**
 * raptor_iostream_write_counted_string:
 * @iostr: raptor iostream
 * @string: string
 * @len: string length
 *
 * Write a counted string to the iostream.
 *
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_counted_string(raptor_iostream *iostr, 
                                     const void *string, size_t len) 
{
  return (raptor_iostream_write_bytes(iostr, string, 1, len) != (int)len);
}


/**
 * raptor_iostream_write_uri:
 * @iostr: raptor iostream
 * @uri: URI
 *
 * Write a raptor URI to the iostream.
 *
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_uri(raptor_iostream* iostr,raptor_uri* uri)
{
  size_t len;
  const void *string=raptor_uri_as_counted_string(uri, &len);
  return (raptor_iostream_write_bytes(iostr, string, 1, len) != (int)len);
}


/**
 * raptor_iostream_write_end:
 * @iostr: raptor iostream
 *
 * End writing to the iostream.
 **/
void
raptor_iostream_write_end(raptor_iostream *iostr)
{
  if(iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF)
    return;
  if(iostr->handler->write_end)
    iostr->handler->write_end(iostr->user_data);
  iostr->flags |= RAPTOR_IOSTREAM_FLAGS_EOF;
}


#ifndef RAPTOR_DISABLE_DEPRECATED
/**
 * raptor_iostream_get_bytes_written_count:
 * @iostr: raptor iostream
 *
 * Get the number of bytes written to the iostream.
 *
 * @deprecated: Use raptor_iostream_tell() instead
 *
 * Return value: number of bytes written or 0 if none written so far
 **/
size_t
raptor_iostream_get_bytes_written_count(raptor_iostream *iostr)
{
  return iostr->offset;
}
#endif


/**
 * raptor_iostream_write_stringbuffer:
 * @iostr: raptor iostream
 * @sb: #raptor_stringbuffer to write
 *
 * Write a stringbuffer to an iostream.
 * 
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_stringbuffer(raptor_iostream* iostr,
                                   raptor_stringbuffer *sb)
{
  int length;
  if(!sb)
    return 1;
  
  length=(int)raptor_stringbuffer_length(sb);
  if(length) {
    int count=raptor_iostream_write_bytes(iostr,
                                          raptor_stringbuffer_as_string(sb),
                                          1, length);
    return (count != length);
  } else
    return 0;
}


/**
 * raptor_iostream_write_decimal:
 * @iostr: raptor iostream
 * @integer: integer to format as decimal
 *
 * Write an integer in decimal to the iostream.
 * 
 * Return value: non-0 on failure
 **/
int
raptor_iostream_write_decimal(raptor_iostream* iostr, int integer)
{
  /* enough for 64 bit signed integer
   * INT64_MAX is  9223372036854775807 (19 digits) + 1 for sign 
   */
  unsigned char buf[20];
  unsigned char *p;
  int i=integer;
  size_t length=1;
  if(integer < 0) {
    length++;
    i= -integer;
  }
  while(i/=10)
    length++;

  p=buf+length-1;
  i=integer;
  if(i < 0)
    i= -i;
  do {
    *p-- ='0'+(i %10);
    i /= 10;
  } while(i);
  if(integer < 0)
    *p= '-';
  
  return raptor_iostream_write_bytes(iostr, buf, 1, length);
}


/**
 * raptor_iostream_format_hexadecimal:
 * @iostr: raptor iostream
 * @integer: unsigned integer to format as hexadecimal
 * @width: field width
 *
 * Write an integer in hexadecimal to the iostream.
 *
 * Always 0-fills the entire field and writes in uppercase A-F
 * 
 * Return value: non-0 on failure
 **/
int
raptor_iostream_format_hexadecimal(raptor_iostream* iostr, 
                                   unsigned int integer, int width)
{
  unsigned char *buf;
  unsigned char *p;
  int rc;

  if(width <1)
    return 1;
  
  buf=(unsigned char*)RAPTOR_MALLOC(cstring, width);
  if(!buf)
    return 1;
  
  p=buf+width-1;
  do {
    unsigned int digit=(integer & 15);
    *p-- =(digit < 10) ? '0'+digit : 'A'+(digit-10);
    integer >>= 4;
  } while(integer);
  while(p >= buf)
    *p-- = '0';
  
  rc=raptor_iostream_write_bytes(iostr, buf, 1, width);
  RAPTOR_FREE(cstring, buf);
  return rc;
}



/**
 * raptor_iostream_read_bytes:
 * @iostr: raptor iostream
 * @ptr: start of buffer to read objects into
 * @size: size of object
 * @nmemb: number of objects to read
 *
 * Read bytes to the iostream.
 *
 * Return value: number of objects read, 0 or less than nmemb on EOF, <0 on failure
 **/
int
raptor_iostream_read_bytes(raptor_iostream *iostr,
                           void *ptr, size_t size, size_t nmemb)
{
  int count;
  
  if(!(iostr->mode & RAPTOR_IOSTREAM_MODE_READ))
    return -1;

  if(iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF)
    return 0;

  if(!iostr->handler->read_bytes)
    count= -1;
  else
    count=iostr->handler->read_bytes(iostr->user_data, ptr, size, nmemb);

  if(count > 0)
    iostr->offset += (size*count);

  if(count < (int)nmemb)
    iostr->flags |= RAPTOR_IOSTREAM_FLAGS_EOF;
  
  return count;
}


/**
 * raptor_iostream_read_eof:
 * @iostr: raptor read iostream
 *
 * Check if an read iostream has ended
 *
 * Return value: non-0 if EOF (or not a read iostream)
 **/
int
raptor_iostream_read_eof(raptor_iostream *iostr)
{
  /* Streams without read are always EOF */
  if(!(iostr->mode & RAPTOR_IOSTREAM_MODE_READ))
    return 1;
  
  if(!(iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF) &&
     iostr->handler->read_eof &&
     iostr->handler->read_eof(iostr->user_data))
    iostr->flags |= RAPTOR_IOSTREAM_FLAGS_EOF;

  return ((iostr->flags & RAPTOR_IOSTREAM_FLAGS_EOF) != 0);
}


struct raptor_read_string_iostream_context {
  /* input buffer */
  void* string;
  size_t length;
  /* pointer into buffer */
  size_t offset;
};


/* Local handlers for reading from a string */

static void
raptor_read_string_iostream_finish(void *user_data) 
{
  struct raptor_read_string_iostream_context* con;

  con=(struct raptor_read_string_iostream_context*)user_data;
  RAPTOR_FREE(raptor_read_string_iostream_context, con);
  return;
}

static int
raptor_read_string_iostream_read_bytes(void *user_data, void *ptr, 
                                       size_t size, size_t nmemb)
{
  struct raptor_read_string_iostream_context* con;
  size_t avail;
  size_t blen;

  if(!ptr || size <= 0 || !nmemb)
    return -1;
  
  con=(struct raptor_read_string_iostream_context*)user_data;
  if(con->offset >= con->length)
    return 0;

  avail=(int)((con->length-con->offset) / size);  
  if(avail < nmemb)
    avail=nmemb;
  blen=(avail * size);
  memcpy(ptr, (char*)con->string + con->offset, blen);
  con->offset+= blen;
  
  return avail;
}

static int
raptor_read_string_iostream_read_eof(void *user_data)
{
  struct raptor_read_string_iostream_context* con;

  con=(struct raptor_read_string_iostream_context*)user_data;
  return (con->offset >= con->length);
}


static const raptor_iostream_handler2 raptor_iostream_read_string_handler={
  /* .version     = */ 2,
  /* .init        = */ NULL,
  /* .finish      = */ raptor_read_string_iostream_finish,
  /* .write_byte  = */ NULL,
  /* .write_bytes = */ NULL,
  /* .write_end   = */ NULL,
  /* .read_bytes  = */ raptor_read_string_iostream_read_bytes,
  /* .read_eof    = */ raptor_read_string_iostream_read_eof
};


/**
 * raptor_new_iostream_from_string:
 * @string: pointer to string
 * @length: length of string
 *
 * Constructor - create a new iostream reading from a string.
 *
 * Return value: new #raptor_iostream object or NULL on failure
 **/
raptor_iostream*
raptor_new_iostream_from_string(void *string, size_t length)
{
  raptor_iostream* iostr;
  struct raptor_read_string_iostream_context* con;
  const raptor_iostream_handler2* handler2=&raptor_iostream_read_string_handler;
  const unsigned int mode=RAPTOR_IOSTREAM_MODE_READ;

  if(!string)
    return NULL;
  
  if(!raptor_iostream_check_handler(handler2, mode))
    return NULL;

  iostr=(raptor_iostream*)RAPTOR_CALLOC(raptor_iostream, 1,
                                        sizeof(raptor_iostream));
  if(!iostr)
    return NULL;

  con=(struct raptor_read_string_iostream_context*)RAPTOR_CALLOC(raptor_read_string_iostream_context, 1, sizeof(struct raptor_read_string_iostream_context));
  if(!con) {
    RAPTOR_FREE(raptor_iostream, iostr);
    return NULL;
  }

  con->string=string;
  con->length=length;

  iostr->handler=handler2;
  iostr->user_data=(void*)con;
  iostr->mode = mode;

  if(iostr->handler->init && iostr->handler->init(iostr->user_data)) {
    raptor_free_iostream(iostr);
    return NULL;
  }
  return iostr;
}


/**
 * raptor_iostream_tell:
 * @iostr: raptor iostream
 *
 * Get the offset in the iostream.
 *
 * Return value: offset in iostream
 **/
unsigned long
raptor_iostream_tell(raptor_iostream *iostr)
{
  return iostr->offset;
}



#endif



#ifdef STANDALONE

/* one more prototype */
int main(int argc, char *argv[]);


static const char *program;

#define READ_BUFFER_SIZE 256


static int
test_write_to_filename(const char* filename, 
                       const char* test_string, size_t test_string_len,
                       const unsigned int expected_bytes_count)
{
  raptor_iostream *iostr=NULL;
  unsigned long count;
  int rc=0;
  const char* const label="write iostream to filename";
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s '%s'\n", program, label, filename);
#endif

  iostr=raptor_new_iostream_to_filename(filename);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s '%s'\n", program, label, filename);
    rc=1;
    goto tidy;
  }

  raptor_iostream_write_bytes(iostr, test_string, 1, test_string_len);
  raptor_iostream_write_byte(iostr, '\n');
  
  count=raptor_iostream_tell(iostr);
  if(count != expected_bytes_count) {
    fprintf(stderr, "%s: %s wrote %d bytes, expected %d\n", program, label,
            (int)count, expected_bytes_count);
    rc=1;
    goto tidy;
  }

  tidy:
  if(iostr)
    raptor_free_iostream(iostr);
  remove(filename);

  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_write_to_file_handle(FILE* handle,
                          const char* test_string, size_t test_string_len,
                          const unsigned int expected_bytes_count)
{
  raptor_iostream *iostr=NULL;
  unsigned long count;
  int rc=0;
  const char* const label="write iostream to file handle";
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s\n", program, label);
#endif

  iostr=raptor_new_iostream_to_file_handle(handle);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s\n", program, label);
    rc=1;
    goto tidy;
  }

  raptor_iostream_write_bytes(iostr, test_string, 1, test_string_len);
  raptor_iostream_write_byte(iostr, '\n');
  
  count=raptor_iostream_tell(iostr);
  if(count != expected_bytes_count) {
    fprintf(stderr, "%s: %s wrote %d bytes, expected %d\n", program, label,
            (int)count, expected_bytes_count);
    rc=1;
  }

  tidy:
  if(iostr)
    raptor_free_iostream(iostr);
  
  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_write_to_string(const char* test_string, size_t test_string_len,
                     const unsigned int expected_bytes_count)
{
  raptor_iostream *iostr=NULL;
  unsigned long count;
  int rc=0;
  void *string=NULL;
  size_t string_len;
  const char* const label="write iostream to a string";

  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s\n", program, label);
#endif

  iostr=raptor_new_iostream_to_string(&string, &string_len, NULL);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create write iostream to string\n",
            program);
    rc=1;
    goto tidy;
  }

  raptor_iostream_write_bytes(iostr, test_string, 1, test_string_len);
  raptor_iostream_write_byte(iostr, '\n');
  
  count=raptor_iostream_tell(iostr);
  if(count != expected_bytes_count) {
    fprintf(stderr, "%s: %s wrote %d bytes, expected %d\n", program, label,
            (int)count, expected_bytes_count);
    rc=1;
  }

  raptor_free_iostream(iostr); iostr=NULL;

  if(!string) {
    fprintf(stderr, "%s: %s failed to create a string\n", program, label);
    return 1;
  }
  if(string_len != count) {
    fprintf(stderr, "%s: %s created a string length %d, expected %d\n",
            program, label, (int)string_len, (int)count);
    return 1;
  }

  tidy:
  if(string)
    raptor_free_memory(string);
  if(iostr)
    raptor_free_iostream(iostr);
  
  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_write_to_sink(const char* test_string, size_t test_string_len,
                   const unsigned int expected_bytes_count)
{
  raptor_iostream *iostr=NULL;
  unsigned long count;
  int rc=0;
  const char* const label="write iostream to sink";

#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s\n", program, label);
#endif

  iostr=raptor_new_iostream_to_sink();
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s\n", program, label);
    rc=1;
    goto tidy;
  }

  raptor_iostream_write_bytes(iostr, test_string, 1, test_string_len);
  raptor_iostream_write_byte(iostr, '\n');
  
  count=raptor_iostream_tell(iostr);
  if(count != expected_bytes_count) {
    fprintf(stderr, "%s: %s wrote %d bytes, expected %d\n", program, label,
            (int)count, expected_bytes_count);
    rc=1;
  }

  tidy:
  if(iostr)
    raptor_free_iostream(iostr);
  
  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_read_from_filename(const char* filename, 
                        const char* test_string, size_t test_string_len,
                        const unsigned int expected_len,
                        const unsigned int expected_len2)
{
  raptor_iostream *iostr=NULL;
  char buffer[READ_BUFFER_SIZE];
  unsigned long count;
  int rc=0;
  const char* const label="read iostream from filename";

#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s '%s'\n", program, label, filename);
#endif

  iostr=raptor_new_iostream_from_filename(filename);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s '%s'\n", program, label, filename);
    rc=1;
    goto tidy;
  }
  
  count=raptor_iostream_read_bytes(iostr, buffer, 1, test_string_len);
  if(count != expected_len) {
    fprintf(stderr, "%s: %s read %d bytes, expected %d\n", program, label,
            (int)count, (int)expected_len);
    rc=1;
    goto tidy;
  }

  count=raptor_iostream_read_bytes(iostr, buffer, 1, test_string_len);
  if(count != expected_len2) {
    fprintf(stderr, "%s: %s read %d bytes, expected %d\n", program, label,
            (int)count, (int)expected_len2);
    rc=1;
    goto tidy;
  }

  if(!raptor_iostream_read_eof(iostr)) {
    fprintf(stderr, "%s: %s not EOF as expected\n", program, label);
    rc=1;
    goto tidy;
  }

  if(strncmp(buffer, test_string, test_string_len)) {
    fprintf(stderr, "%s: %s returned '%s' expected '%s'\n", program, label,
            buffer, test_string);
    rc=1;
  }

  tidy:
  if(iostr)
    raptor_free_iostream(iostr);

  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_read_from_file_handle(FILE* handle,
                           const char* test_string, size_t test_string_len,
                           const unsigned int expected_len,
                           const unsigned int expected_len2)
{
  raptor_iostream *iostr=NULL;
  char buffer[READ_BUFFER_SIZE];
  unsigned long count;
  int rc=0;
  const char* const label="read iostream from file handle";
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s\n", program, label);
#endif

  iostr=raptor_new_iostream_from_file_handle(handle);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s\n", program, label);
    rc=1;
    goto tidy;
  }

  count=raptor_iostream_read_bytes(iostr, buffer, 1, test_string_len);
  if(count != expected_len) {
    fprintf(stderr, "%s: %s read %d bytes, expected %d\n", program, label,
            (int)count, (int)expected_len);
    rc=1;
  }

  count=raptor_iostream_read_bytes(iostr, buffer, 1, test_string_len);
  if(count != expected_len2) {
    fprintf(stderr, "%s: %s read %d bytes, expected %d\n", program, label,
            (int)count, (int)expected_len2);
    rc=1;
    goto tidy;
  }

  if(!raptor_iostream_read_eof(iostr)) {
    fprintf(stderr, "%s: %s not EOF as expected\n", program, label);
    rc=1;
  }

  if(strncmp(buffer, test_string, test_string_len)) {
    fprintf(stderr, "%s: %s returned '%s' expected '%s'\n", program, label,
            buffer, test_string);
    rc=1;
  }
  
  tidy:
  if(iostr)
    raptor_free_iostream(iostr);

  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_read_from_string(const char* test_string, size_t test_string_len,
                      const unsigned int expected_len)
{
  raptor_iostream *iostr=NULL;
  char buffer[READ_BUFFER_SIZE];
  unsigned long count;
  int rc=0;
  const char* const label="read iostream from a string";
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s\n", program, label);
#endif
  
  iostr=raptor_new_iostream_from_string((void*)test_string, test_string_len);
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s\n", program, label);
    rc=1;
    goto tidy;
  }

  count=raptor_iostream_read_bytes(iostr, buffer, 1, test_string_len);
  if(count != expected_len) {
    fprintf(stderr, "%s: %s read %d bytes, expected %d\n", program, label,
            (int)count, (int)expected_len);
    rc=1;
  }

  if(!raptor_iostream_read_eof(iostr)) {
    fprintf(stderr, "%s: %s not EOF as expected\n", program, label);
    rc=1;
  }

  if(strncmp(buffer, test_string, test_string_len)) {
    fprintf(stderr, "%s: %s returned '%s' expected '%s'\n", program, label,
            buffer, test_string);
    rc=1;
  }
  
  tidy:
  if(iostr)
    raptor_free_iostream(iostr);

  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


static int
test_read_from_sink(size_t read_len, size_t expected_len)
{
  raptor_iostream *iostr=NULL;
  char buffer[READ_BUFFER_SIZE];
  unsigned long count;
  int rc=0;
  const char* const label="read iostream from sink";
  
#ifdef RAPTOR_DEBUG
  fprintf(stderr, "%s: Testing %s\n", program, label);
#endif
  expected_len=0;
  iostr=raptor_new_iostream_from_sink();
  if(!iostr) {
    fprintf(stderr, "%s: Failed to create %s\n", program, label);
    rc=1;
    goto tidy;
  }

  count=raptor_iostream_read_bytes(iostr, buffer, 1, read_len);
  if(count != expected_len) {
    fprintf(stderr, "%s: %s read %d bytes, expected %d\n", program, label,
            (int)count, (int)expected_len);
    rc=1;
  }

  if(!raptor_iostream_read_eof(iostr)) {
    fprintf(stderr, "%s: %s not EOF as expected\n", program, label);
    rc=1;
  }

  tidy:
  if(iostr)
    raptor_free_iostream(iostr);

  if(rc)
    fprintf(stderr, "%s: FAILED Testing %s\n", program, label);
    
  return rc;
}


#define OUT_FILENAME "out.bin"
#define OUT_BYTES_COUNT 14
#define TEST_STRING "Hello, world!"
#define TEST_STRING_LEN 13
#define IN_FILENAME "in.bin"


int
main(int argc, char *argv[]) 
{
  FILE *handle=NULL;
  int failures=0;
  
  program=raptor_basename(argv[0]);

  /* Write tests */
  failures+= test_write_to_filename((const char*)OUT_FILENAME,
                         TEST_STRING, TEST_STRING_LEN, (int)OUT_BYTES_COUNT);
  handle=fopen((const char*)OUT_FILENAME, "wb");
  if(!handle) {
    fprintf(stderr, "%s: Failed to create write file handle to file %s\n",
            program, OUT_FILENAME);
    failures++;
  } else {
    failures+= test_write_to_file_handle(handle, TEST_STRING, TEST_STRING_LEN,
                                         (int)OUT_BYTES_COUNT);
    fclose(handle);
    remove(OUT_FILENAME);
  }
  
  failures+= test_write_to_string(TEST_STRING,
                                  TEST_STRING_LEN, (int)OUT_BYTES_COUNT);
  failures+= test_write_to_sink(TEST_STRING,
                                TEST_STRING_LEN, (int)OUT_BYTES_COUNT);

  remove(OUT_FILENAME);


  /* Read tests */
  handle=fopen((const char*)IN_FILENAME, "wb");
  if(!handle) {
    fprintf(stderr, "%s: Failed to create write handle to file %s\n",
            program, IN_FILENAME);
    failures++;
  } else {
    fwrite(TEST_STRING, 1, TEST_STRING_LEN, handle);
    fclose(handle);

    failures+= test_read_from_filename((const char*)IN_FILENAME,
                                       TEST_STRING, TEST_STRING_LEN,
                                       TEST_STRING_LEN, 0);
    handle=fopen((const char*)IN_FILENAME, "rb");
    if(!handle) {
      fprintf(stderr, "%s: Failed to create read file handle to file %s\n",
              program, IN_FILENAME);
      failures++;
    } else {
      failures+= test_read_from_file_handle(handle,
                                            TEST_STRING, TEST_STRING_LEN,
                                            TEST_STRING_LEN, 0);
      fclose(handle); handle=NULL;
    }
  }
  
  failures+= test_read_from_string(TEST_STRING, TEST_STRING_LEN,
                                   TEST_STRING_LEN);
  failures+= test_read_from_sink(TEST_STRING_LEN, 0);

  remove(IN_FILENAME);
  
  return failures;
}

#endif
