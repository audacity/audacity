/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland_dbus.c - Redland D-BUS example code
 *
 * $Id: redland_dbus.c,v 1.1 2008-07-08 10:39:59 larsl Exp $
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2003-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <unistd.h>

/* for the memory allocation functions */
#if defined(HAVE_DMALLOC_H) && defined(RAPTOR_MEMORY_DEBUG_DMALLOC)
#include <dmalloc.h>
#endif

/* Redland includes */
#include <redland.h>

#define DBUS_API_SUBJECT_TO_CHANGE 1

/* glib */
#include <glib.h>
/* with threads*/
#include <glib/gthread.h>

/* D-BUS */
#include <dbus/dbus.h>

/* D-BUS glib */
#include <dbus/dbus-glib.h>

static DBusMessageHandler *disconnect_handler;

static dbus_int32_t handler_slot = -1;

typedef void TestData;


static TestData*
test_message_data_new(void) 
{
  return NULL;
}

static void
test_message_data_free(TestData *data) 
{
  
}


 
static DBusHandlerResult
handle_test_message (DBusMessageHandler *handler,
                     DBusConnection     *connection,
                     DBusMessage        *message,
                     void               *user_data)
{
   dbus_connection_flush (connection);
   return DBUS_HANDLER_RESULT_ALLOW_MORE_HANDLERS;   
}


static DBusHandlerResult
handle_disconnect (DBusMessageHandler *handler,
                   DBusConnection     *connection,
                   DBusMessage        *message,
                   void               *user_data)
{
  g_print ("connection disconnected\n");
  dbus_connection_unref (connection);

  return DBUS_HANDLER_RESULT_ALLOW_MORE_HANDLERS;
}


static void
new_connection_callback (DBusServer     *server,
                         DBusConnection *new_connection,
                         void           *user_data) {
  const char *test_messages[] = { "org.librdf.redland.Test" };
  const char *disconnect_messages[] = { "org.freedesktop.Local.Disconnect" };
  DBusMessageHandler *test_message_handler;
  TestData *data;
  
  g_print ("new_connection_callback\n");
  dbus_connection_ref (new_connection);

  dbus_connection_setup_with_g_main (new_connection, NULL);  

  data = test_message_data_new();
   
   test_message_handler =
     dbus_message_handler_new (handle_test_message,
                               data, (DBusFreeFunction)test_message_data_free);
   
   if (!dbus_connection_register_handler (new_connection,
                                          test_message_handler,
                                          test_messages, 1))
     goto nomem;
 
   if (!dbus_connection_set_data (new_connection,
                                  handler_slot,
                                  test_message_handler,
                                  (DBusFreeFunction)dbus_message_handler_unref))
     goto nomem;
   
   if (!dbus_connection_register_handler (new_connection,
                                          disconnect_handler,
                                          disconnect_messages, 1))
     goto nomem;
   
   return;
   
 nomem:
   g_error ("Out of memory in setting up new connection");
}


int
main(int argc, char *argv[])
{
  librdf_world* world;
  GMainLoop *loop;
  DBusServer *server;
  DBusError error;
  char *program=argv[0];

  if (argc < 2) {
    fprintf (stderr, "%s: USAGE [server address]\n", program);
    return 1;
  }
 
  world=librdf_new_world();
  librdf_world_open(world);

  g_thread_init (NULL);
  dbus_gthread_init ();

  dbus_error_init (&error);  
  server = dbus_server_listen (argv[1], &error);
  if (server == NULL)
    {
      fprintf (stderr, "Failed to start server on %s: %s\n",
               argv[1], error.message);
      dbus_error_free (&error);
      return 1;
    }
  
  if (!dbus_connection_allocate_data_slot (&handler_slot))
    g_error ("no memory for data slot");
  
   disconnect_handler =
     dbus_message_handler_new (handle_disconnect, NULL, NULL);
   if (disconnect_handler == NULL)
     g_error ("no memory for handler");
   
   dbus_server_set_new_connection_function (server,
                                            new_connection_callback,
                                            NULL, NULL);
 
   dbus_server_setup_with_g_main (server, NULL);
   
   loop = g_main_loop_new (NULL, FALSE);
   g_main_run (loop);  

  librdf_free_world(world);

#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif
	
  /* keep gcc -Wall happy */

  return 0;
}

