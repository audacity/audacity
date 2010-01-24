/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_mysql.c - RDF Storage in MySQL DB interface definition.
 *
 * Based in part on rdf_storage_list and rdf_storage_parka.
 *
 * Copyright (C) 2003-2005 Morten Frederiksen - http://purl.org/net/morten/
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <config-win.h>
#include <winsock.h>
#include <assert.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> /* for abort() as used in errors */
#endif
#include <sys/types.h>

#include <redland.h>
#include <rdf_types.h>

#include <mysql.h>
#include <mysqld_error.h>


/* Define to emit SQL: statements to stderr */
/*
#define LIBRDF_DEBUG_SQL 1
*/

typedef enum {
  TABLE_RESOURCES,
  TABLE_BNODES,
  TABLE_LITERALS,
  TABLE_STATEMENTS,
  TABLE_MODELS,
  TABLE_LAST = TABLE_MODELS
} mysql_table_numbers;

typedef enum {
  TRIPLE_URI    =0,
  TRIPLE_BLANK  =1,
  TRIPLE_LITERAL=2,
  TRIPLE_NONE   =3,
} triple_node_type;


typedef struct 
{
  const char *name;
  const char *schema;
  const char *columns; /* Excluding key column, always called ID */
} table_info;


typedef struct
{
  /* how many ints form the primary key for this row. e.g. for statements=4 */
  short key_len;

  u64 uints[4];           /* 4 is for Statements S,P,O,C, rest 1=ID, 2... */
  char *strings[3];       /* 3 is for Literals longtext, text, text */
  size_t strings_len[3];
  int strings_count;
} pending_row;


static const table_info mysql_tables[TABLE_LAST+1]={
  /* VALUES "(" UINT64_T_FMT ",'%s')" */
  { "Resources",
    "ID bigint unsigned NOT NULL, URI text NOT NULL,",
    "URI" },

  /*  VALUES "(" UINT64_T_FMT ",'%s')" */
  { "Bnodes",
    "ID bigint unsigned NOT NULL, Name text NOT NULL,",
    "Name" },

  /* VALUES "(" UINT64_T_FMT ",'%s','%s','%s')" */
  { "Literals",
    "ID bigint unsigned NOT NULL, Value longtext NOT NULL, Language text NOT NULL, Datatype text NOT NULL",
    "Value, Language, Datatype" },

  /* VALUES "(" UINT64_T_FMT "," UINT64_T_FMT "," UINT64_T_FMT "," UINT64_T_FMT ")" */
  { NULL /* Statements%d" */,
    "", 
    "Subject, Predicate, Object, Context"  },

  /* VALUES "(" UINT64_T_FMT ",'%s')" */
  { "Models",
    "ID bigint unsigned NOT NULL, Name text NOT NULL,",
    "Name"  }
     
};


typedef enum {
  /* Status of individual MySQL connections */
  LIBRDF_STORAGE_MYSQL_CONNECTION_CLOSED = 0,
  LIBRDF_STORAGE_MYSQL_CONNECTION_OPEN = 1,
  LIBRDF_STORAGE_MYSQL_CONNECTION_BUSY = 2
} librdf_storage_mysql_connection_status;

typedef struct {
  /* A MySQL connection */
  librdf_storage_mysql_connection_status status;
  MYSQL *handle;
} librdf_storage_mysql_connection;

typedef struct {
  /* MySQL connection parameters */
  const char *host;
  int port;
  const char *database;
  const char *user;
  const char *password;

  /* Array of virtual MySQL connections */
  librdf_storage_mysql_connection *connections;
  int connections_count;

  /* hash of model name in the database (table Models, column ID) */
  u64 model;

  /* if inserts should be optimized by locking and index optimizations */
  int bulk;

  /* if a table with merged models should be maintained */
  int merge;

  /* if mysql MYSQL_OPT_RECONNECT should be set on new connections */
  int reconnect;

  /* digest object for node hashes */
  librdf_digest *digest;

  MYSQL* transaction_handle;
  
  raptor_sequence* pending_inserts[4];
  librdf_hash* pending_insert_hash_nodes;
  raptor_sequence* pending_statements;
  
  /* SQL config */
  librdf_sql_config* config;
  
  /* configuration variables */
  librdf_hash* vars;

  /* SQL schema layout - default is "v1" */
  char *layout;

  /* SQL config directory - default is defined in
   * librdf_new_sql_config_for_storage
   */
  char *config_dir;
} librdf_storage_mysql_context;

/* prototypes for local functions */
static int librdf_storage_mysql_init(librdf_storage* storage, const char *name,
                                     librdf_hash* options);
static int librdf_storage_mysql_merge(librdf_storage* storage);
static void librdf_storage_mysql_terminate(librdf_storage* storage);
static int librdf_storage_mysql_open(librdf_storage* storage,
                                     librdf_model* model);
static int librdf_storage_mysql_close(librdf_storage* storage);
static int librdf_storage_mysql_sync(librdf_storage* storage);
static int librdf_storage_mysql_size(librdf_storage* storage);
static int librdf_storage_mysql_add_statement(librdf_storage* storage,
                                              librdf_statement* statement);
static int librdf_storage_mysql_add_statements(librdf_storage* storage,
                                               librdf_stream* statement_stream);
static int librdf_storage_mysql_remove_statement(librdf_storage* storage,
                                                 librdf_statement* statement);
static int librdf_storage_mysql_contains_statement(librdf_storage* storage,
                                                   librdf_statement* statement);
static librdf_stream*
       librdf_storage_mysql_serialise(librdf_storage* storage);
static librdf_stream*
       librdf_storage_mysql_find_statements(librdf_storage* storage,
                                            librdf_statement* statement);
static librdf_stream*
       librdf_storage_mysql_find_statements_with_options(librdf_storage* storage,
                                                         librdf_statement* statement,
                                                         librdf_node* context_node,
                                                         librdf_hash* options);

/* context functions */
static int librdf_storage_mysql_context_add_statement(librdf_storage* storage,
                                                      librdf_node* context_node,
                                                      librdf_statement* statement);
static int librdf_storage_mysql_context_add_statements(librdf_storage* storage,
                                                       librdf_node* context_node,
                                                       librdf_stream* statement_stream);
static int librdf_storage_mysql_context_remove_statement(librdf_storage* storage,
                                                         librdf_node* context_node,
                                                         librdf_statement* statement);
static int librdf_storage_mysql_context_remove_statements(librdf_storage* storage,
                                                          librdf_node* context_node);
static librdf_stream*
       librdf_storage_mysql_context_serialise(librdf_storage* storage,
                                              librdf_node* context_node);
static librdf_stream* librdf_storage_mysql_find_statements_in_context(librdf_storage* storage,
                                               librdf_statement* statement,
                                               librdf_node* context_node);
static librdf_iterator* librdf_storage_mysql_get_contexts(librdf_storage* storage);

static void librdf_storage_mysql_register_factory(librdf_storage_factory *factory);

/* "private" helper definitions */
typedef struct {
  librdf_storage *storage;
  librdf_statement *current_statement;
  librdf_node *current_context;
  librdf_statement *query_statement;
  librdf_node *query_context;
  MYSQL *handle;
  MYSQL_RES *results;
  int is_literal_match;
} librdf_storage_mysql_sos_context;

typedef struct {
  librdf_storage *storage;
  librdf_node *current_context;
  MYSQL *handle;
  MYSQL_RES *results;
} librdf_storage_mysql_get_contexts_context;

static u64 librdf_storage_mysql_hash(librdf_storage* storage, const char *type,
                                     const char *string, int length);

#define NODE_HASH_MODE_GET_HASH 0
#define NODE_HASH_MODE_STORE_NODE 1
static u64 librdf_storage_mysql_node_hash_common(librdf_storage* storage,
                                                 librdf_node* node,
                                                 int mode);
static u64 librdf_storage_mysql_get_node_hash(librdf_storage* storage, librdf_node* node);
static u64 librdf_storage_mysql_store_node(librdf_storage* storage, librdf_node* node);
static int librdf_storage_mysql_start_bulk(librdf_storage* storage);
static int librdf_storage_mysql_stop_bulk(librdf_storage* storage);
static int librdf_storage_mysql_context_add_statement_helper(librdf_storage* storage,
                                                             u64 ctxt,
                                                             librdf_statement* statement);
static int librdf_storage_mysql_find_statements_in_context_augment_query(char **query, const char *addition);

/* methods for stream of statements */
static int librdf_storage_mysql_find_statements_in_context_end_of_stream(void* context);
static int librdf_storage_mysql_find_statements_in_context_next_statement(void* context);
static void* librdf_storage_mysql_find_statements_in_context_get_statement(void* context, int flags);
static void librdf_storage_mysql_find_statements_in_context_finished(void* context);

/* methods for iterator for contexts */
static int librdf_storage_mysql_get_contexts_end_of_iterator(void* context);
static int librdf_storage_mysql_get_contexts_next_context(void* context);
static void* librdf_storage_mysql_get_contexts_get_context(void* context, int flags);
static void librdf_storage_mysql_get_contexts_finished(void* context);


static int librdf_storage_mysql_transaction_rollback(librdf_storage* storage);



/* functions implementing storage api */

/*
 * librdf_storage_mysql_hash - Find hash value of string.
 * @storage: the storage
 * @type: character type of node to hash ("R", "L" or "B")
 * @string: a string to get hash for
 * @length: length of string
 *
 * Find hash value of string.
 *
 * Return value: Non-zero on succes.
 **/
static u64
librdf_storage_mysql_hash(librdf_storage* storage, const char *type,
                          const char *string, int length)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  u64 hash;
  byte* digest;
  uint i;

  /* (Re)initialize digest object */
  librdf_digest_init(context->digest);
  
  /* Update digest with data */
  if(type)
    librdf_digest_update(context->digest, (unsigned char*)type, 1);
  librdf_digest_update(context->digest, (unsigned char*)string, length);
  librdf_digest_final(context->digest);
  
  /* Copy first 8 bytes of digest into unsigned 64bit hash
   * using a method portable across big/little endianness
   *
   * Fixes Issue#0000023 - http://bugs.librdf.org/mantis/view.php?id=23
   */
  digest = (byte*) librdf_digest_get_digest(context->digest);
  hash = 0;
  for(i=0; i<8; i++)
    hash += ((u64) digest[i]) << (i*8);
  
  return hash;
}


/*
 * librdf_storage_mysql_init_connections - Initialize MySQL connection pool.
 * @storage: the storage
 *
 * Return value: Non-zero on success.
 **/
static int
librdf_storage_mysql_init_connections(librdf_storage* storage)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;

  /* Reset connection pool */
  context->connections=NULL;
  context->connections_count=0;
  return 0;
}


/*
 * librdf_storage_mysql_finish_connections - Finish all connections in MySQL connection pool and free structures.
 * @storage: the storage
 *
 * Return value: None.
 **/
static void
librdf_storage_mysql_finish_connections(librdf_storage* storage)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  int i;

  /* Loop through connections and close */
  for(i=0; i < context->connections_count; i++) {
    if(LIBRDF_STORAGE_MYSQL_CONNECTION_CLOSED != context->connections[i].status)
#ifdef LIBRDF_DEBUG_SQL
      LIBRDF_DEBUG2("mysql_close connection handle %p\n",
                    context->connections[i].handle);
#endif
      mysql_close(context->connections[i].handle);
  }
  /* Free structure and reset */
  if (context->connections_count) {
    LIBRDF_FREE(librdf_storage_mysql_connection*, context->connections);
    context->connections=NULL;
    context->connections_count=0;
  }
}

/*
 * librdf_storage_mysql_get_handle - get a connection handle to the MySQL server
 * @storage: the storage
 *
 * This attempts to reuses any existing available pooled connection
 * otherwise creates a new connection to the server.
 *
 * Return value: Non-zero on succes.
 **/
static MYSQL*
librdf_storage_mysql_get_handle(librdf_storage* storage)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  librdf_storage_mysql_connection* connection= NULL;
  int i;

  if(context->transaction_handle)
    return context->transaction_handle;

  /* Look for an open connection handle to return */
  for(i=0; i < context->connections_count; i++) {
    if(LIBRDF_STORAGE_MYSQL_CONNECTION_OPEN == context->connections[i].status) {
      context->connections[i].status=LIBRDF_STORAGE_MYSQL_CONNECTION_BUSY;
      return context->connections[i].handle;
    }
  }

  /* Look for a closed connection */
  for(i=0; i < context->connections_count && !connection; i++) {
    if(LIBRDF_STORAGE_MYSQL_CONNECTION_CLOSED == context->connections[i].status) {
      connection=&context->connections[i];
      break;
    }
  }
  /* Expand connection pool if no closed connection was found */
  if (!connection) {
    /* Allocate new buffer with two extra slots */
    librdf_storage_mysql_connection* connections;
    if(!(connections=(librdf_storage_mysql_connection*)
        LIBRDF_CALLOC(librdf_storage_mysql_connection,
                      context->connections_count+2,
                      sizeof(librdf_storage_mysql_connection))))
      return NULL;

    if (context->connections_count) {
      /* Copy old buffer to new */
      memcpy(connections, context->connections, sizeof(librdf_storage_mysql_connection)*context->connections_count);
      /* Free old buffer */
      LIBRDF_FREE(librdf_storage_mysql_connection*, context->connections);
    }

    /* Update buffer size and reset new connections */
    context->connections_count+=2;
    connection=&connections[context->connections_count-2];
    connection->status=LIBRDF_STORAGE_MYSQL_CONNECTION_CLOSED;
    connection->handle=NULL;
    connections[context->connections_count-1].status=LIBRDF_STORAGE_MYSQL_CONNECTION_CLOSED;
    connections[context->connections_count-1].handle=NULL;
    context->connections=connections;
  }

  /* Initialize closed MySQL connection handle */
  connection->handle=mysql_init(connection->handle);

#ifdef HAVE_MYSQL_OPT_RECONNECT
  if(1) {
    my_bool value=(context->reconnect) ? 1 : 0;
    mysql_options(connection->handle, MYSQL_OPT_RECONNECT, &value);
  }
#endif

  /* Create connection to database for handle */
  if(!mysql_real_connect(connection->handle,
                         context->host, context->user, context->password,
                         context->database, context->port, NULL, 0)) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "Connection to MySQL database %s:%d name %s as user %s failed: %s",
               context->host, context->port, context->database,
               context->user, mysql_error(connection->handle));
    return NULL;
  }
  /* Update status and return */
  connection->status=LIBRDF_STORAGE_MYSQL_CONNECTION_BUSY;
  return connection->handle;
}


/*
 * librdf_storage_mysql_release_handle - Release a connection handle to MySQL server back to the pool
 * @storage: the storage
 * @handle: the MySQL handle to release
 *
 * Return value: None.
 **/
static void
librdf_storage_mysql_release_handle(librdf_storage* storage, MYSQL *handle)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  int i;

  if(handle == context->transaction_handle)
    return;
  
  /* Look for busy connection handle to drop */
  for(i=0; i < context->connections_count; i++) {
    if(LIBRDF_STORAGE_MYSQL_CONNECTION_BUSY == context->connections[i].status &&
       context->connections[i].handle == handle) {
      context->connections[i].status=LIBRDF_STORAGE_MYSQL_CONNECTION_OPEN;
      return;
    }
  }
  librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
             "Unable to find busy connection (in pool of %i connections) to drop for MySQL server thread: %lu",
             context->connections_count, mysql_thread_id(handle));
}


/**
 * librdf_storage_mysql_init:
 * @storage: the storage
 * @name: model name
 * @options: host, port, database, user, password [, new] [, bulk] [, merge].
 *
 * .
 *
 * Create connection to database.  Defaults to port 3306 if not given.
 *
 * The boolean bulk option can be set to true if optimized inserts (table
 * locks and temporary key disabling) is wanted. Note that this will block
 * all other access, and requires table locking and alter table privileges.
 *
 * The boolean merge option can be set to true if a merged "view" of all
 * models should be maintained. This "view" will be a table with TYPE=MERGE.
 *
 * Return value: Non-zero on failure.
 **/
static int
librdf_storage_mysql_init(librdf_storage* storage, const char *name,
                          librdf_hash* options)
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context*)storage->context;
  const char create_model[]="INSERT INTO Models (ID,Name) VALUES (" UINT64_T_FMT ",'%s')";
  const char check_model[]="SELECT 1 FROM Models WHERE ID=" UINT64_T_FMT " AND Name='%s'";
  int status=0;
  char *escaped_name=NULL;
  char *query=NULL;
  MYSQL_RES *res;
  MYSQL *handle;
  const char* default_layout="v1";

  /* Must have connection parameters passed as options */
  if(!options)
    return 1;

  /* Create digest */
  if(!(context->digest=librdf_new_digest(storage->world,"MD5")))
    return 1;

  /* Save hash of model name */
  context->model=librdf_storage_mysql_hash(storage, NULL, (char*)name, strlen(name));

  /* Save connection parameters */
  context->host=librdf_hash_get_del(options, "host");
  context->port=librdf_hash_get_as_long(options, "port");
  if(context->port < 0)
    context->port=3306; /* default mysql port */
  context->database=librdf_hash_get_del(options, "database");
  context->user=librdf_hash_get_del(options, "user");
  context->password=librdf_hash_get_del(options, "password");

  if(!context->host || !context->database || !context->user || !context->port
     || !context->password)
    return 1;

  /* Maintain merge table? */
  context->merge=(librdf_hash_get_as_boolean(options, "merge")>0);

  /* Reconnect? */
  context->reconnect=(librdf_hash_get_as_boolean(options, "reconnect")>0);

  context->layout=librdf_hash_get_del(options, "layout");
  if(!context->layout) {
    context->layout=(char*)LIBRDF_MALLOC(cstring, strlen(default_layout)+1);
    strcpy(context->layout, default_layout);
  }

  context->config_dir=librdf_hash_get_del(options, "config-dir");

  /* Initialize MySQL connections */
  librdf_storage_mysql_init_connections(storage);

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

  /* Read SQL configuration */
  context->config=librdf_new_sql_config_for_storage(storage, context->layout,
                                                    context->config_dir);
  if(!context->config)
    status= 1;

  if(!status) {
    char vars_str[50];
    context->vars=librdf_new_hash(storage->world, NULL);
      
    sprintf(vars_str, "STATEMENTS_NAME='Statements" UINT64_T_FMT "'", 
            context->model);
    librdf_hash_from_string(context->vars, vars_str);
  }
  

  /* Create tables, if new and not existing */
  if(!status && (librdf_hash_get_as_boolean(options, "new")>0)) {
    int table;
    
    for(table= DBCONFIG_CREATE_TABLE_STATEMENTS;
        table <= DBCONFIG_CREATE_TABLE_MODELS;
        table++) {
      query=(char*)librdf_hash_interpret_template((const unsigned char*)context->config->values[table],
                                                  context->vars, 
                                                  (const unsigned char*)"$(", 
                                                  (const unsigned char*)")");
      
#ifdef LIBRDF_DEBUG_SQL
      LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
      if(mysql_real_query(handle, (const char*)query, 
                          strlen((const char*)query))) {
        librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, 
                   NULL,
                   "MySQL table creation failed: %s",
                   mysql_error(handle));
        status=-1;
        break;
      }

      LIBRDF_FREE(cstring, query);
    } /* end for */
    
  }

  /* Create model if new and not existing, or check for existence */
  if(!status) {
    if(!(escaped_name=(char*)LIBRDF_MALLOC(cstring,strlen(name)*2+1)))
      status=1;
    mysql_real_escape_string(handle, escaped_name,
                             (const char*)name, strlen(name));
  }
  if(!status && (librdf_hash_get_as_boolean(options, "new")>0)) {
    /* Create new model */
    if(!(query=(char*)LIBRDF_MALLOC(cstring,strlen(create_model)+20+
                                    strlen(escaped_name)+1)))
      status=1;
    sprintf(query, create_model, context->model, escaped_name);

#ifdef LIBRDF_DEBUG_SQL
    LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
    if(!status && mysql_real_query(handle, query, strlen(query)) &&
       mysql_errno(handle) != ER_DUP_ENTRY) {
      librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "MySQL insert into Models table failed: %s",
                 mysql_error(handle));
      status=-1;
    }
    /* Maintain merge table? */
    if(!status && context->merge)
      status=librdf_storage_mysql_merge(storage);
  } else if(!status) {
    /* Check for model existence */
    if(!(query=(char*)LIBRDF_MALLOC(cstring,strlen(check_model)+20+
                                    strlen(escaped_name)+1)))
      status=1;
    sprintf(query, check_model, context->model, name);
    res=NULL;

#ifdef LIBRDF_DEBUG_SQL
    LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
    if(!status && (mysql_real_query(handle, query, strlen(query)) ||
                   !(res=mysql_store_result(handle)))) {
      librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "MySQL select from Models table failed: %s",
                 mysql_error(handle));
      status=-1;
    }
    if(!status && !(mysql_fetch_row(res))) {
      librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Unknown model: %s",name);
      status=1;
    }
    if(res)
      mysql_free_result(res);
  }
  if(query)
    LIBRDF_FREE(cstring, query);
  if(escaped_name)
    LIBRDF_FREE(cstring, escaped_name);

  /* Optimize loads? */
  context->bulk=(librdf_hash_get_as_boolean(options, "bulk")>0);

  /* Truncate model? */
  if(!status && (librdf_hash_get_as_boolean(options, "new")>0))
    status=librdf_storage_mysql_context_remove_statements(storage, NULL);

  /* Unused options: write (always...) */
  librdf_free_hash(options);

  librdf_storage_mysql_release_handle(storage, handle);

  return status;
}

/*
 * librdf_storage_mysql_merge - (re)create merged "view" of all models
 * @storage: the storage
 *
 * Return value: Non-zero on failure.
 */
static int
librdf_storage_mysql_merge(librdf_storage* storage)
{
  const char get_models[]="SELECT ID FROM Models";
  const char drop_table_statements[]="DROP TABLE IF EXISTS Statements";
  const char create_table_statements[]="\
  CREATE TABLE Statements (\
  Subject bigint unsigned NOT NULL,\
  Predicate bigint unsigned NOT NULL,\
  Object bigint unsigned NOT NULL,\
  Context bigint unsigned NOT NULL,\
  KEY Context (Context),\
  KEY SubjectPredicate (Subject,Predicate),\
  KEY PredicateObject (Predicate,Object),\
  KEY ObjectSubject (Object,Subject)\
) TYPE=MERGE INSERT_METHOD=NO UNION=(";
  char *query=NULL;
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

  /* Query for list of models. */
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", get_models);
#endif
  if(mysql_real_query(handle, get_models, strlen(get_models)) ||
     !(res=mysql_store_result(handle))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL query for model list failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  /* Allocate space for merge table generation query. */
  if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(create_table_statements)+
                                  mysql_num_rows(res)*31+2))) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 1;
  }
  /* Generate CSV list of models. */
  strcpy(query,create_table_statements);
  while((row=mysql_fetch_row(res))) {
    strcat(query,"Statements");
    strcat(query,row[0]);
    strcat(query,",");
  }
  mysql_free_result(res);
  query[strlen(query)-1]=')';

  /* Drop and create merge table. */
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", drop_table_statements);
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, drop_table_statements,
                      strlen(drop_table_statements)) ||
     mysql_real_query(handle, query, strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL merge table creation failed: %s",
               mysql_error(handle));
    LIBRDF_FREE(cstring, query);
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  LIBRDF_FREE(cstring, query);
  librdf_storage_mysql_release_handle(storage, handle);

  return 0;
}

/**
 * librdf_storage_mysql_terminate:
 * @storage: the storage
 *
 * .
 *
 * Close the storage and database connections.
 *
 * Return value: None.
 **/
static void
librdf_storage_mysql_terminate(librdf_storage* storage)
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context*)storage->context;

  librdf_storage_mysql_finish_connections(storage);

  if(context->config_dir)
    LIBRDF_FREE(cstring,(char *)context->config_dir);

  if(context->layout)
    LIBRDF_FREE(cstring,(char *)context->layout);

  if(context->vars)
    librdf_free_hash(context->vars);

  if(context->config)
    librdf_free_sql_config(context->config);

  if(context->password)
    LIBRDF_FREE(cstring,(char *)context->password);

  if(context->user)
    LIBRDF_FREE(cstring,(char *)context->user);

  if(context->database)
    LIBRDF_FREE(cstring,(char *)context->database);

  if(context->host)
    LIBRDF_FREE(cstring,(char *)context->host);

  if(context->digest)
    librdf_free_digest(context->digest);

  if(context->transaction_handle)
    librdf_storage_mysql_transaction_rollback(storage);
}

/**
 * librdf_storage_mysql_open:
 * @storage: the storage
 * @model: the model
 *
 * .
 *
 * Create or open model in database (nop).
 *
 * Return value: Non-zero on failure.
 **/
static int
librdf_storage_mysql_open(librdf_storage* storage, librdf_model* model)
{
  return 0;
}

/**
 * librdf_storage_mysql_close:
 * @storage: the storage
 *
 * .
 *
 * Close model (nop).
 *
 * Return value: Non-zero on failure.
 **/
static int
librdf_storage_mysql_close(librdf_storage* storage)
{
  librdf_storage_mysql_transaction_rollback(storage);

  return librdf_storage_mysql_sync(storage);
}

/**
 * librdf_storage_mysql_sync:
 * @storage: the storage
 *
 * Flush all tables, making sure they are saved on disk.
 *
 * Return value: Non-zero on failure.
 **/
static int
librdf_storage_mysql_sync(librdf_storage* storage)
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context*)storage->context;

  /* Make sure optimizing for bulk operations is stopped? */
  if(context->bulk)
    librdf_storage_mysql_stop_bulk(storage);

  return 0;
}

/**
 * librdf_storage_mysql_size:
 * @storage: the storage
 *
 * .
 *
 * Close model (nop).
 *
 * Return value: Negative on failure.
 **/
static int
librdf_storage_mysql_size(librdf_storage* storage)
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context*)storage->context;
  char model_size[]="SELECT COUNT(*) FROM Statements" UINT64_T_FMT;
  char *query;
  MYSQL_RES *res;
  MYSQL_ROW row;
  int count;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return -1;

  /* Query for number of statements */
  if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(model_size)+21))) {
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  sprintf(query, model_size, context->model);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, query, strlen(query)) ||
     !(res=mysql_store_result(handle)) ||
     !(row=mysql_fetch_row(res))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL query for model size failed: %s",
               mysql_error(handle));
    LIBRDF_FREE(cstring,query);
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  count=atol(row[0]);
  mysql_free_result(res);
  LIBRDF_FREE(cstring,query);
  librdf_storage_mysql_release_handle(storage, handle);

  return count;
}


static int
librdf_storage_mysql_add_statement(librdf_storage* storage,
                                   librdf_statement* statement)
{
  /* Do not add duplicate statements */
  if(librdf_storage_mysql_contains_statement(storage, statement))
    return 0;

  return librdf_storage_mysql_context_add_statement_helper(storage, 0,
                                                           statement);
}


/**
 * librdf_storage_mysql_add_statements:
 * @storage: the storage
 * @statement_stream: the stream of statements
 *
 * .
 *
 * Add statements in stream to storage, without context.
 *
 * Return value: Non-zero on failure.
 **/
static int
librdf_storage_mysql_add_statements(librdf_storage* storage,
                                    librdf_stream* statement_stream)
{
  int helper=0;

  while(!helper && !librdf_stream_end(statement_stream)) {
    librdf_statement* statement=librdf_stream_get_object(statement_stream);
    /* Do not add duplicate statements */
    if(!librdf_storage_mysql_contains_statement(storage, statement))
      helper=librdf_storage_mysql_context_add_statement_helper(storage, 0,
                                                               statement);
    librdf_stream_next(statement_stream);
  }

  return helper;
}


static int
compare_pending_rows(const void *a, const void *b)
{
  pending_row* prow_a=*(pending_row**)a;
  pending_row* prow_b=*(pending_row**)b;
  int i;
  
  for(i=0; i< prow_a->key_len; i++) {
    /* These are u64 <> u64 compares - DO NOT USE 'int' here */
    if(prow_b->uints[i] > prow_a->uints[i])
      return -1;
    else if(prow_b->uints[i] < prow_a->uints[i])
      return 1;
  }
  return 0;
}



static void
free_pending_row(pending_row* prow)
{
  int i;
  
  for(i=0; i < prow->strings_count; i++)
    LIBRDF_FREE(cstring, prow->strings[i]);

  LIBRDF_FREE(pending_row, prow);
}


static raptor_stringbuffer*
format_pending_row_sequence(const table_info *table, raptor_sequence* seq)
{
  int i;
  raptor_stringbuffer* sb;
  
  if(!raptor_sequence_size(seq))
    return NULL;

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG3("Format pending row for table %s with %d entries\n",
                table->name, raptor_sequence_size(seq));
#endif
  
  sb=raptor_new_stringbuffer();

  raptor_stringbuffer_append_string(sb,
                                    (const unsigned char*)"REPLACE INTO ", 1);
  raptor_stringbuffer_append_string(sb,
                                    (const unsigned char*)table->name, 1);
  raptor_stringbuffer_append_string(sb,
                                    (const unsigned char*)" (ID, ", 1);
  raptor_stringbuffer_append_string(sb,
                                    (const unsigned char*)table->columns, 1);
  raptor_stringbuffer_append_counted_string(sb,
                                    (const unsigned char*)") VALUES ", 9, 1);

  for(i=0; i< raptor_sequence_size(seq); i++) {
    pending_row* prow;
    char uint64_buffer[64];
    int j;
    
    if(i > 0)
      raptor_stringbuffer_append_counted_string(sb,
                                    (const unsigned char*)", ", 2, 1);
    
    prow=(pending_row*)raptor_sequence_get_at(seq, i);
    
    raptor_stringbuffer_append_counted_string(sb,
                                    (const unsigned char*)"(", 1, 1);
    /* First column is always the ID */
    sprintf(uint64_buffer, UINT64_T_FMT, prow->uints[0]);
    raptor_stringbuffer_append_string(sb,
                                    (const unsigned char*)uint64_buffer, 1);

    for(j=0; j < prow->strings_count; j++) {
      raptor_stringbuffer_append_counted_string(sb,
                                    (const unsigned char*)", '", 3, 1);
      raptor_stringbuffer_append_string(sb,
                                    (const unsigned char*)prow->strings[j], 1);
      raptor_stringbuffer_append_counted_string(sb,
                                    (const unsigned char*)"'", 1, 1);
    }

    raptor_stringbuffer_append_counted_string(sb,
                                    (const unsigned char*)")", 1, 1);
  }

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG3("Format pending row for table %s returning query size %d\n",
                table->name, raptor_stringbuffer_length(sb));
#endif
  
  return sb;
}


/*
 * librdf_storage_mysql_node_hash_common - Create/get hash value for node
 * @storage: the storage
 * @node: a node to get hash for (and possibly create in database)
 * @mode: mode to do: 0=just do hash, 1=add them
 *
 * Return value: Non-zero on succes.
 **/
static u64
librdf_storage_mysql_node_hash_common(librdf_storage* storage,
                                      librdf_node* node,
                                      int mode)
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context *)storage->context;
  librdf_node_type type=librdf_node_get_type(node);
  u64 hash;
  size_t nodelen;
  char *query;
  triple_node_type node_type;
  const table_info *table;
  MYSQL *handle;
  unsigned char *uri=NULL;
  unsigned char *value=NULL, *datatype=NULL;
  char *lang=NULL, *nodestring;
  librdf_uri *dt;
  size_t valuelen, langlen=0, datatypelen=0;
  unsigned char *name=NULL;
  /* Escape URI for db query */
  char *escaped_uri;
  /* Escape value, lang and datatype for db query */
  char *escaped_value, *escaped_lang, *escaped_datatype;
  /* Escape name for db query */
  char *escaped_name;
  raptor_sequence *seq=NULL;
  librdf_hash_datum hd_key, hd_value; /* on stack - not allocated */
  librdf_hash_datum* old_value;
  pending_row* prow;
  
  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 0;

  /* Get hash */
  switch(type) {
    case LIBRDF_NODE_TYPE_RESOURCE:
      node_type=TRIPLE_URI;

      uri=librdf_uri_as_counted_string(librdf_node_get_uri(node), &nodelen);
      hash=librdf_storage_mysql_hash(storage, "R", (char*)uri, nodelen);
      break;
      
    case LIBRDF_NODE_TYPE_LITERAL:
      node_type=TRIPLE_LITERAL;
      
      value=librdf_node_get_literal_value_as_counted_string(node, &valuelen);
      lang=librdf_node_get_literal_value_language(node);
      if(lang)
        langlen=strlen(lang);
      dt=librdf_node_get_literal_value_datatype_uri(node);
      if(dt)
        datatype=librdf_uri_as_counted_string(dt, &datatypelen);

      /* Create composite node string for hash generation */
      if(!(nodestring=(char*)LIBRDF_MALLOC(cstring, valuelen+langlen+datatypelen+3))) {
        librdf_storage_mysql_release_handle(storage, handle);
        return 0;
      }
      strcpy(nodestring, (const char*)value);
      strcat(nodestring, "<");
      if(lang)
        strcat(nodestring, lang);
      strcat(nodestring, ">");
      if(datatype)
        strcat(nodestring, (const char*)datatype);
      nodelen=valuelen+langlen+datatypelen+2;
      hash=librdf_storage_mysql_hash(storage, "L", nodestring, nodelen);
      LIBRDF_FREE(cstring, nodestring);
      break;
    
    case LIBRDF_NODE_TYPE_BLANK:
      node_type=TRIPLE_BLANK;
      
      name=librdf_node_get_blank_identifier(node);
      nodelen=strlen((const char*)name);
      hash=librdf_storage_mysql_hash(storage, "B", (char*)name, nodelen);
      break;
      
    case LIBRDF_NODE_TYPE_UNKNOWN:
    default:
      librdf_log(node->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Do not know how to store node type %d", node->type);
      hash=0;
      goto tidy;
  }
  
  if(mode != NODE_HASH_MODE_STORE_NODE)
    goto tidy;

  
  table=&mysql_tables[node_type];

  if(context->transaction_handle) {
    /* In a transaction, check this node has not already been handled */

    /* Store the new */
    hd_key.data=&hash;
    hd_key.size=sizeof(u64);
  
    /* if existing hash found, do not add it */
    if((old_value=librdf_hash_get_one(context->pending_insert_hash_nodes, 
                                      &hd_key))) {
#ifdef LIBRDF_DEBUG_SQL
      LIBRDF_DEBUG2("Already seen node with hash " UINT64_T_FMT " - not inserting\n", hash);
#endif
      librdf_free_hash_datum(old_value);
      goto tidy;
    }
    
    hd_value.data=(void*)"1";
    hd_value.size=2;
    /* store in hash: 'hash'(u64) => "1" */
    if(librdf_hash_put(context->pending_insert_hash_nodes,
                       &hd_key, &hd_value)) {
      hash=0;
      goto tidy;
    }

    /* Store in pending inserts sequence */
    seq=context->pending_inserts[node_type];
  } else {
    /* not a transaction - store in temporary sequence */
    seq=raptor_new_sequence((raptor_sequence_free_handler*)free_pending_row, NULL);
  }


  prow=(pending_row*)LIBRDF_CALLOC(pending_row, sizeof(pending_row), 1);
  prow->key_len=1;
  prow->uints[0]=hash;

  switch(type) {
   case LIBRDF_NODE_TYPE_RESOURCE:
     if(!(escaped_uri=(char*)LIBRDF_MALLOC(cstring, nodelen*2+1))) {
       hash=0;
       goto tidy;
     }
     mysql_real_escape_string(handle, escaped_uri,
                              (const char*)uri, nodelen);
     
     prow->strings[0]=escaped_uri;
     prow->strings_len[0]=strlen(escaped_uri);
     prow->strings_count=1;
     break;
  
    case LIBRDF_NODE_TYPE_LITERAL:
      if(!(escaped_value=(char*)LIBRDF_MALLOC(cstring, valuelen*2+1)) ||
         !(escaped_lang=(char*)LIBRDF_MALLOC(cstring, langlen*2+1)) ||
         !(escaped_datatype=(char*)LIBRDF_MALLOC(cstring, datatypelen*2+1))) {
        hash=0;
        goto tidy;
      }
      mysql_real_escape_string(handle, escaped_value,
                               (const char*)value, valuelen);
      if(lang)
        mysql_real_escape_string(handle, escaped_lang,
                                 (const char*)lang, langlen);
      else
        strcpy(escaped_lang,"");
      if(datatype)
        mysql_real_escape_string(handle, escaped_datatype,
                                 (const char*)datatype, datatypelen);
      else
        strcpy(escaped_datatype,"");

      prow->strings[0]=escaped_value;
      prow->strings_len[0]=strlen(escaped_value);
      prow->strings[1]=escaped_lang;
      prow->strings_len[1]=strlen(escaped_lang);
      prow->strings[2]=escaped_datatype;
      prow->strings_len[2]=strlen(escaped_datatype);
      prow->strings_count=3;
      break;
      
    case LIBRDF_NODE_TYPE_BLANK:
      if(!(escaped_name=(char*)LIBRDF_MALLOC(cstring, nodelen*2+1))) {
        hash=0;
        goto tidy;
      }
      mysql_real_escape_string(handle, escaped_name,
                               (const char*)name, nodelen);
       
      prow->strings[0]=escaped_name;
      prow->strings_len[0]=strlen(escaped_name);
      prow->strings_count=1;
      break;
      
    case LIBRDF_NODE_TYPE_UNKNOWN:
    default:
      librdf_log(node->world,
                 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "Do not know how to store node type %d", node->type);
      hash=0;
      goto tidy;

  }

  raptor_sequence_push(seq, prow);


  if(context->transaction_handle) {
    /* in a transaction */
  } else {
    /* not in a transaction so run it now */
    raptor_stringbuffer *sb=NULL;
    size_t query_len;

    sb=format_pending_row_sequence(table, seq);
    
    query_len=raptor_stringbuffer_length(sb);
    query=(char*)raptor_stringbuffer_as_string(sb);
    if(query) {
#ifdef LIBRDF_DEBUG_SQL
      LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
      if(mysql_real_query(handle, query, query_len) &&
         mysql_errno(handle) != ER_DUP_ENTRY) {
        librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE,
                   NULL, "MySQL insert into %s failed with error %s",
                   table->name, mysql_error(handle));
        raptor_free_stringbuffer(sb);
        hash=0;
        goto tidy;
      }
    }

    raptor_free_stringbuffer(sb);
  }
  
  tidy:
  if(!context->transaction_handle) {
    /* if not in a transaction, lose this */
    if(seq)
      raptor_free_sequence(seq);
  }

  if(handle) {
    librdf_storage_mysql_release_handle(storage, handle);
  }
  
  return hash;
}


static u64
librdf_storage_mysql_get_node_hash(librdf_storage* storage,
                                   librdf_node* node) 
{
  return librdf_storage_mysql_node_hash_common(storage, node,
                                               NODE_HASH_MODE_GET_HASH);
}

static u64
librdf_storage_mysql_store_node(librdf_storage* storage,
                                librdf_node* node) 
{
  return librdf_storage_mysql_node_hash_common(storage, node,
                                               NODE_HASH_MODE_STORE_NODE);
}


/*
 * librdf_storage_mysql_start_bulk - Prepare for bulk insert operation
 * @storage: the storage
 *
 * Return value: Non-zero on failure.
 */
static int
librdf_storage_mysql_start_bulk(librdf_storage* storage)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  char disable_statement_keys[]="ALTER TABLE Statements" UINT64_T_FMT " DISABLE KEYS";
  char disable_literal_keys[]="ALTER TABLE Literals DISABLE KEYS";
  char lock_tables[]="LOCK TABLES Statements" UINT64_T_FMT " WRITE, Resources WRITE, Bnodes WRITE, Literals WRITE";
  char lock_tables_extra[]=", Statements WRITE";
  char *query=NULL;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

  if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(disable_statement_keys)+21))) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 1;
  }
  sprintf(query, disable_statement_keys, context->model);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, query, strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL statement key disabling failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  LIBRDF_FREE(cstring,query);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, disable_literal_keys,
                      strlen(disable_literal_keys))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL literal key disabling failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }

  if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(lock_tables)+
                                  strlen(lock_tables_extra)+21))) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 1;
  }
  sprintf(query, lock_tables, context->model);
  if(context->merge)
    strcat(query, lock_tables_extra);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, query, strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL table locking failed: %s",
               mysql_error(handle));
    LIBRDF_FREE(cstring,query);
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  LIBRDF_FREE(cstring,query);

  librdf_storage_mysql_release_handle(storage, handle);

  return 0;
}


/*
 * librdf_storage_mysql_stop_bulk - End bulk insert operation
 * @storage: the storage
 *
 * Return value: Non-zero on failure.
 */
static int
librdf_storage_mysql_stop_bulk(librdf_storage* storage)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  char enable_statement_keys[]="ALTER TABLE Statements" UINT64_T_FMT " ENABLE KEYS";
  char enable_literal_keys[]="ALTER TABLE Literals ENABLE KEYS";
  char unlock_tables[]="UNLOCK TABLES";
  char flush_statements[]="FLUSH TABLE Statements";
  char *query=NULL;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", unlock_tables);
#endif
  if(mysql_real_query(handle, unlock_tables,
                      strlen(unlock_tables))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL table unlocking failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return 1;
  }

  if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(enable_statement_keys)+21))) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 1;
  }
  sprintf(query, enable_statement_keys, context->model);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, query, strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL statement key re-enabling failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  LIBRDF_FREE(cstring,query);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", enable_literal_keys);
#endif
  if(mysql_real_query(handle, enable_literal_keys,
                      strlen(enable_literal_keys))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL literal key re-enabling failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", flush_statements);
#endif
  if(context->merge && mysql_real_query(handle, flush_statements,
                      strlen(flush_statements))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL table flush failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }

  librdf_storage_mysql_release_handle(storage, handle);
  return 0;
}


/**
 * librdf_storage_mysql_context_add_statements:
 * @storage: the storage
 * @context_node: #librdf_node object
 * @statement_stream: the stream of statements
 *
 * .
 *
 * Add statements in stream to storage, with context.
 *
 * Return value: Non-zero on failure.
 **/
static int
librdf_storage_mysql_context_add_statements(librdf_storage* storage,
                                            librdf_node* context_node,
                                            librdf_stream* statement_stream)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  u64 ctxt=0;
  int helper=0;

  /* Optimize for bulk loads? */
  if(context->bulk) {
    if(librdf_storage_mysql_start_bulk(storage))
      return 1;
  }
  
  /* Find hash for context, creating if necessary */
  if(context_node) {
    ctxt=librdf_storage_mysql_store_node(storage,context_node);
    if(!ctxt)
      return 1;
  }

  while(!helper && !librdf_stream_end(statement_stream)) {
    librdf_statement* statement=librdf_stream_get_object(statement_stream);
    helper=librdf_storage_mysql_context_add_statement_helper(storage, ctxt,
                                                             statement);
    librdf_stream_next(statement_stream);
  }

  return helper;
}


/**
 * librdf_storage_mysql_context_add_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to add
 *
 * Add a statement to a storage context.
 *
 * Return value: non 0 on failure
 **/
static int
librdf_storage_mysql_context_add_statement(librdf_storage* storage,
                                          librdf_node* context_node,
                                          librdf_statement* statement)
{
  u64 ctxt=0;

  /* Find hash for context, creating if necessary */
  if(context_node) {
    ctxt=librdf_storage_mysql_store_node(storage,context_node);
    if(!ctxt)
      return 1;
  }

  return librdf_storage_mysql_context_add_statement_helper(storage, ctxt,
                                                           statement);
}


/*
 * librdf_storage_mysql_context_add_statement_helper - Perform actual addition of a statement to a storage context
 * @storage: #librdf_storage object
 * @ctxt: u64 context hash
 * @statement: #librdf_statement statement to add
 *
 * Return value: non-zero on failure
 **/
static int
librdf_storage_mysql_context_add_statement_helper(librdf_storage* storage,
                                          u64 ctxt, librdf_statement* statement)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  char insert_statement[]="INSERT INTO Statements" UINT64_T_FMT " (Subject,Predicate,Object,Context) VALUES (" UINT64_T_FMT "," UINT64_T_FMT "," UINT64_T_FMT "," UINT64_T_FMT ")";
  u64 subject, predicate, object;
  char *query=NULL;
  MYSQL *handle=NULL;
  int rc=0;
  
  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

  /* Find hashes for nodes, creating if necessary */
  subject=librdf_storage_mysql_store_node(storage,
                                          librdf_statement_get_subject(statement));
  predicate=librdf_storage_mysql_store_node(storage,
                                            librdf_statement_get_predicate(statement));
  object=librdf_storage_mysql_store_node(storage,
                                         librdf_statement_get_object(statement));
  if(!subject || !predicate || !object) {
    rc=1;
    goto tidy;
  }

  if(context->transaction_handle) {
    /* in a transaction */
    pending_row* prow;
    
    prow=(pending_row*)LIBRDF_CALLOC(pending_row, sizeof(pending_row), 1);
    prow->key_len=4;
    prow->uints[0]=subject;
    prow->uints[1]=predicate;
    prow->uints[2]=object;
    prow->uints[3]=ctxt;
    raptor_sequence_push(context->pending_statements, prow);
    
  } else {
    /* not a transaction - add statement to storage */
    if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(insert_statement)+101))) {
      rc=1;
      goto tidy;
    }
    sprintf(query, insert_statement, context->model, subject, predicate, object, ctxt);
    
#ifdef LIBRDF_DEBUG_SQL
    LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
    if(mysql_real_query(handle, query, strlen(query))) {
      librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "MySQL insert into Statements failed: %s",
                 mysql_error(handle));
      rc=-1;
      goto tidy;
    }
  }

  tidy:
  if(query)
    LIBRDF_FREE(cstring,query);
  if(handle) {    
    librdf_storage_mysql_release_handle(storage, handle);
  }

  return 0;
}


/**
 * librdf_storage_mysql_contains_statement:
 * @storage: the storage
 * @statement: a complete statement
 *
 * Test if a given complete statement is present in the model.
 *
 * Return value: Non-zero if the model contains the statement.
 **/
static int
librdf_storage_mysql_contains_statement(librdf_storage* storage,
                                        librdf_statement* statement)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  char find_statement[]="SELECT 1 FROM Statements" UINT64_T_FMT " WHERE Subject=" UINT64_T_FMT " AND Predicate=" UINT64_T_FMT " AND Object=" UINT64_T_FMT " limit 1";
  u64 subject, predicate, object;
  char *query;
  MYSQL_RES *res;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 0;

  /* Find hashes for nodes */
  subject=librdf_storage_mysql_get_node_hash(storage,
                                             librdf_statement_get_subject(statement));
  predicate=librdf_storage_mysql_get_node_hash(storage,
                                               librdf_statement_get_predicate(statement));
  object=librdf_storage_mysql_get_node_hash(storage,
                                            librdf_statement_get_object(statement));
  if(!subject || !predicate || !object) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 0;
  }

  /* Check for statement */
  if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(find_statement)+81))) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 0;
  }
  sprintf(query, find_statement, context->model, subject, predicate, object);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, query, strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL query for statement failed: %s",
               mysql_error(handle));
    LIBRDF_FREE(cstring,query);

    librdf_storage_mysql_release_handle(storage, handle);
    return 0;
  }
  LIBRDF_FREE(cstring, query);
  if(!(res=mysql_store_result(handle)) ||
     !(mysql_fetch_row(res))) {
    if(res)
      mysql_free_result(res);

    librdf_storage_mysql_release_handle(storage, handle);
    return 0;
  }
  if(res)
    mysql_free_result(res);

  librdf_storage_mysql_release_handle(storage, handle);

  return 1;
}


/**
 * librdf_storage_mysql_remove_statement:
 * @storage: #librdf_storage object
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from storage.
 *
 * Return value: non-zero on failure
 **/
static int
librdf_storage_mysql_remove_statement(librdf_storage* storage, librdf_statement* statement)
{
  return librdf_storage_mysql_context_remove_statement(storage,NULL,statement);
}


/**
 * librdf_storage_mysql_context_remove_statement:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 * @statement: #librdf_statement statement to remove
 *
 * Remove a statement from a storage context.
 *
 * Return value: non-zero on failure
 **/
static int
librdf_storage_mysql_context_remove_statement(librdf_storage* storage,
                                             librdf_node* context_node,
                                             librdf_statement* statement)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  char delete_statement[]="DELETE FROM Statements" UINT64_T_FMT " WHERE Subject=" UINT64_T_FMT " AND Predicate=" UINT64_T_FMT " AND Object=" UINT64_T_FMT;
  char delete_statement_with_context[]="DELETE FROM Statements" UINT64_T_FMT " WHERE Subject=" UINT64_T_FMT " AND Predicate=" UINT64_T_FMT " AND Object=" UINT64_T_FMT " AND Context=" UINT64_T_FMT;
  u64 subject, predicate, object, ctxt=0;
  char *query;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

  /* Find hashes for nodes */
  subject=librdf_storage_mysql_get_node_hash(storage,
                                             librdf_statement_get_subject(statement));
  predicate=librdf_storage_mysql_get_node_hash(storage,
                                           librdf_statement_get_predicate(statement));
  object=librdf_storage_mysql_get_node_hash(storage,
                                        librdf_statement_get_object(statement));
  if(context_node) {
    ctxt=librdf_storage_mysql_get_node_hash(storage,context_node);
    if(!ctxt) {
      librdf_storage_mysql_release_handle(storage, handle);
      return 1;
    }
  }
  if(!subject || !predicate || !object || (context_node && !ctxt)) {
    librdf_storage_mysql_release_handle(storage, handle);
    return 1;
  }

  /* Remove statement(s) from storage */
  if(context_node) {
    if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(delete_statement_with_context)+101))) {
      librdf_storage_mysql_release_handle(storage, handle);
      return 1;
    }
    sprintf(query, delete_statement_with_context, context->model, subject,
            predicate, object, ctxt);
  } else {
    if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(delete_statement)+81))) {
      librdf_storage_mysql_release_handle(storage, handle);
      return 1;
    }
    sprintf(query, delete_statement, context->model, subject, predicate,
            object);
  }

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle, query, strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL delete from Statements failed: %s",
               mysql_error(handle));
    LIBRDF_FREE(cstring,query);

    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  LIBRDF_FREE(cstring,query);

  librdf_storage_mysql_release_handle(storage, handle);

  return 0;
}


/**
 * librdf_storage_mysql_context_remove_statements:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 *
 * Remove all statement from a storage context.
 *
 * Return value: non-zero on failure
 **/
static int
librdf_storage_mysql_context_remove_statements(librdf_storage* storage,
                                               librdf_node* context_node)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  char delete_context[]="DELETE FROM Statements" UINT64_T_FMT " WHERE Context=" UINT64_T_FMT;
  char delete_model[]="DELETE FROM Statements" UINT64_T_FMT;
  char flush_statements[]="FLUSH TABLE Statements";
  u64 ctxt=0;
  char *query;
  MYSQL *handle;

  /* Get MySQL connection handle */
  handle=librdf_storage_mysql_get_handle(storage);
  if(!handle)
    return 1;

  /* Find hash for context */
  if(context_node) {
    ctxt=librdf_storage_mysql_get_node_hash(storage,context_node);
    if(!ctxt) {
      librdf_storage_mysql_release_handle(storage, handle);
      return 1;
    }
  }

  /* Remove statement(s) from storage */
  if(context_node) {
    if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(delete_context)+61))) {
      librdf_storage_mysql_release_handle(storage, handle);
      return 1;
    }
    sprintf(query, delete_context, context->model, ctxt);
  } else {
    if(!(query=(char*)LIBRDF_MALLOC(cstring, strlen(delete_model)+21))) {
      librdf_storage_mysql_release_handle(storage, handle);
      return 1;
    }
    sprintf(query, delete_model, context->model);
  }

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(handle,query,strlen(query))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL delete of context from Statements failed: %s",
               mysql_error(handle));
    LIBRDF_FREE(cstring,query);

    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }
  LIBRDF_FREE(cstring,query);

  /* Flush merge table when using delete without where... */
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", flush_statements);
#endif
  if(context->merge && !context_node
      && mysql_real_query(handle, flush_statements,
                                   strlen(flush_statements))) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL table flush failed: %s",
               mysql_error(handle));
    librdf_storage_mysql_release_handle(storage, handle);
    return -1;
  }

  librdf_storage_mysql_release_handle(storage, handle);

  return 0;
}


/**
 * librdf_storage_mysql_serialise:
 * @storage: the storage
 *
 * Return a stream of all statements in a storage.
 *
 * Return a stream of all statements in a storage.
 *
 * Return value: a #librdf_stream or NULL on failure
 **/
static librdf_stream*
librdf_storage_mysql_serialise(librdf_storage* storage)
{
  return librdf_storage_mysql_find_statements_in_context(storage,NULL,NULL);
}


/**
 * librdf_storage_mysql_find_statements:
 * @storage: the storage
 * @statement: the statement to match
 *
 * Find a graph of statements in storage.
 *
 * Return a stream of statements matching the given statement (or
 * all statements if NULL).  Parts (subject, predicate, object) of the
 * statement can be empty in which case any statement part will match that.
 *
 * Return value: a #librdf_stream or NULL on failure
 **/
static librdf_stream*
librdf_storage_mysql_find_statements(librdf_storage* storage,
                                     librdf_statement* statement)
{
  return librdf_storage_mysql_find_statements_in_context(storage,statement,NULL);
}


/**
 * librdf_storage_mysql_context_serialise:
 * @storage: #librdf_storage object
 * @context_node: #librdf_node object
 *
 * List all statements in a storage context.
 *
 * Return value: #librdf_stream of statements or NULL on failure or context is empty
 **/
static librdf_stream*
librdf_storage_mysql_context_serialise(librdf_storage* storage,
                                       librdf_node* context_node)
{
  return librdf_storage_mysql_find_statements_in_context(storage,NULL,context_node);
}


/**
 * librdf_storage_mysql_find_statements_in_context:
 * @storage: the storage
 * @statement: the statement to match
 * @context_node: the context to search
 *
 * Find a graph of statements in a storage context.
 *
 * Return a stream of statements matching the given statement (or
 * all statements if NULL).  Parts (subject, predicate, object) of the
 * statement can be empty in which case any statement part will match that.
 *
 * Return value: a #librdf_stream or NULL on failure
 **/
static librdf_stream*
librdf_storage_mysql_find_statements_in_context(librdf_storage* storage, librdf_statement* statement,
                         librdf_node* context_node)
{
  return librdf_storage_mysql_find_statements_with_options(storage, statement, context_node, NULL);
}


/**
 * librdf_storage_mysql_find_statements_with_options:
 * @storage: the storage
 * @statement: the statement to match
 * @context_node: the context to search
 * @options: #librdf_hash of match options or NULL
 *
 * Find a graph of statements in a storage context with options.
 *
 * Return a stream of statements matching the given statement (or
 * all statements if NULL).  Parts (subject, predicate, object) of the
 * statement can be empty in which case any statement part will match that.
 *
 * Return value: a #librdf_stream or NULL on failure
 **/
static librdf_stream*
librdf_storage_mysql_find_statements_with_options(librdf_storage* storage, 
                                                  librdf_statement* statement,
                                                  librdf_node* context_node,
                                                  librdf_hash* options)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  librdf_storage_mysql_sos_context* sos;
  librdf_node *subject=NULL, *predicate=NULL, *object=NULL;
  char *query;
  char tmp[64];
  char where[256];
  char joins[640];
  librdf_stream *stream;

  /* Initialize sos context */
  if(!(sos=(librdf_storage_mysql_sos_context*)
      LIBRDF_CALLOC(librdf_storage_mysql_sos_context,1,
                    sizeof(librdf_storage_mysql_sos_context))))
    return NULL;
  sos->storage=storage;
  librdf_storage_add_reference(sos->storage);

  if(statement)
    sos->query_statement=librdf_new_statement_from_statement(statement);
  if(context_node)
    sos->query_context=librdf_new_node_from_node(context_node);
  sos->current_statement=NULL;
  sos->current_context=NULL;
  sos->results=NULL;

  if(options) {
    sos->is_literal_match=librdf_hash_get_as_boolean(options, "match-substring");
  }

  /* Get MySQL connection handle */
  sos->handle=librdf_storage_mysql_get_handle(storage);
  if(!sos->handle) {
    librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
    return NULL;
  }

  /* Construct query */
  if(!(query=(char*)LIBRDF_MALLOC(cstring, 21))) {
    librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
    return NULL;
  }
  strcpy(query, "SELECT");
  *where='\0';
  if(sos->is_literal_match)
    sprintf(joins, " FROM Literals AS L LEFT JOIN Statements" UINT64_T_FMT " as S ON L.ID=S.Object",
            context->model);
  else
    sprintf(joins, " FROM Statements" UINT64_T_FMT " AS S", context->model);

  if(statement) {
    subject=librdf_statement_get_subject(statement);
    predicate=librdf_statement_get_predicate(statement);
    object=librdf_statement_get_object(statement);
  }

  /* Subject */
  if(statement && subject) {
    sprintf(tmp, "S.Subject=" UINT64_T_FMT "",
            librdf_storage_mysql_get_node_hash(storage,subject));
    if(!strlen(where))
      strcat(where, " WHERE ");
    else
      strcat(where, " AND ");
    strcat(where, tmp);
  } else {
    if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, " SubjectR.URI AS SuR, SubjectB.Name AS SuB")) {
      librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
      return NULL;
    }
    strcat(joins," LEFT JOIN Resources AS SubjectR ON S.Subject=SubjectR.ID");
    strcat(joins," LEFT JOIN Bnodes AS SubjectB ON S.Subject=SubjectB.ID");
  }

  /* Predicate */
  if(statement && predicate) {
    sprintf(tmp, "S.Predicate=" UINT64_T_FMT "",
            librdf_storage_mysql_get_node_hash(storage, predicate));
    if(!strlen(where))
      strcat(where, " WHERE ");
    else
      strcat(where, " AND ");
    strcat(where, tmp);
  } else {
    if(!statement || !subject) {
      if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, ",")) {
        librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
        return NULL;
      }
    }
    if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, " PredicateR.URI AS PrR")) {
      librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
      return NULL;
    }
    strcat(joins," LEFT JOIN Resources AS PredicateR ON S.Predicate=PredicateR.ID");
  }

  /* Object */
  if(statement && object) {
    if(!sos->is_literal_match) {
      sprintf(tmp,"S.Object=" UINT64_T_FMT "",
              librdf_storage_mysql_get_node_hash(storage, object));
      if(!strlen(where))
        strcat(where, " WHERE ");
      else
        strcat(where, " AND ");
      strcat(where, tmp);
    } else {
      /* MATCH literal, not hash_id */
      if(!statement || !subject || !predicate) {
        if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, ",")) {
          librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
          return NULL;
        }
      }
      if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, " ObjectR.URI AS ObR, ObjectB.Name AS ObB, ObjectL.Value AS ObV, ObjectL.Language AS ObL, ObjectL.Datatype AS ObD")) {
        librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
        return NULL;
      }
      strcat(joins," LEFT JOIN Resources AS ObjectR ON S.Object=ObjectR.ID");
      strcat(joins," LEFT JOIN Bnodes AS ObjectB ON S.Object=ObjectB.ID");
      strcat(joins," LEFT JOIN Literals AS ObjectL ON S.Object=ObjectL.ID");

      sprintf(tmp, "MATCH(L.Value) AGAINST ('%s')",
              librdf_node_get_literal_value(object));

      /* NOTE:  This is NOT USED but could be if FULLTEXT wasn't enabled */
      /*
        sprintf(tmp, " L.Value LIKE '%%%s%%'",
                librdf_node_get_literal_value(object));
       */
      if(!strlen(where))
        strcat(where, " WHERE ");
      else
        strcat(where, " AND ");
      strcat(where, tmp);
    }
  } else {
    if(!statement || !subject || !predicate) {
      if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, ",")) {
        librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
        return NULL;
      }
    }
    if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, " ObjectR.URI AS ObR, ObjectB.Name AS ObB, ObjectL.Value AS ObV, ObjectL.Language AS ObL, ObjectL.Datatype AS ObD")) {
      librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
      return NULL;
    }
    strcat(joins," LEFT JOIN Resources AS ObjectR ON S.Object=ObjectR.ID");
    strcat(joins," LEFT JOIN Bnodes AS ObjectB ON S.Object=ObjectB.ID");
    strcat(joins," LEFT JOIN Literals AS ObjectL ON S.Object=ObjectL.ID");
  }

  /* Context */
  if(context_node) {
    sprintf(tmp,"S.Context=" UINT64_T_FMT "",
            librdf_storage_mysql_get_node_hash(storage,context_node));
    if(!strlen(where))
      strcat(where, " WHERE ");
    else
      strcat(where, " AND ");
    strcat(where, tmp);
  } else {
    if(!statement || !subject || !predicate || !object) {
      if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, ",")) {
        librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
        return NULL;
      }
    }
    if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, " ContextR.URI AS CoR, ContextB.Name AS CoB, ContextL.Value AS CoV, ContextL.Language AS CoL, ContextL.Datatype AS CoD")) {
      librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
      return NULL;
    }
    strcat(joins," LEFT JOIN Resources AS ContextR ON S.Context=ContextR.ID");
    strcat(joins," LEFT JOIN Bnodes AS ContextB ON S.Context=ContextB.ID");
    strcat(joins," LEFT JOIN Literals AS ContextL ON S.Context=ContextL.ID");
  }

  /* Query without variables? */
  if(statement && subject && predicate && object && context_node) {
    if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, " 1")) {
      librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
      return NULL;
    }
  }

  /* Complete query string */
  if(librdf_storage_mysql_find_statements_in_context_augment_query(&query, joins) ||
      librdf_storage_mysql_find_statements_in_context_augment_query(&query, where)) {
    librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
    return NULL;
  }

  /* Start query... */
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(sos->handle, query, strlen(query)) ||
     !(sos->results=mysql_use_result(sos->handle))) {
    librdf_log(sos->storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL query failed: %s",
               mysql_error(sos->handle));
    librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
    return NULL;
  }
  LIBRDF_FREE(cstring, query);

  /* Get first statement, if any, and initialize stream */
  if(librdf_storage_mysql_find_statements_in_context_next_statement(sos) ||
      !sos->current_statement) {
    librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
    return librdf_new_empty_stream(storage->world);
  }

  stream=librdf_new_stream(storage->world,(void*)sos,
                           &librdf_storage_mysql_find_statements_in_context_end_of_stream,
                           &librdf_storage_mysql_find_statements_in_context_next_statement,
                           &librdf_storage_mysql_find_statements_in_context_get_statement,
                           &librdf_storage_mysql_find_statements_in_context_finished);
  if(!stream) {
    librdf_storage_mysql_find_statements_in_context_finished((void*)sos);
    return NULL;
  }

  return stream;
}


static int
librdf_storage_mysql_find_statements_in_context_augment_query(char **query, 
                                                              const char *addition)
{
  char *newquery;

  /* Augment existing query, returning 0 on success. */
  if(!(newquery=(char*)LIBRDF_MALLOC(cstring, strlen(*query)+strlen(addition)+1)))
      return 1;
  strcpy(newquery,*query);
  strcat(newquery,addition);
  LIBRDF_FREE(cstring, *query);
  *query=newquery;

  return 0;
}


static int
librdf_storage_mysql_find_statements_in_context_end_of_stream(void* context)
{
  librdf_storage_mysql_sos_context* sos=(librdf_storage_mysql_sos_context*)context;

  return sos->current_statement==NULL;
}


static int
librdf_storage_mysql_find_statements_in_context_next_statement(void* context)
{
  librdf_storage_mysql_sos_context* sos=(librdf_storage_mysql_sos_context*)context;
  MYSQL_ROW row;
  librdf_node *subject=NULL, *predicate=NULL, *object=NULL;
  librdf_node *node;

  /* Get next statement */
  row=mysql_fetch_row(sos->results);
  if(row) {
    /* Get ready for context */
    if(sos->current_context)
      librdf_free_node(sos->current_context);
    sos->current_context=NULL;
    /* Is this a query with statement parts? */
    if(sos->query_statement) {
      subject=librdf_statement_get_subject(sos->query_statement);
      predicate=librdf_statement_get_predicate(sos->query_statement);
      if(sos->is_literal_match)
        object=NULL;
      else
        object=librdf_statement_get_object(sos->query_statement);
    }
    /* Make sure we have a statement object to return */
    if(!sos->current_statement) {
      if(!(sos->current_statement=librdf_new_statement(sos->storage->world)))
        return 1;

    }
    librdf_statement_clear(sos->current_statement);
    /* Query without variables? */
    if(subject && predicate && object && sos->query_context) {
      librdf_statement_set_subject(sos->current_statement,librdf_new_node_from_node(subject));
      librdf_statement_set_predicate(sos->current_statement,librdf_new_node_from_node(predicate));
      librdf_statement_set_object(sos->current_statement,librdf_new_node_from_node(object));
      sos->current_context=librdf_new_node_from_node(sos->query_context);
    } else {
      /* Turn row parts into statement and context */
      int part=0;
      /* Subject - constant or from row? */
      if(subject) {
        librdf_statement_set_subject(sos->current_statement,librdf_new_node_from_node(subject));
      } else {
        /* Resource or Bnode? */
        if(row[part]) {
          if(!(node=librdf_new_node_from_uri_string(sos->storage->world,
                                                     (const unsigned char*)row[part])))
            return 1;
        } else if(row[part+1]) {
          if(!(node=librdf_new_node_from_blank_identifier(sos->storage->world,
                                                           (const unsigned char*)row[part+1])))
            return 1;
        } else
          return 1;

        librdf_statement_set_subject(sos->current_statement,node);
        part+=2;
      }
      /* Predicate - constant or from row? */
      if(predicate) {
        librdf_statement_set_predicate(sos->current_statement,librdf_new_node_from_node(predicate));
      } else {
        /* Resource? */
        if(row[part]) {
          if(!(node=librdf_new_node_from_uri_string(sos->storage->world,
                                                     (const unsigned char*)row[part])))
            return 1;
        } else
          return 1;

        librdf_statement_set_predicate(sos->current_statement,node);
        part+=1;
      }
      /* Object - constant or from row? */
      if(object) {
        librdf_statement_set_object(sos->current_statement,librdf_new_node_from_node(object));
      } else {
        /* Resource, Bnode or Literal? */
        if(row[part]) {
          if(!(node=librdf_new_node_from_uri_string(sos->storage->world,
                                                     (const unsigned char*)row[part])))
            return 1;
        } else if(row[part+1]) {
          if(!(node=librdf_new_node_from_blank_identifier(sos->storage->world,
                                                           (const unsigned char*)row[part+1])))
            return 1;
        } else if(row[part+2]) {
          /* Typed literal? */
          librdf_uri *datatype=NULL;
          if(row[part+4] && strlen(row[part+4]))
            datatype=librdf_new_uri(sos->storage->world,
                                    (const unsigned char*)row[part+4]);
          if(!(node=librdf_new_node_from_typed_literal(sos->storage->world,
                                                        (const unsigned char*)row[part+2],
                                                        row[part+3],
                                                        datatype)))
            return 1;
        } else
          return 1;

        librdf_statement_set_object(sos->current_statement,node);
        part+=5;
      }
      /* Context - constant or from row? */
      if(sos->query_context) {
        sos->current_context=librdf_new_node_from_node(sos->query_context);
      } else {
        /* Resource, Bnode or Literal? */
        if(row[part]) {
          if(!(node=librdf_new_node_from_uri_string(sos->storage->world,
                                                     (const unsigned char*)row[part])))
            return 1;
        } else if(row[part+1]) {
          if(!(node=librdf_new_node_from_blank_identifier(sos->storage->world,
                                                           (const unsigned char*)row[part+1])))
            return 1;
        } else if(row[part+2]) {
          /* Typed literal? */
          librdf_uri *datatype=NULL;
          if(row[part+4] && strlen(row[part+4]))
            datatype=librdf_new_uri(sos->storage->world,
                                    (const unsigned char*)row[part+4]);
          if(!(node=librdf_new_node_from_typed_literal(sos->storage->world,
                                                        (const unsigned char*)row[part+2],
                                                        row[part+3],
                                                        datatype)))
            return 1;
        } else
          /* no context */
          node=NULL;
        sos->current_context=node;
      }
    }
  } else {
    if(sos->current_statement)
      librdf_free_statement(sos->current_statement);
    sos->current_statement=NULL;
    if(sos->current_context)
      librdf_free_node(sos->current_context);
    sos->current_context=NULL;
  }

  return 0;
}


static void*
librdf_storage_mysql_find_statements_in_context_get_statement(void* context, int flags)
{
  librdf_storage_mysql_sos_context* sos=(librdf_storage_mysql_sos_context*)context;

  switch(flags) {
    case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
      return sos->current_statement;
    case LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT:
      return sos->current_context;
    default:
      abort();
  }
}


static void
librdf_storage_mysql_find_statements_in_context_finished(void* context)
{
  librdf_storage_mysql_sos_context* sos=(librdf_storage_mysql_sos_context*)context;

  if(sos->results)
    mysql_free_result(sos->results);

  if(sos->handle) {
    librdf_storage_mysql_release_handle(sos->storage, sos->handle);
  }

  if(sos->current_statement)
    librdf_free_statement(sos->current_statement);

  if(sos->current_context)
    librdf_free_node(sos->current_context);

  if(sos->query_statement)
    librdf_free_statement(sos->query_statement);

  if(sos->query_context)
    librdf_free_node(sos->query_context);

  if(sos->storage)
    librdf_storage_remove_reference(sos->storage);

  LIBRDF_FREE(librdf_storage_mysql_sos_context, sos);
}


/**
 * librdf_storage_mysql_get_contexts:
 * @storage: the storage
 *
 * Return an iterator with the context nodes present in storage.
 *
 * Return value: a #librdf_iterator or NULL on failure
 **/
static librdf_iterator*
librdf_storage_mysql_get_contexts(librdf_storage* storage)
{
  librdf_storage_mysql_context* context=(librdf_storage_mysql_context*)storage->context;
  librdf_storage_mysql_get_contexts_context* gccontext;
  const char select_contexts[]="\
SELECT DISTINCT R.URI AS CoR, B.Name AS CoB, \
L.Value AS CoV, L.Language AS CoL, L.Datatype AS CoD \
FROM Statements" UINT64_T_FMT " as S \
LEFT JOIN Resources AS R ON S.Context=R.ID \
LEFT JOIN Bnodes AS B ON S.Context=B.ID \
LEFT JOIN Literals AS L ON S.Context=L.ID";
  char *query;
  librdf_iterator* iterator;

  /* Initialize get_contexts context */
  if(!(gccontext=(librdf_storage_mysql_get_contexts_context*)
      LIBRDF_CALLOC(librdf_storage_mysql_get_contexts_context,1,
                    sizeof(librdf_storage_mysql_get_contexts_context))))
    return NULL;
  gccontext->storage=storage;
  librdf_storage_add_reference(gccontext->storage);

  gccontext->current_context=NULL;
  gccontext->results=NULL;

  /* Get MySQL connection handle */
  gccontext->handle=librdf_storage_mysql_get_handle(storage);
  if(!gccontext->handle) {
    librdf_storage_mysql_get_contexts_finished((void*)gccontext);
    return NULL;
  }

  /* Construct query */
  if(!(query=(char*)LIBRDF_MALLOC(cstring,strlen(select_contexts)+21))) {
    librdf_storage_mysql_get_contexts_finished((void*)gccontext);
    return NULL;
  }
  sprintf(query, select_contexts, context->model);

  /* Start query... */
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(gccontext->handle, query, strlen(query)) ||
     !(gccontext->results=mysql_use_result(gccontext->handle))) {
    librdf_log(gccontext->storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL query failed: %s",
               mysql_error(gccontext->handle));
    librdf_storage_mysql_get_contexts_finished((void*)gccontext);
    return NULL;
  }
  LIBRDF_FREE(cstring, query);

  /* Get first context, if any, and initialize iterator */
  if(librdf_storage_mysql_get_contexts_next_context(gccontext) ||
      !gccontext->current_context) {
    librdf_storage_mysql_get_contexts_finished((void*)gccontext);
    return librdf_new_empty_iterator(storage->world);
  }

  iterator=librdf_new_iterator(storage->world,(void*)gccontext,
                               &librdf_storage_mysql_get_contexts_end_of_iterator,
                               &librdf_storage_mysql_get_contexts_next_context,
                               &librdf_storage_mysql_get_contexts_get_context,
                               &librdf_storage_mysql_get_contexts_finished);
  if(!iterator)
    librdf_storage_mysql_get_contexts_finished(gccontext);
  return iterator;
}


static int
librdf_storage_mysql_get_contexts_end_of_iterator(void* context)
{
  librdf_storage_mysql_get_contexts_context* gccontext=(librdf_storage_mysql_get_contexts_context*)context;

  return gccontext->current_context==NULL;
}


static int
librdf_storage_mysql_get_contexts_next_context(void* context)
{
  librdf_storage_mysql_get_contexts_context* gccontext=(librdf_storage_mysql_get_contexts_context*)context;
  MYSQL_ROW row;
  librdf_node *node;

  /* Get next statement */
  row=mysql_fetch_row(gccontext->results);
  if(row) {
    /* Free old context node, if allocated */
    if(gccontext->current_context)
      librdf_free_node(gccontext->current_context);
    /* Resource, Bnode or Literal? */
    if(row[0]) {
      if(!(node=librdf_new_node_from_uri_string(gccontext->storage->world,
                                                 (const unsigned char*)row[0])))
        return 1;
    } else if(row[1]) {
      if(!(node=librdf_new_node_from_blank_identifier(gccontext->storage->world,
                                                       (const unsigned char*)row[1])))
        return 1;
    } else if(row[2]) {
      /* Typed literal? */
      librdf_uri *datatype=NULL;
      if(row[4] && strlen(row[4]))
        datatype=librdf_new_uri(gccontext->storage->world,
                                (const unsigned char*)row[4]);
      if(!(node=librdf_new_node_from_typed_literal(gccontext->storage->world,
                                                    (const unsigned char*)row[2],
                                                    row[3],
                                                    datatype)))
        return 1;
    } else
      return 1;

    gccontext->current_context=node;
  } else {
    if(gccontext->current_context)
      librdf_free_node(gccontext->current_context);
    gccontext->current_context=NULL;
  }

  return 0;
}


static void*
librdf_storage_mysql_get_contexts_get_context(void* context, int flags)
{
  librdf_storage_mysql_get_contexts_context* gccontext=(librdf_storage_mysql_get_contexts_context*)context;

  return gccontext->current_context;
}


static void
librdf_storage_mysql_get_contexts_finished(void* context)
{
  librdf_storage_mysql_get_contexts_context* gccontext=(librdf_storage_mysql_get_contexts_context*)context;

  if(gccontext->results)
    mysql_free_result(gccontext->results);

  if(gccontext->handle) {
    librdf_storage_mysql_release_handle(gccontext->storage, gccontext->handle);
  }

  if(gccontext->current_context)
    librdf_free_node(gccontext->current_context);

  if(gccontext->storage)
    librdf_storage_remove_reference(gccontext->storage);

  LIBRDF_FREE(librdf_storage_mysql_get_contexts_context, gccontext);

}


/**
 * librdf_storage_mysql_get_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a storage feature.
 * 
 * Return value: #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
static librdf_node*
librdf_storage_mysql_get_feature(librdf_storage* storage, librdf_uri* feature)
{
  unsigned char *uri_string;

  if(!feature)
    return NULL;

  uri_string=librdf_uri_as_string(feature);
  if(!uri_string)
    return NULL;
  
  if(!strcmp((const char*)uri_string, (const char*)LIBRDF_MODEL_FEATURE_CONTEXTS)) {
    /* Always have contexts */
    static const unsigned char value[2]="1";

    return librdf_new_node_from_typed_literal(storage->world,
                                              value,
                                              NULL, NULL);
  }

  return NULL;
}



/**
 * librdf_storage_mysql_transaction_start:
 * @storage: the storage object
 * 
 * Start a transaction
 * 
 * Return value: non-0 on failure
 **/
static int
librdf_storage_mysql_transaction_start(librdf_storage* storage) 
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context *)storage->context;
  int i;
  
  if(context->transaction_handle) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL transaction already started");
    return 1;
  }
  
  context->transaction_handle=librdf_storage_mysql_get_handle(storage);
  if(!context->transaction_handle) 
    return 1;

  for(i=0; i<= TABLE_STATEMENTS; i++)
    context->pending_inserts[i]=raptor_new_sequence((raptor_sequence_free_handler*)free_pending_row, NULL);

  context->pending_insert_hash_nodes=librdf_new_hash(storage->world, NULL);
  if(!context->pending_insert_hash_nodes)
    LIBRDF_FATAL1(storage->world, LIBRDF_FROM_STORAGE, 
                  "Failed to create MySQL seen nodes hash from factory");
  
  if(librdf_hash_open(context->pending_insert_hash_nodes, NULL, 0, 1, 1, NULL))
    LIBRDF_FATAL1(storage->world, LIBRDF_FROM_STORAGE,
                  "Failed to open MySQL seen nodes hash");

  context->pending_statements=raptor_new_sequence((raptor_sequence_free_handler*)free_pending_row, NULL);

  return 0;
}


/**
 * librdf_storage_mysql_transaction_start_with_handle:
 * @storage: the storage object
 * @handle: the transaction object
 * 
 * Start a transaction using an existing external transaction object.
 * 
 * Return value: non-0 on failure
 **/
static int
librdf_storage_mysql_transaction_start_with_handle(librdf_storage* storage,
                                                   void* handle)
{
  return librdf_storage_mysql_transaction_start(storage);
}


/**
 * INTERNAL - free transaction state
 */
static
void librdf_storage_mysql_transaction_terminate(librdf_storage *storage) 
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context *)storage->context;
  MYSQL* handle=context->transaction_handle;
  int i;
  
  if(!handle)
    return;

  context->transaction_handle=NULL;

  librdf_storage_mysql_release_handle(storage, handle);
  
  for(i=0; i<= TABLE_STATEMENTS; i++) {
    raptor_sequence* seq;
    seq=context->pending_inserts[i];
    if(seq)
      raptor_free_sequence(seq);
    context->pending_inserts[i]=NULL;
  }
  
  if(context->pending_insert_hash_nodes) {
    librdf_free_hash(context->pending_insert_hash_nodes);
    context->pending_insert_hash_nodes=NULL;
  }

  if(context->pending_statements) {
    raptor_free_sequence(context->pending_statements);
    context->pending_statements=NULL;
  }
  
}


/**
 * librdf_storage_mysql_transaction_commit:
 * @storage: the storage object
 * 
 * Commit a transaction.
 * 
 * Return value: non-0 on failure 
 **/
static int
librdf_storage_mysql_transaction_commit(librdf_storage* storage) 
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context *)storage->context;
  const char* query;
  MYSQL* handle;
  int status;
  int i;
  size_t query_len;
  const char start_query[]="START TRANSACTION";
  const table_info *table;
  raptor_stringbuffer* sb=NULL;
  int count=0;

  handle=context->transaction_handle;

  if(!handle)
    return 1;


  /* Take a look to see if there is anything at all to commit */
  count=raptor_sequence_size(context->pending_statements);
  for(i=0; i< TABLE_STATEMENTS; i++)
    count += raptor_sequence_size(context->pending_inserts[i]);
    
  if(!count) {
#ifdef LIBRDF_DEBUG_SQL
    LIBRDF_DEBUG1("Nothing pending to commit\n");
#endif
    librdf_storage_mysql_transaction_terminate(storage);
    return 0;
  }
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("%d items pending to commit\n", count);
#endif



  /* START TRANSACTION */
  query=start_query; query_len=strlen(query);

#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
  if(mysql_real_query(context->transaction_handle, query, query_len)) {
    librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
               "MySQL query failed: %s", 
               mysql_error(context->transaction_handle));
    librdf_storage_mysql_transaction_rollback(storage);    
    return 1;
  }

  /* INSERT node values */
  for(i=0; i< TABLE_STATEMENTS; i++) {
    raptor_sequence* seq;
    raptor_stringbuffer* tsb;

    seq=context->pending_inserts[i];
    table=&mysql_tables[i];

    /* sort pending nodes to always be inserted in same order */
    raptor_sequence_sort(seq, compare_pending_rows);

    tsb=format_pending_row_sequence(table, seq);
    if(!tsb)
      continue;

    query_len=raptor_stringbuffer_length(tsb);
    query=(char*)raptor_stringbuffer_as_string(tsb);
    
#ifdef LIBRDF_DEBUG_SQL
    LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
    if(mysql_real_query(context->transaction_handle, query, query_len)) {
      librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                 "MySQL query to table %s failed: %s", table->name,
                 mysql_error(context->transaction_handle));
      raptor_free_stringbuffer(tsb);
      librdf_storage_mysql_transaction_rollback(storage);    
      return 1;
    }
    
    raptor_free_stringbuffer(tsb);
  }


  /* INSERT STATEMENT* */
  if(raptor_sequence_size(context->pending_statements)) {
    char uint64_buffer[64];
    raptor_sequence* seq;
    
    table=&mysql_tables[TABLE_STATEMENTS];

    /* sort pending statements to always be inserted in same order */
    raptor_sequence_sort(context->pending_statements, 
                         compare_pending_rows);
    
    sb=raptor_new_stringbuffer();
    
    raptor_stringbuffer_append_string(sb, (const unsigned char*)"REPLACE INTO Statements", 1);
    sprintf(uint64_buffer, UINT64_T_FMT, context->model);
    raptor_stringbuffer_append_string(sb, (const unsigned char*)uint64_buffer, 1);
    raptor_stringbuffer_append_counted_string(sb, (const unsigned char*)" (", 2, 1);
    raptor_stringbuffer_append_string(sb, (const unsigned char*)table->columns, 1);
    raptor_stringbuffer_append_counted_string(sb, (const unsigned char*)") VALUES ", 9, 1);

    seq=context->pending_statements;
    for(i=0; i< raptor_sequence_size(seq); i++) {
      pending_row* prow=(pending_row*)raptor_sequence_get_at(seq, i);
      int j;

      if(i > 0)
        raptor_stringbuffer_append_counted_string(sb,
                                             (const unsigned char*)", ", 2, 1);
      
      raptor_stringbuffer_append_counted_string(sb,
                                             (const unsigned char*)"(", 1, 1);

      for(j=0; j < 4; j++) {
        if(j > 0)
          raptor_stringbuffer_append_counted_string(sb,
                                             (const unsigned char*)", ", 2, 1);
        sprintf(uint64_buffer, UINT64_T_FMT, prow->uints[j]);
        raptor_stringbuffer_append_string(sb,
                                       (const unsigned char*)uint64_buffer, 1);
      }

      raptor_stringbuffer_append_counted_string(sb,
                                             (const unsigned char*)")", 1, 1);
    }
    
    query=(char*)raptor_stringbuffer_as_string(sb);
    if(query) {
#ifdef LIBRDF_DEBUG_SQL
      LIBRDF_DEBUG2("SQL: >>%s<<\n", query);
#endif
      if(mysql_real_query(handle, query, strlen(query)) &&
         mysql_errno(handle) != ER_DUP_ENTRY) {
        librdf_log(storage->world, 0, LIBRDF_LOG_ERROR, LIBRDF_FROM_STORAGE, NULL,
                   "MySQL insert into %s failed with error %s",
                   table->name, mysql_error(handle));
        raptor_free_stringbuffer(sb);
        librdf_storage_mysql_transaction_rollback(storage);    
        return 1;
      }
    }
  }
  

  /* COMMIT */
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG1("SQL: mysql_commit()\n");
#endif
  status=mysql_commit(handle);

  librdf_storage_mysql_transaction_terminate(storage);

  if(sb)
    raptor_free_stringbuffer(sb);

  return (status != 0);
}


/**
 * librdf_storage_mysql_transaction_rollback:
 * @storage: the storage object
 * 
 * Rollback a transaction.
 * 
 * Return value: non-0 on failure 
 **/
static int
librdf_storage_mysql_transaction_rollback(librdf_storage* storage)
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context *)storage->context;
  MYSQL* handle;
  int status;
  
  handle=context->transaction_handle;
  if(!handle)
    return 1;
  
#ifdef LIBRDF_DEBUG_SQL
  LIBRDF_DEBUG1("SQL: mysql_rollback()\n");
#endif
  status=mysql_rollback(handle);

  librdf_storage_mysql_transaction_terminate(storage);
  
  return (status != 0);
}


/**
 * librdf_storage_mysql_transaction_get_handle:
 * @storage: the storage object
 * 
 * Get the current transaction handle.
 * 
 * Return value: non-0 on failure 
 **/
static void*
librdf_storage_mysql_transaction_get_handle(librdf_storage* storage) 
{
  librdf_storage_mysql_context *context=(librdf_storage_mysql_context *)storage->context;

  return context->transaction_handle;
}



/* local function to register MySQL storage functions */
static void
librdf_storage_mysql_register_factory(librdf_storage_factory *factory)
{
  factory->context_length     = sizeof(librdf_storage_mysql_context);
  factory->init               = librdf_storage_mysql_init;
  factory->terminate          = librdf_storage_mysql_terminate;
  factory->open               = librdf_storage_mysql_open;
  factory->close              = librdf_storage_mysql_close;
  factory->sync               = librdf_storage_mysql_sync;
  factory->size               = librdf_storage_mysql_size;
  factory->add_statement      = librdf_storage_mysql_add_statement;
  factory->add_statements     = librdf_storage_mysql_add_statements;
  factory->remove_statement   = librdf_storage_mysql_remove_statement;
  factory->contains_statement = librdf_storage_mysql_contains_statement;
  factory->serialise          = librdf_storage_mysql_serialise;
  factory->find_statements    = librdf_storage_mysql_find_statements;
  factory->find_statements_with_options    = librdf_storage_mysql_find_statements_with_options;
  factory->context_add_statement      = librdf_storage_mysql_context_add_statement;
  factory->context_add_statements     = librdf_storage_mysql_context_add_statements;
  factory->context_remove_statement   = librdf_storage_mysql_context_remove_statement;
  factory->context_remove_statements  = librdf_storage_mysql_context_remove_statements;
  factory->context_serialise          = librdf_storage_mysql_context_serialise;
  factory->find_statements_in_context = librdf_storage_mysql_find_statements_in_context;
  factory->get_contexts               = librdf_storage_mysql_get_contexts;
  factory->get_feature                = librdf_storage_mysql_get_feature;

  factory->transaction_start             = librdf_storage_mysql_transaction_start;
  factory->transaction_start_with_handle = librdf_storage_mysql_transaction_start_with_handle;
  factory->transaction_commit            = librdf_storage_mysql_transaction_commit;
  factory->transaction_rollback          = librdf_storage_mysql_transaction_rollback;
  factory->transaction_get_handle        = librdf_storage_mysql_transaction_get_handle;
}


/**
 * librdf_init_storage_mysql:
 * @world: world object
 * 
 * INTERNAL - initialise the storage_mysql module.
 **/
void
librdf_init_storage_mysql(librdf_world *world)
{
  librdf_storage_register_factory(world, "mysql", "MySQL database store",
                                  &librdf_storage_mysql_register_factory);
}
