/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdfproc.c - Redland RDF command processor
 *
 * Copyright (C) 2000-2007, David Beckett http://purl.org/net/dajobe/
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
#include <win32_config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else
#include <rdfproc_getopt.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <unistd.h>

#include <redland.h>
#include <raptor.h>

#ifdef NEED_OPTIND_DECLARATION
extern int optind;
extern char *optarg;
#endif

/* one prototype needed */
int main(int argc, char *argv[]);

static  char *program=NULL;


enum command_type {
  CMD_PRINT,
  CMD_CONTAINS,
  CMD_FIND,
  CMD_SOURCES,
  CMD_ARCS,
  CMD_TARGETS,
  CMD_SOURCE,
  CMD_ARC,
  CMD_TARGET,
  CMD_ADD,
  CMD_REMOVE,
  CMD_ADD_TYPED,
  CMD_PARSE_MODEL,
  CMD_PARSE_STREAM,
  CMD_ARCS_IN,
  CMD_ARCS_OUT,
  CMD_HAS_ARC_IN,
  CMD_HAS_ARC_OUT,
  CMD_QUERY_AS_BINDINGS,
  CMD_QUERY,
  CMD_SERIALIZE,
  CMD_REMOVE_CONTEXT,
  CMD_CONTEXTS,
  CMD_MATCH,
  CMD_SIZE
};

typedef struct
{
  enum command_type type;
  const char *name;
  int min_args; /* min args needed? */
  int max_args; /* max args needed? */
  int write; /* write to db? */
} command;

static command commands[]={
  {CMD_PRINT, "print", 0, 0, 0,},
  {CMD_CONTAINS, "contains", 3, 3, 0},
  {CMD_FIND, "find", 3, 4, 0},
  {CMD_SOURCES, "sources", 2, 2, 0},
  {CMD_ARCS, "arcs", 2, 2, 0},
  {CMD_TARGETS, "targets", 2, 2, 0},
  {CMD_SOURCE, "source", 2, 2, 0},
  {CMD_ARC, "arc", 2, 2, 0},
  {CMD_TARGET, "target", 2, 2, 0},
  {CMD_ADD, "add", 3, 4, 1},
  {CMD_REMOVE, "remove", 3, 4, 1},
  {CMD_ADD_TYPED, "add-typed", 5, 6, 1},
  {CMD_PARSE_MODEL, "parse", 1, 4, 1},
  {CMD_PARSE_STREAM, "parse-stream", 1, 4, 1},
  {CMD_ARCS_IN, "arcs-in", 1, 1, 0},
  {CMD_ARCS_OUT, "arcs-out", 1, 1, 0},
  {CMD_HAS_ARC_IN, "has-arc-in", 2, 2, 0},
  {CMD_HAS_ARC_OUT, "has-arc-out", 2, 2, 0},
  /* FIXME triples-match-query is deliberately not documented */
  {CMD_QUERY, "triples-match-query", 3, 3, 0},
  {CMD_QUERY_AS_BINDINGS, "query", 3, 3, 0},
  {CMD_SERIALIZE, "serialize", 0, 4, 0},
  {CMD_REMOVE_CONTEXT, "remove-context", 1, 1, 0},
  {CMD_CONTEXTS, "contexts", 0, 0, 0},
  {CMD_MATCH, "match", 3, 4, 0},
  {CMD_SIZE, "size", 0, 0, 0},
  {(enum command_type)-1, NULL, 0, 0, 0}  
};
 

#ifdef HAVE_GETOPT_LONG
#define HELP_TEXT(short, long, description) "  -" #short ", --" long "  " description
#define HELP_ARG(short, long) "--" #long
#else
#define HELP_TEXT(short, long, description) "  -" #short "  " description
#define HELP_ARG(short, long) "-" #short
#endif


#define GETOPT_STRING "chno:pr:s:t:Tv"

#ifdef HAVE_GETOPT_LONG
static struct option long_options[] =
{
  /* name, has_arg, flag, val */
  {"contexts", 0, 0, 'c'},
  {"help", 0, 0, 'h'},
  {"new", 0, 0, 'n'},
  {"output", 1, 0, 'o'},
  {"password", 0, 0, 'p'},
  {"results", 1, 0, 'r'},
  {"storage", 1, 0, 's'},
  {"storage-options", 1, 0, 't'},
  {"transactions", 0, 0, 'T'},
  {"version", 0, 0, 'v'},
  {NULL, 0, 0, 0}
};
#endif

static const char *title_format_string="Redland RDF processor utility %s\n";

static const char *default_storage_name="hashes";
static const char *default_storage_options="hash-type='bdb',dir='.'";


static int
log_handler(void *user_data, librdf_log_message *message) 
{
  /* int code=message->code; */ /* The error code */
  raptor_locator *locator=(raptor_locator*)(message->locator);

  /* Do not handle messages below warning*/
  if(message->level < LIBRDF_LOG_WARN)
    return 0;

  if(message->level == LIBRDF_LOG_WARN)
    fprintf(stderr, "%s: Warning - ", program);
  else
    fprintf(stderr, "%s: Error - ", program);

  if(locator) { /* && message->facility == LIBRDF_FROM_PARSER) */
    raptor_print_locator(stderr, locator); 
    fputc(':', stderr);
    fputc(' ', stderr);
 }
  fputs(message->message, stderr);
  fputc('\n', stderr);
  if(message->level >= LIBRDF_LOG_FATAL)
    exit(1);

  /* Handled */
  return 1;
}



static void
print_nodes(FILE* fh, librdf_iterator* iterator) 
{
  int count;
  librdf_node* context_node;
  librdf_node* node;

  /* (Common code) Print out nodes */
  count=0;
  while(!librdf_iterator_end(iterator)) {
    context_node=(librdf_node*)librdf_iterator_get_context(iterator);
    node=(librdf_node*)librdf_iterator_get_object(iterator);
    if(!node) {
      fprintf(stderr, "%s: librdf_iterator_get_object returned NULL\n",
              program);
      break;
    }
    
    fputs("Matched node: ", fh);
    librdf_node_print(node, fh);
    if(context_node) {
      fputs(" with context ", fh);
      librdf_node_print(context_node, fh);
    }
    fputc('\n', fh);
    
    count++;
    librdf_iterator_next(iterator);
  }
  librdf_free_iterator(iterator);
  fprintf(stderr, "%s: matching nodes: %d\n", program, count);
}


static void
print_node(FILE* fh, librdf_node* node) 
{
  if(node) {
    fputs("Matched node: ", fh);
    librdf_node_print(node, fh);
    fputc('\n', fh);
    
    librdf_free_node(node);
  }
}


int
main(int argc, char *argv[]) 
{
  librdf_world* world;
  librdf_parser* parser;
  librdf_serializer* serializer;
  librdf_storage *storage;
  librdf_model* model;
  librdf_node *source, *arc, *target, *node;
  librdf_node* context_node=NULL;
  librdf_stream* stream;
  librdf_iterator* iterator;
  librdf_uri *uri;
  librdf_uri *base_uri=NULL;
  librdf_query *query;
  librdf_query_results *results;
  librdf_hash *options;
  int usage=0;
  int help=0;
  int is_new=0;
  char *identifier=NULL;
  char *cmd=NULL;
  char *name;
  char *mime_type;
  int cmd_index= -1;
  int count;
  int type;
  int result;
  char *p;
  int i;
  int rc;
  int transactions=0;
  char *storage_name=(char*)default_storage_name;
  char *storage_options=(char*)default_storage_options;
  char *storage_password=NULL;
  librdf_statement* partial_statement=NULL;
  unsigned char *uri_string=NULL;
  int free_uri_string=0;
  librdf_model* output_model=NULL;
  librdf_storage* output_storage=NULL;
  librdf_serializer* output_serializer=NULL;
  size_t capacity;
  size_t size;
  const char *query_graph_serializer_syntax_name="rdfxml";
  char* results_format=NULL;

  program=argv[0];
  if((p=strrchr(program, '/')))
    program=p+1;
  else if((p=strrchr(program, '\\')))
    program=p+1;
  argv[0]=program;

  world=librdf_new_world();

  librdf_world_set_logger(world, NULL, log_handler);

  librdf_world_open(world);

  options=librdf_new_hash(world, NULL);
  librdf_hash_open(options, NULL, 0, 1, 1, NULL);

#ifdef HAVE_GETENV
  if((name=getenv("RDFPROC_STORAGE_OPTIONS")))
    storage_options=name;

  if((name=getenv("RDFPROC_STORAGE_TYPE")))
    storage_name=name;
#endif


  while (!usage && !help)
  {
    int c;
#ifdef HAVE_GETOPT_LONG
    int option_index = 0;

    c = getopt_long (argc, argv, GETOPT_STRING, long_options, &option_index);
#else
    c = getopt (argc, argv, GETOPT_STRING);
#endif
    if(c == -1)
      break;

    switch(c) {
      case 0:
      case '?': /* getopt() - unknown option */
        usage=1;
        break;
        
      case 'c':
        librdf_hash_put_strings(options, "contexts", "yes");
        break;

      case 'h':
        help=1;
        break;

      case 'n':
        is_new=1;
        break;

      case 'o':
        if(optarg) {
          if(!strcmp(optarg, "simple"))
            output_serializer=NULL;
          else {
            output_serializer=librdf_new_serializer(world, optarg, NULL, NULL);
            if(!output_serializer) {
              fprintf(stderr, "%s: unknown output serializer `%s' for `" HELP_ARG(o, output) "'\n",
                      program, optarg);
              fprintf(stderr, "Valid arguments are:\n  `simple'   for a simple format (default)\n  `ntriples' for N-Triples\n");
              usage=1;
            } else {
              output_storage = librdf_new_storage(world, NULL, NULL, NULL);
              if(!output_storage) {
                fprintf(stderr, "%s: Failed to create output storage\n", 
                        program);
                librdf_free_serializer(output_serializer);
                output_serializer=NULL;
              } else {
                output_model = librdf_new_model(world, output_storage, NULL);
                if(!output_model) {
                  fprintf(stderr, "%s: Failed to create output storage model\n", 
                          program);
                  librdf_free_storage(output_storage);
                  output_storage=NULL;
                  librdf_free_serializer(output_serializer);
                  output_serializer=NULL;
                }
              }
            }
            
          }
        }
        break;

      case 'p':
        /* gets() the password from stdin, checking for input overflow */
        capacity=0;
        size=0;
        while(!feof(stdin)) {
          if((capacity-size) < 10) {
            capacity += 10;
            p=(char*)malloc(capacity);
            if(storage_password) {
              strncpy(p, storage_password, size);
              free(storage_password);
            }
            storage_password=p;
          }
          rc=fgetc(stdin); /* short enough that I don't care to use fread */
          if(rc == '\n' || rc == EOF) {
            storage_password[size]='\0';
            break;
          }
          storage_password[size++]=(char)rc;
        }

        if(!size) {
          fprintf(stderr, "%s: WARNING: No storage password found on input\n", 
                  program);
          if(storage_password) {
            free(storage_password);
            storage_password=NULL;
          }
        }

        if(storage_password) {
          librdf_hash_put_strings(options, "password", storage_password);
          free(storage_password);
        }
        break;

      case 'r':
        if(optarg) {
          if(!strcmp(optarg, "help")) {
            fprintf(stderr, "%s: Valid query result formats are:\n", program);
            for(i=0; 1; i++) {
              const char *format_name;
              const char *format_label;
              if(rasqal_query_results_formats_enumerate(world->rasqal_world_ptr,
                                                        i,
                                                        &format_name, 
                                                        &format_label,
                                                        NULL, NULL, NULL))
                break;
              printf("  %-20s  %s\n", format_name, format_label);
            }
            exit(0);
          } else {
            if(rasqal_query_results_formats_check(world->rasqal_world_ptr, optarg, NULL, NULL))
              results_format=optarg;
            else {
              fprintf(stderr, "%s: invalid argument `%s' for `" HELP_ARG(r, results) "'\nTry '%s " HELP_ARG(r, results) " help' for a list of valid formats\n",
                      program, optarg, program);
              usage=1;
            }
          }
        }
        break;
        
      case 's':
        if(optarg) {
          if(!librdf_get_storage_factory(world, optarg)) {
            fprintf(stderr, "%s: invalid storage `%s'\n", program, optarg);
            usage=1;
          } else
            storage_name=optarg;
        }
        break;

      case 't':
        storage_options=optarg;
        break;

      case 'T':
        transactions=1;
        break;

      case 'v':
        fputs(librdf_version_string, stdout);
        fputc('\n', stdout);
        exit(0);
    }
    
  }

  if(usage || help)
    goto usage;

  if(optind == argc) {
    usage=2; /* Title and usage */
    goto usage;
  }

  argv+=optind;
  argc-=optind;
  
  identifier=argv[0];
  argv++;
  argc--;

  if(!argc) {
    fprintf(stderr, "%s: No command given\n", program);
    usage=1;
    goto usage;
  }
  
  cmd=argv[0];
  
  for(i=0; commands[i].name; i++)
    if(!strcmp(cmd, commands[i].name)) {
      cmd_index=i;
      break;
    }
  if(cmd_index<0) {
    fprintf(stderr, "%s: No such command `%s'\n", program, argv[0]);
    usage=1;
    goto usage;
  }
    
  argv++;
  argc--;
    
  if(argc < commands[cmd_index].min_args) {
    fprintf(stderr, "%s: Command %s needs %d arguments\n", program, 
            cmd, commands[cmd_index].min_args);
    usage=1;
  } else if(argc > commands[cmd_index].max_args) {
    fprintf(stderr, "%s: Command %s given more than %d arguments\n",
            program, cmd, commands[cmd_index].max_args);
    usage=1;
  } /* otherwise is just fine and argv points to remaining args */


  usage:
  if(usage) {
    if(usage>1) {
      fprintf(stderr, title_format_string, librdf_version_string);
      fputs(librdf_short_copyright_string, stderr);
      fputc('\n', stderr);
    }
    fprintf(stderr, "Try `%s " HELP_ARG(h, help) "' for more information.\n",
                    program);
    exit(1);
  }

  if(help) {
    printf("Usage: %s [options] store-name command arg...\n", program);
    printf(title_format_string, librdf_version_string);
    puts(librdf_short_copyright_string);
    puts("Utility for processing RDF using the Redland library.");
    puts("\nMain options:");
    puts(HELP_TEXT(c, "contexts        ", "Use Redland contexts"));
    puts(HELP_TEXT(h, "help            ", "Print this help, then exit"));
    puts(HELP_TEXT(n, "new             ", "Create a new store (default no)"));
    puts(HELP_TEXT(o, "output FORMAT   ", "Set the triple output format to one of:"));
    puts("    simple                  A simple format (default)\n    ntriples                N-Triples\n    rdfxml                  RDF/XML");
    puts(HELP_TEXT(p, "password        ", "Read storage option 'password' from standard input"));
    puts(HELP_TEXT(r, "results FORMAT  ", "Set the query results format"));
    for(i=0; 1; i++) {
      const char *help_name;
      const char *help_label;
      if(librdf_query_results_formats_enumerate(world, i, 
                                                &help_name, &help_label, 
                                                NULL, NULL))
        break;
      printf("    %-10s              %s", help_name, help_label);
      if(!i)
        puts(" (default)");
      else
        putchar('\n');
    }
    puts(HELP_TEXT(s, "storage TYPE    ", "Set the graph storage type"));
    for(i=0; 1; i++) {
      const char *help_name;
      const char *help_label;
      if(librdf_storage_enumerate(world, i, &help_name, &help_label))
        break;
      printf("    %-10s              %s", help_name, help_label);
      if(!i)
        puts(" (default)");
      else
        putchar('\n');
    }
    printf(HELP_TEXT(t, "storage-options OPTIONS\n                        ", "Storage options (default \"%s\")\n"), default_storage_options);
    puts(HELP_TEXT(v, "version         ", "Print the Redland version"));
    puts("\nCommands:");
    puts("  parse FILE|URI [SYNTAX [BASEURI [CONTEXT]]]");
    puts("  parse-stream FILE|URI [SYNTAX [BASEURI [CONTEXT]]]");
    puts("      Parse RDF syntax (default RDF/XML) in FILE or URI into the graph");
    puts("      with optional BASEURI, into the optional CONTEXT.");
    puts("  print                                     Print the graph triples.");
    puts("  serialize [SYNTAX [URI [MIME-TYPE]]]      Serializes to a syntax (RDF/XML).");
    puts("  query NAME|- URI|- QUERY-STRING           Run QUERY-STRING query in language NAME for bindings");
#if 0
    puts("  triples-match-query NAME URI|- QUERY-STRING  Query for matching triples");
#endif
    puts("  find SUBJECT|- PREDICATE|- OBJECT|- [CONTEXT] Find matching triples");
    puts("  contains SUBJECT PREDICATE OBJECT         Check if triple is in the graph.");
    puts("  contexts                                  List the contexts in the graph.");
    puts("  add SUBJECT PREDICATE OBJECT [CONTEXT]    Add triple to graph.");
    puts("  add-typed SUBJECT PREDICATE OBJECT OBJECT-LANG OBJECT-URI [CONTEXT]");
    puts("                                            Add datatyped triple to graph.");
    puts("  remove SUBJECT PREDICATE OBJECT [CONTEXT] Remove triple from graph.");
    puts("  remove-context CONTEXT                    Remove all triples with CONTEXT.");
    puts("  sources | targets | arcs NODE1 NODE2      Show matching nodes.");
    puts("  source | target | arc NODE1 NODE2         Show 1 matching node.");
    puts("  arcs-in | arcs-out NODE                   Show properties in/out of NODE");
    puts("  has-arc-in | has-arc-out NODE ARC         Check for property in/out of NODE.");
    puts("  size                                      Print the number of triples in the graph.");
    puts("\nNotation:");
    puts("  nodes are either blank node identifiers like _:ABC,");
    puts("    URIs like http://example.org otherwise are literal strings.");
    puts("    - matches any node when allowed.");
    puts("  triples are three nodes (subject, predicate, object)");
    puts("    predicates can only be uris, only objects can be literals");
    puts("  source means subject of triples matching (?,  NODE1, NODE2)");
    puts("  target means object of triples matching (NODE1, NODE2, ?)");
    puts("  arc means predicate of triples matching (NODE1, ?, NODE2)");
    puts("\nReport bugs to http://bugs.librdf.org/");
    puts("Redland home page: http://librdf.org/");
    exit(0);
  }
  
  type=commands[cmd_index].type;

  if(commands[cmd_index].write) {
    librdf_hash_put_strings(options, "write", "yes");
    if(is_new)
      librdf_hash_put_strings(options, "new", "yes");
  }

  librdf_hash_from_string(options, storage_options);


  storage=librdf_new_storage_with_options(world, storage_name, identifier, 
                                          options);

  if(!storage) {
    fprintf(stderr, "%s: Failed to open %s storage '%s'\n", program, 
            storage_name, identifier);
    return(1);
  }

  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create model\n", program);
    return(1);
  }

  if(transactions)
    librdf_model_transaction_start(model);

  /* Do this or gcc moans */
  stream=NULL;
  iterator=NULL;
  parser=NULL;
  serializer=NULL;
  source=NULL;
  arc=NULL;
  target=NULL;
  uri=NULL;
  node=NULL;
  query=NULL;
  results=NULL;

  switch(type) {
    case CMD_PRINT:
      librdf_model_print(model, stdout);
      break;


    case CMD_PARSE_MODEL:
    case CMD_PARSE_STREAM:
      uri_string=(unsigned char *)argv[0];
      if(!access((const char*)uri_string, R_OK)) {
        uri_string=raptor_uri_filename_to_uri_string((char*)uri_string);
        free_uri_string=1;
      }
      uri=librdf_new_uri(world, uri_string);
      if(!uri) {
        fprintf(stderr, "%s: Failed to create URI from %s\n", program, argv[0]);
        break;
      }
      
      parser=librdf_new_parser(world, ((argc > 1) ? argv[1] : NULL), NULL, NULL);
      if(!parser) {
        fprintf(stderr, "%s: Failed to create new parser %s\n", program,
                argv[1]);
        librdf_free_uri(uri);
        break;
      }
      fprintf(stdout, "%s: Parsing URI %s with %s parser\n", program,
              librdf_uri_as_string(uri), 
              (argc > 1) ? argv[1] : "default");
      
      if(argc >= 3 && argv[2]) {
        base_uri=NULL;
        if(strcmp(argv[2], "-")) {
          base_uri=librdf_new_uri(world, (const unsigned char *)argv[2]);
          if(!base_uri) {
            fprintf(stderr, "%s: Failed to create base URI from %s\n", program,
                    argv[2]);
            break;
          }
        }
        fprintf(stderr, "%s: Using base URI %s\n", program,
                (base_uri ? (const char*)librdf_uri_as_string(base_uri) : "NULL"));

        target=NULL; /* context node */
        if(argc >= 4 && argv[3]) {
          if(librdf_heuristic_is_blank_node(argv[3]))
            target=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[3]));
          else
            target=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[3]);
        }
        fprintf(stderr, "%s: Using context node %s\n", program,
                (argv[3] ? argv[3] : "NULL"));
      } else
        base_uri=librdf_new_uri_from_uri(uri);

      rc=0;

      if(type == CMD_PARSE_MODEL && !target) {
        if(librdf_parser_parse_into_model(parser, uri, base_uri, model)) {
          fprintf(stderr, "%s: Failed to parse into the graph\n", program);
          rc=1;
        }
      } else {
        /* either CMD_PARSE_STREAM or it's a parse into context */
        count=0;
        if(!(stream=librdf_parser_parse_as_stream(parser, uri, base_uri))) {
          fprintf(stderr, "%s: Failed to parse RDF as stream\n", program);
          rc=1;
        } else {
          while(!librdf_stream_end(stream)) {
            librdf_statement *statement=librdf_stream_get_object(stream);
            if(!statement) {
              fprintf(stderr, "%s: librdf_stream_next returned NULL\n", program);
              break;
            }
          
            if(target)  /* context node */
              librdf_model_context_add_statement(model, target, statement);
            else
              librdf_model_add_statement(model, statement);
            count++;
            librdf_stream_next(stream);
          }
          librdf_free_stream(stream);  
        }

        if(target) {
          librdf_free_node(target);
          target=NULL;
        }
        
        fprintf(stderr, "%s: Added %d triples\n", program, count);
        rc=1;
      }

      if(rc) {
        librdf_uri* error_count_uri=librdf_new_uri(world, (const unsigned char*)LIBRDF_PARSER_FEATURE_ERROR_COUNT);
        librdf_uri* warning_count_uri=librdf_new_uri(world, (const unsigned char*)LIBRDF_PARSER_FEATURE_WARNING_COUNT);
        librdf_node* error_count_node;
        librdf_node* warning_count_node;
        int error_count, warning_count;

        error_count_node=librdf_parser_get_feature(parser, error_count_uri);
        if(error_count_node) {
          error_count=atoi((const char*)librdf_node_get_literal_value(error_count_node));
          librdf_free_node(error_count_node);
        } else {
          fprintf(stderr, "%s: Could not get parsing error count\n", program);
          error_count= (-1);
        }

        warning_count_node=librdf_parser_get_feature(parser, warning_count_uri);
        if(warning_count_node) {
          warning_count=atoi((const char*)librdf_node_get_literal_value(error_count_node));
          librdf_free_node(warning_count_node);
        } else {
          fprintf(stderr, "%s: Could not get parsing warning count\n", program);
          warning_count= (-1);
        }

        if((error_count >=0) && (warning_count >=0) &&
           error_count+warning_count >0)
          fprintf(stderr,
                  "%s: The parsing returned %d errors and %d warnings\n", 
                  program, error_count, warning_count);
         
        librdf_free_uri(error_count_uri);
        librdf_free_uri(warning_count_uri);
      }
      
      librdf_free_parser(parser);
      librdf_free_uri(uri);
      if(base_uri)
        librdf_free_uri(base_uri);
      break;


    case CMD_SERIALIZE:
        /* args are name (optional), uri (may be NULL), mime_type
         * (optional), base URI (optional)
         */

        uri=NULL;
        name=NULL;
        mime_type=NULL;
        base_uri=NULL;

        if(!argc)
          goto serialize;

        if(strcmp(argv[0], "-"))
          name=argv[0];

        if(argc < 2)
          goto serialize;

        if(strcmp(argv[1], "-")) {
          uri=librdf_new_uri(world, (const unsigned char *)argv[1]);
          if(!uri) {
            fprintf(stderr, "%s: Failed to create URI from %s\n", program, argv[1]);
            break;
          }
        }

        if(argc == 3) {
          if(strcmp(argv[2], "-"))
            mime_type=argv[2];
        }

	if(argc == 4) {
          if(strcmp(argv[3], "-")) {
            base_uri=librdf_new_uri(world, (const unsigned char *)argv[3]);
            if(!base_uri) {
              fprintf(stderr, "%s: Failed to create base URI from %s\n", program, argv[3]);
              break;
            }
          }
        }

      serialize:
        serializer=librdf_new_serializer(world, name, mime_type, uri);
        if(!serializer) {
          fprintf(stderr, "%s: Failed to create new serializer name %s, uri %s, mime type %s\n", program, argv[0], argv[1], argv[2]);
          if(uri)
            librdf_free_uri(uri);
          break;
        }

        librdf_serializer_serialize_model_to_file_handle(serializer, stdout,
                                                         NULL, model);

        librdf_free_serializer(serializer);
        if(uri)
          librdf_free_uri(uri);

        break;
      

    case CMD_QUERY:
      /* args are name, uri (may be NULL), query_string/mime_type */

      name=argv[0];
      if(!strcmp(name, "-"))
        name=NULL;
      
      if(!strcmp(argv[1], "-"))
        uri=NULL;
      else {
        uri=librdf_new_uri(world, (const unsigned char *)argv[1]);
        if(!uri) {
          fprintf(stderr, "%s: Failed to create URI from %s\n", program,
                  argv[1]);
          break;
        }
      }

      query=librdf_new_query(world, name, uri, (const unsigned char *)argv[2],
                             NULL);
      goto printmatching;
      break;


    case CMD_QUERY_AS_BINDINGS:
      /* args are name (optional), uri (may be NULL), query_string */
      uri=NULL;
      name=NULL;
      
      if(strcmp(argv[0], "-"))
        name=argv[0];

      if(!strcmp(argv[1], "-"))
        uri=NULL;
      else {
        uri=librdf_new_uri(world, (const unsigned char *)argv[1]);
        if(!uri) {
          fprintf(stderr, "%s: Failed to create URI from %s\n", program,
                  argv[1]);
          break;
        }
      }

      query=librdf_new_query(world, name, uri, (const unsigned char *)argv[2], NULL);
      if(!query) {
        fprintf(stderr, "%s: Failed to create new query %s\n", program,
                argv[2]);
        if(uri)
          librdf_free_uri(uri);
        break;
      }

      if(!(results=librdf_model_query_execute(model, query))) {
        fprintf(stderr, "%s: Query of model with '%s' failed\n", 
                program, argv[2]);
        if(uri)
          librdf_free_uri(uri);
        librdf_free_query(query);
        query=NULL;
        break;
      }

      if(results_format) {
        raptor_iostream *iostr;
        librdf_query_results_formatter *formatter;

        fprintf(stdout, "%s: Formatting query result as '%s':\n", program,
                results_format);

        iostr=raptor_new_iostream_to_file_handle(stdout);
        formatter=librdf_new_query_results_formatter(results,
                                                     results_format, NULL);

        librdf_query_results_formatter_write(iostr, formatter, results, 
                                             base_uri);

        librdf_free_query_results_formatter(formatter);
        raptor_free_iostream(iostr);
      } else if(librdf_query_results_is_bindings(results)) {
        fprintf(stdout, "%s: Query returned bindings results:\n", program);
        
        while(!librdf_query_results_finished(results)) {
          fputs("result: [", stdout);
          for(i=0; i<librdf_query_results_get_bindings_count(results); i++) {
            librdf_node *value=librdf_query_results_get_binding_value(results, i);
            name=(char*)librdf_query_results_get_binding_name(results, i);

            if(i>0)
              fputs(", ", stdout);
            fprintf(stdout, "%s=", name);
            if(value) {
              librdf_node_print(value, stdout);
              librdf_free_node(value);
            } else
              fputs("NULL", stdout);
          }
          fputs("]\n", stdout);

          librdf_query_results_next(results);
        }

        fprintf(stdout, "%s: Query returned %d results\n", program, 
                librdf_query_results_get_count(results));
     } else if(librdf_query_results_is_boolean(results)) {
       fprintf(stdout, "%s: Query returned boolean result: %s\n",
               program,
               librdf_query_results_get_boolean(results) ? "true" : "false");
     } else if(librdf_query_results_is_graph(results)) {
       librdf_storage* tmp_storage;
       librdf_model* tmp_model;

       tmp_storage=librdf_new_storage(world, NULL, NULL, NULL);
       tmp_model=librdf_new_model(world, tmp_storage, NULL);

       fprintf(stdout, "%s: Query returned graph result:\n", program);
       
       stream=librdf_query_results_as_stream(results);
       if(!stream) {
         fprintf(stderr, "%s: Failed to get query results graph\n", program);
         return(1);
       }
       librdf_model_add_statements(tmp_model, stream);
       librdf_free_stream(stream);

       fprintf(stdout, "%s: Total %d triples\n", program, librdf_model_size(model));

       serializer=librdf_new_serializer(world, 
                                        query_graph_serializer_syntax_name, 
                                        NULL, NULL);
       if(!serializer) {
         fprintf(stderr, 
                 "%s: Failed to create serializer type %s\n", program,
                 query_graph_serializer_syntax_name);
         return(1);
       }

       librdf_serializer_serialize_model_to_file_handle(serializer, stdout, NULL, tmp_model);
       librdf_free_serializer(serializer);
       librdf_free_model(tmp_model);
       librdf_free_storage(tmp_storage);
     } else {
       fprintf(stdout, "%s: Query returned unknown result format\n", program);
       rc=1;
     }
      
      if(uri)
        librdf_free_uri(uri);
      librdf_free_query_results(results);
      break;
      

    case CMD_CONTAINS:
    case CMD_FIND:
    case CMD_MATCH:
    case CMD_ADD:
    case CMD_REMOVE:
    case CMD_ADD_TYPED:
      if(!strcmp(argv[0], "-"))
        source=NULL;
      else if(librdf_heuristic_is_blank_node(argv[0]))
        source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
      else
        source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      
      if(!strcmp(argv[1], "-"))
        arc=NULL;
      else
        arc=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);

      if(type == CMD_ADD_TYPED) {
        char *lang=(strcmp(argv[3], "-")) ? argv[3] : NULL;
        librdf_uri* dt_uri=librdf_new_uri(world, (const unsigned char *)argv[4]);
        target=librdf_new_node_from_typed_literal(world, (const unsigned char *)argv[2], lang, dt_uri);
        librdf_free_uri(dt_uri);
      } else {
        if(!strcmp(argv[2], "-"))
          target=NULL;
        else {
          if(!strncmp(argv[2], "_:", 2))
            target=librdf_new_node_from_blank_identifier(world, (const unsigned char *)argv[2]+2);
          else if(librdf_heuristic_object_is_literal(argv[2]))
            target=librdf_new_node_from_literal(world, (const unsigned char *)argv[2], NULL, 0);
          else
            target=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[2]);
        }
      }
      
      
      partial_statement=librdf_new_statement(world);
      librdf_statement_set_subject(partial_statement, source);
      librdf_statement_set_predicate(partial_statement, arc);
      librdf_statement_set_object(partial_statement, target);
      
      if((type != CMD_FIND) && (type != CMD_MATCH)) {
        if(!source || !arc || !target) {
          fprintf(stderr, "%s: cannot have missing triple parts for %s\n", program, cmd);
          librdf_free_statement(partial_statement);
        break;
        }
      }

    printmatching:
      switch(type) {
        case CMD_CONTAINS:
          if(librdf_model_contains_statement(model, partial_statement))
            fprintf(stdout, "%s: the graph contains the triple\n", program);
        else
          fprintf(stdout, "%s: the graph does not contain the triple\n", program);
          break;
          
        case CMD_FIND:
        case CMD_MATCH:
        case CMD_QUERY:
          /* Print out matching statements */
          if(type==CMD_FIND || type==CMD_MATCH) {
            if(argc == 4 || type==CMD_MATCH) {
              if(argc == 4) {
                if(librdf_heuristic_is_blank_node(argv[3]))
                  context_node=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[3]));
                else
                  context_node=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[3]);
              }
              if(type == CMD_MATCH) {
                librdf_hash* match_options=librdf_new_hash(world, NULL);
                librdf_hash_open(match_options, NULL, 0, 1, 1, NULL);
                librdf_hash_put_strings(match_options, "match-substring", "yes");
                stream=librdf_model_find_statements_with_options(model, partial_statement, context_node, match_options);
                librdf_free_hash(match_options);
              } else {
                stream=librdf_model_find_statements_in_context(model, partial_statement, context_node);
              }

              if(context_node)
                librdf_free_node(context_node);
            } else
              stream=librdf_model_find_statements(model, partial_statement);
          } else {
            if(!(results=librdf_model_query_execute(model, query))) {
              fprintf(stderr, "%s: Query of model with '%s' failed\n", 
                      program, argv[2]);
              librdf_free_query(query);
              query=NULL;
              break;
            }
            stream=librdf_query_results_as_stream(results);
          }

          if(!stream) {
            fprintf(stderr, "%s: %s returned no results (NULL stream)\n", program, commands[type].name);
          } else {
            count=0;
            while(!librdf_stream_end(stream)) {
              librdf_statement *statement=librdf_stream_get_object(stream);
              context_node=(librdf_node*)librdf_stream_get_context(stream);
              if(!statement) {
                fprintf(stderr, "%s: librdf_stream_next returned NULL\n", program);
                break;
              }

              if(output_model) {
                librdf_model_add_statement(output_model,
                      librdf_new_statement_from_statement(statement));
              } else {
                fputs("Matched triple: ", stdout);
                librdf_statement_print(statement, stdout);
                if(context_node) {
                  fputs(" with context ", stdout);
                  librdf_node_print(context_node, stdout);
                }
                fputc('\n', stdout);
              }
              
              count++;
              librdf_stream_next(stream);
            }
            librdf_free_stream(stream);  

            if(!output_model)
              fprintf(stderr, "%s: matching triples: %d\n", program, count);

            if(results)
              librdf_free_query_results(results);
          }
          break;
          
        case CMD_ADD:
        case CMD_ADD_TYPED:
          if(argv[3]) {
            if(librdf_heuristic_is_blank_node(argv[3]))
              context_node=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[3]));
            else
              context_node=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[3]);

            rc=librdf_model_context_add_statement(model, context_node,
                                                  partial_statement);
            librdf_free_node(context_node);
          } else {
            if(argv[3])
              fprintf(stderr, "%s: ERROR: Cannot add in context - model does not support contexts\n", program);
            rc=librdf_model_add_statement(model, partial_statement);
          }
          if(rc)
            fprintf(stdout, "%s: failed to add triple to the graph\n", program);
          else
            fprintf(stdout, "%s: added triple to the graph\n", program);
          break;
          
        case CMD_REMOVE:
          if(argv[3]) {
            if(librdf_heuristic_is_blank_node(argv[3]))
              context_node=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[3]));
            else
              context_node=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[3]);

            rc=librdf_model_context_remove_statement(model, context_node,
                                                     partial_statement);
            librdf_free_node(context_node);
          } else
            rc=librdf_model_remove_statement(model, partial_statement);
          if(rc)
            fprintf(stdout, "%s: failed to remove triple from the graph\n", program);
          else
            fprintf(stdout, "%s: removed triple from the graph\n", program);
        break;
        
        default:
          fprintf(stderr, "Unexpected command %d\n", type);
          
      } /* end inner switch */
      
      /* also frees the nodes */
      librdf_free_statement(partial_statement);
      break;
      

    case CMD_SOURCES:
      arc=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      if(librdf_heuristic_is_blank_node(argv[1]))
        target=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[1]));
      else if(librdf_heuristic_object_is_literal(argv[1]))
        target=librdf_new_node_from_literal(world, (const unsigned char *)argv[1], NULL, 0);
      else
        target=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);
      
      iterator=librdf_model_get_sources(model, arc, target);
      if(!iterator) {
        fprintf(stderr, "%s: Failed to get sources\n", program);
        break;
      }
      
      print_nodes(stdout, iterator);

      librdf_free_node(arc);
      librdf_free_node(target);
      break;
      

    case CMD_ARCS:
      if(!iterator) {
        if(librdf_heuristic_is_blank_node(argv[0]))
          source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
        else
          source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
        if(librdf_heuristic_is_blank_node(argv[1]))
          target=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[1]));
        else if(librdf_heuristic_object_is_literal(argv[1]))
          target=librdf_new_node_from_literal(world, (const unsigned char *)argv[1], NULL, 0);
        else
          target=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);
        iterator=librdf_model_get_arcs(model, source, target);
        if(!iterator) {
          fprintf(stderr, "Failed to get arcs\n");
          break;
        }
      }

      print_nodes(stdout, iterator);

      librdf_free_node(source);
      librdf_free_node(target);
      break;
      

    case CMD_TARGETS:
      if(!iterator) {
        if(librdf_heuristic_is_blank_node(argv[0]))
          source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
        else
          source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
        arc=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);
        iterator=librdf_model_get_targets(model, source, arc);
        if(!iterator) {
          fprintf(stderr, "%s: Failed to get targets\n", program);
          break;
        }
      }

      print_nodes(stdout, iterator);

      librdf_free_node(source);
      librdf_free_node(arc);
      break;
      

    case CMD_SOURCE:
      arc=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      if(librdf_heuristic_is_blank_node(argv[1]))
        target=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[1]));
      else if(librdf_heuristic_object_is_literal(argv[1]))
        target=librdf_new_node_from_literal(world, (const unsigned char *)argv[1], NULL, 0);
      else
        target=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);

      node=librdf_model_get_source(model, arc, target);
      if(!node) {
        fprintf(stderr, "%s: Failed to get source\n", program);
        librdf_free_node(arc);
        librdf_free_node(target);
        break;
      }
      
      print_node(stdout, node);
      librdf_free_node(arc);
      librdf_free_node(target);
      break;


    case CMD_ARC:
      if(!node) {
        if(librdf_heuristic_is_blank_node(argv[0]))
          source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
        else
          source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      if(librdf_heuristic_is_blank_node(argv[1]))
        target=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[1]));
      else if(librdf_heuristic_object_is_literal(argv[1]))
        target=librdf_new_node_from_literal(world, (const unsigned char *)argv[1], NULL, 0);
      else
        target=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);
        node=librdf_model_get_arc(model, source, target);
        if(!node) {
          fprintf(stderr, "Failed to get arc\n");
          librdf_free_node(source);
          librdf_free_node(target);
          break;
        }
      }
      
      print_node(stdout, node);
      librdf_free_node(source);
      librdf_free_node(target);
      break;


    case CMD_TARGET:
      if(!node) {
        if(librdf_heuristic_is_blank_node(argv[0]))
          source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
        else
          source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
        arc=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);
        node=librdf_model_get_target(model, source, arc);
        if(!node) {
          fprintf(stderr, "%s: Failed to get target\n", program);
          librdf_free_node(source);
          librdf_free_node(arc);
          break;
        }
      }
      
      print_node(stdout, node);
      librdf_free_node(source);
      librdf_free_node(arc);
      break;
      

    case CMD_ARCS_IN:
    case CMD_ARCS_OUT:
      if(librdf_heuristic_is_blank_node(argv[0]))
        source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
      else
        source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      iterator=(type == CMD_ARCS_IN) ? librdf_model_get_arcs_in(model, source) :
                                       librdf_model_get_arcs_out(model, source);
      if(!iterator) {
        fprintf(stderr, "%s: Failed to get arcs in/out\n", program);
        librdf_free_node(source);
        break;
      }

      count=0;
      while(!librdf_iterator_end(iterator)) {
        context_node=(librdf_node*)librdf_iterator_get_context(iterator);
        node=(librdf_node*)librdf_iterator_get_object(iterator);
        if(!node) {
          fprintf(stderr, "%s: librdf_iterator_get_next returned NULL\n",
                  program);
          break;
        }
        
        fputs("Matched arc: ", stdout);
        librdf_node_print(node, stdout);
        if(context_node) {
          fputs(" with context ", stdout);
          librdf_node_print(context_node, stdout);
        }
        fputc('\n', stdout);
        
        librdf_free_node(node);
        count++;
        librdf_iterator_next(iterator);
      }
      librdf_free_iterator(iterator);
      fprintf(stderr, "%s: matching arcs: %d\n", program, count);
      
      librdf_free_node(source);
      break;

      
    case CMD_HAS_ARC_IN:
    case CMD_HAS_ARC_OUT:
      if(librdf_heuristic_is_blank_node(argv[0]))
        source=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
      else
        source=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      arc=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[1]);
      result=(type == CMD_HAS_ARC_IN) ? librdf_model_has_arc_in(model, arc, source) :
                                        librdf_model_has_arc_out(model, source, arc);
      if(result)
        fprintf(stdout, "%s: the graph contains the arc\n", program);
      else
        fprintf(stdout, "%s: the graph does not contain the arc\n", program);
      
      librdf_free_node(source);
      librdf_free_node(arc);
      break;


    case CMD_REMOVE_CONTEXT:
      if(librdf_heuristic_is_blank_node(argv[0]))
        context_node=librdf_new_node_from_blank_identifier(world, (const unsigned char *)librdf_heuristic_get_blank_node(argv[0]));
      else
        context_node=librdf_new_node_from_uri_string(world, (const unsigned char *)argv[0]);
      
      rc=librdf_model_context_remove_statements(model, context_node);
      librdf_free_node(context_node);
      
      if(rc)
        fprintf(stdout, "%s: failed to remove context triples from the graph\n", program);
      else
        fprintf(stdout, "%s: removed context triples from the graph\n", program);
      break;


    case CMD_CONTEXTS:
      iterator=librdf_model_get_contexts(model);
      if(!iterator) {
        fprintf(stderr, "%s: Failed to get contexts\n", program);
        break;
      }
      
      count=0;
      while(!librdf_iterator_end(iterator)) {
        node=(librdf_node*)librdf_iterator_get_object(iterator);
        if(!node) {
          fprintf(stderr, "%s: librdf_iterator_get_next returned NULL\n",
                  program);
          break;
        }
        
        fputs("Context: ", stdout);
        librdf_node_print(node, stdout);
        fputc('\n', stdout);
        
        librdf_free_node(node);
        count++;
        librdf_iterator_next(iterator);
      }
      librdf_free_iterator(iterator);
      fprintf(stderr, "%s: contexts: %d\n", program, count);
      break;


    case CMD_SIZE:
      count=librdf_model_size(model);
      if(count >= 0)
        fprintf(stderr, "%s: graph has %d triples\n", program, count);
      else
        fprintf(stderr, "%s: graph has unknown number of triples\n", program);
      break;

    default:
      fprintf(stderr, "%s: Unknown command %d\n", program, type);
      return(1);
  }

  if(query)
    librdf_free_query(query);

  if(free_uri_string)
    free(uri_string);

  if(output_model) {
    if(librdf_serializer_serialize_model_to_file_handle(output_serializer, 
                                                        stdout, NULL,
                                                        output_model)) {
      fprintf(stderr, "%s: Failed to serialize output model\n", program);
    };
    librdf_free_serializer(output_serializer);
    librdf_free_model(output_model);
    librdf_free_storage(output_storage);
  }

  
  librdf_free_hash(options);

  if(transactions)
    librdf_model_transaction_commit(model);

  librdf_free_model(model);
  librdf_free_storage(storage);

  librdf_free_world(world);

#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif
	
  /* keep gcc -Wall happy */
  return(0);
}
