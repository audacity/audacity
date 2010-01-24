/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * roqet.c - Rasqal RDF Query test program
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
/* for access() and R_OK */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif
#ifndef HAVE_GETOPT
#include <rasqal_getopt.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Rasqal includes */
#include <rasqal.h>
#ifdef RASQAL_DEBUG
/* for internal rasqal_query_set_store_results() */
#include <rasqal_internal.h>
#endif


#ifdef NEED_OPTIND_DECLARATION
extern int optind;
extern char *optarg;
#endif

int main(int argc, char *argv[]);


static char *program=NULL;


#ifdef HAVE_GETOPT_LONG
#define HELP_TEXT(short, long, description) "  -" short ", --" long "  " description
#define HELP_TEXT_LONG(long, description) "      --" long "  " description
#define HELP_ARG(short, long) "--" #long
#define HELP_PAD "\n                            "
#else
#define HELP_TEXT(short, long, description) "  -" short "  " description
#define HELP_TEXT_LONG(long, description)
#define HELP_ARG(short, long) "-" #short
#define HELP_PAD "\n      "
#endif

#define GETOPT_STRING "cd:D:e:f:G:hi:nr:qs:vw"

#ifdef HAVE_GETOPT_LONG

#ifdef RASQAL_DEBUG
#define STORE_RESULTS_FLAG 0x100
#endif

static struct option long_options[] =
{
  /* name, has_arg, flag, val */
  {"count", 0, 0, 'c'},
  {"dump-query", 1, 0, 'd'},
  {"dryrun", 0, 0, 'n'},
  {"exec", 1, 0, 'e'},
  {"feature", 1, 0, 'f'},
  {"help", 0, 0, 'h'},
  {"input", 1, 0, 'i'},
  {"quiet", 0, 0, 'q'},
  {"results", 1, 0, 'r'},
  {"source", 1, 0, 's'},
  {"data", 1, 0, 'D'},
  {"named", 1, 0, 'G'},
  {"version", 0, 0, 'v'},
  {"walk-query", 0, 0, 'w'},
#ifdef STORE_RESULTS_FLAG
  {"store-results", 1, 0, STORE_RESULTS_FLAG},
#endif
  {NULL, 0, 0, 0}
};
#endif


static int error_count=0;

static const char *title_format_string="Rasqal RDF query utility %s\n";

#ifdef BUFSIZ
#define FILE_READ_BUF_SIZE BUFSIZ
#else
#define FILE_READ_BUF_SIZE 1024
#endif

#define MAX_QUERY_ERROR_REPORT_LEN 512


static void
roqet_error_handler(void *user_data, 
                    raptor_locator* locator, const char *message) 
{
  fprintf(stderr, "%s: Error - ", program);
  raptor_print_locator(stderr, locator);
  fprintf(stderr, " - %s\n", message);

  error_count++;
}

#define SPACES_LENGTH 80
static const char spaces[SPACES_LENGTH+1]="                                                                                ";

static void
roqet_write_indent(FILE *fh, int indent) 
{
  while(indent > 0) {
    int sp=(indent > SPACES_LENGTH) ? SPACES_LENGTH : indent;
    (void)fwrite(spaces, sizeof(char), sp, fh);
    indent -= sp;
  }
}

  

static void
roqet_graph_pattern_walk(rasqal_graph_pattern *gp, int gp_index,
                         FILE *fh, int indent) {
  int triple_index=0;
  rasqal_graph_pattern_operator op;
  int seen;
  raptor_sequence *seq;
  int idx;
  
  op=rasqal_graph_pattern_get_operator(gp);
  
  roqet_write_indent(fh, indent);
  fprintf(fh, "%s graph pattern", 
          rasqal_graph_pattern_operator_as_string(op));
  idx=rasqal_graph_pattern_get_index(gp);
  if(idx >= 0)
    fprintf(fh, "[%d]", idx);
  if(gp_index >= 0)
    fprintf(fh, " #%d", gp_index);
  fputs(" {\n", fh);
  
  indent+= 2;

  /* look for triples */
  seen=0;
  while(1) {
    rasqal_triple* t=rasqal_graph_pattern_get_triple(gp, triple_index);
    if(!t)
      break;
    
    if(!seen) {
      roqet_write_indent(fh, indent);
      fputs("triples {\n", fh);
      seen=1;
    }
    roqet_write_indent(fh, indent+2);
    fprintf(fh, "triple #%d { ", triple_index);
    rasqal_triple_print(t, fh);
    fputs(" }\n", fh);

    triple_index++;
  }
  if(seen) {
    roqet_write_indent(fh, indent);
    fputs("}\n", fh);
  }


  /* look for sub-graph patterns */
  seq=rasqal_graph_pattern_get_sub_graph_pattern_sequence(gp);
  if(seq && raptor_sequence_size(seq) > 0) {
    roqet_write_indent(fh, indent);
    fprintf(fh, "sub-graph patterns (%d) {\n", raptor_sequence_size(seq));

    gp_index=0;
    while(1) {
      rasqal_graph_pattern* sgp;
      sgp=rasqal_graph_pattern_get_sub_graph_pattern(gp, gp_index);
      if(!sgp)
        break;
      
      roqet_graph_pattern_walk(sgp, gp_index, fh, indent+2);
      gp_index++;
    }

    roqet_write_indent(fh, indent);
    fputs("}\n", fh);
  }
  

  /* look for constraints */
  seq=rasqal_graph_pattern_get_constraint_sequence(gp);
  if(seq && raptor_sequence_size(seq) > 0) {
    roqet_write_indent(fh, indent);
    fprintf(fh, "constraints (%d) {\n", raptor_sequence_size(seq));

    gp_index=0;
    while(1) {
      rasqal_expression* expr=rasqal_graph_pattern_get_constraint(gp, gp_index);
      if(!expr)
        break;
      
      roqet_write_indent(fh, indent+2);
      fprintf(fh, "constraint #%d { ", gp_index);
      rasqal_expression_print(expr, fh);
      fputs("}\n", fh);
      
      gp_index++;
    }

    roqet_write_indent(fh, indent);
    fputs("}\n", fh);
  }
  

  indent-=2;
  
  roqet_write_indent(fh, indent);
  fputs("}\n", fh);
}


    
static void
roqet_query_write_variable(FILE* fh, rasqal_variable* v)
{
  fputs((const char*)v->name, fh);
  if(v->expression) {
    fputc('=', fh);
    rasqal_expression_print(v->expression, fh);
  }
}


static void
roqet_query_walk(rasqal_query *rq, FILE *fh, int indent) {
  rasqal_query_verb verb;
  int i;
  rasqal_graph_pattern* gp;
  raptor_sequence *seq;

  verb=rasqal_query_get_verb(rq);
  roqet_write_indent(fh, indent);
  fprintf(fh, "query verb: %s\n", rasqal_query_verb_as_string(verb));

  i=rasqal_query_get_distinct(rq);
  if(i != 0) {
    roqet_write_indent(fh, indent);
    fprintf(fh, "query asks for distinct results\n");
  }
  
  i=rasqal_query_get_limit(rq);
  if(i >= 0) {
    roqet_write_indent(fh, indent);
    fprintf(fh, "query asks for result limits %d\n", i);
  }
  
  i=rasqal_query_get_offset(rq);
  if(i >= 0) {
    roqet_write_indent(fh, indent);
    fprintf(fh, "query asks for result offset %d\n", i);
  }
  
  seq=rasqal_query_get_bound_variable_sequence(rq);
  if(seq && raptor_sequence_size(seq) > 0) {
    fprintf(fh, "query bound variables (%d): ", 
            raptor_sequence_size(seq));
    i=0;
    while(1) {
      rasqal_variable* v=(rasqal_variable*)raptor_sequence_get_at(seq, i);
      if(!v)
        break;

      if(i > 0)
        fputs(", ", fh);
      roqet_query_write_variable(fh, v);
      i++;
    }
    fputc('\n', fh);
  }

  gp=rasqal_query_get_query_graph_pattern(rq);
  if(!gp)
    return;


  seq=rasqal_query_get_construct_triples_sequence(rq);
  if(seq && raptor_sequence_size(seq) > 0) {
    roqet_write_indent(fh, indent);
    fprintf(fh, "query construct triples (%d) {\n", 
            raptor_sequence_size(seq));
    i=0;
    while(1) {
      rasqal_triple* t=rasqal_query_get_construct_triple(rq, i);
      if(!t)
        break;
    
      roqet_write_indent(fh, indent+2);
      fprintf(fh, "triple #%d { ", i);
      rasqal_triple_print(t, fh);
      fputs(" }\n", fh);

      i++;
    }
    roqet_write_indent(fh, indent);
    fputs("}\n", fh);
  }

  fputs("query ", fh);
  roqet_graph_pattern_walk(gp, -1, fh, indent);
}


typedef enum {
  QUERY_OUTPUT_UNKNOWN,
  QUERY_OUTPUT_DEBUG,
  QUERY_OUTPUT_STRUCTURE,
  QUERY_OUTPUT_SPARQL,
  QUERY_OUTPUT_LAST= QUERY_OUTPUT_SPARQL
} query_output_format;

const char* query_output_format_labels[QUERY_OUTPUT_LAST+1][2]={
  { NULL, NULL },
  { "debug", "Debug query dump (output format may change)" },
  { "structure", "Query structure walk (output format may change)" },
  { "sparql", "SPARQL" }
};



int
main(int argc, char *argv[]) 
{ 
  int query_from_string=0;
  void *query_string=NULL;
  unsigned char *uri_string=NULL;
  int free_uri_string=0;
  unsigned char *base_uri_string=NULL;
  rasqal_query *rq;
  rasqal_query_results *results;
  const char *ql_name="sparql";
  char *ql_uri=NULL;
  int rc=0;
  raptor_uri *uri=NULL;
  raptor_uri *base_uri=NULL;
  char *filename=NULL;
  char *p;
  int usage=0;
  int help=0;
  int quiet=0;
  int count=0;
  int dryrun=0;
  raptor_sequence* data_source_uris=NULL;
  raptor_sequence* named_source_uris=NULL;
  raptor_serializer* serializer=NULL;
  const char *serializer_syntax_name="ntriples";
  query_output_format output_format= QUERY_OUTPUT_UNKNOWN;
  rasqal_query_results_formatter* results_formatter=NULL;
  rasqal_feature query_feature=(rasqal_feature)-1;
  int query_feature_value= -1;
  unsigned char* query_feature_string_value=NULL;
  rasqal_world *world;
#ifdef STORE_RESULTS_FLAG
  int store_results= -1;
#endif
  
  program=argv[0];
  if((p=strrchr(program, '/')))
    program=p+1;
  else if((p=strrchr(program, '\\')))
    program=p+1;
  argv[0]=program;

  world=rasqal_new_world();
  if(!world) {
    fprintf(stderr, "%s: rasqal_new_world() failed\n", program);
    return(1);
  }
  
#ifdef STORE_RESULTS_FLAG
  /* This is for debugging only */
  if(1) {
    char* sr=getenv("RASQAL_DEBUG_STORE_RESULTS");
    if(sr)
      store_results=atoi(sr);
  }
#endif

  while (!usage && !help)
  {
    int c;
    raptor_uri *source_uri;
    raptor_sequence** seq_p=NULL;
    
#ifdef HAVE_GETOPT_LONG
    int option_index = 0;

    c = getopt_long (argc, argv, GETOPT_STRING, long_options, &option_index);
#else
    c = getopt (argc, argv, GETOPT_STRING);
#endif
    if (c == -1)
      break;

    switch (c) {
      case 0:
      case '?': /* getopt() - unknown option */
        usage=1;
        break;
        
      case 'c':
        count=1;
        break;

      case 'd':
        output_format= QUERY_OUTPUT_UNKNOWN;
        if(optarg) {
          int i;

          for(i=1; i <= QUERY_OUTPUT_LAST; i++)
            if(!strcmp(optarg, query_output_format_labels[i][0])) {
              output_format=(query_output_format)i;
              break;
            }
            
        }
        if(output_format == QUERY_OUTPUT_UNKNOWN) {
          int i;
          fprintf(stderr,
                  "%s: invalid argument `%s' for `" HELP_ARG(d, dump-query) "'\n",
                  program, optarg);
          for(i=1; i <= QUERY_OUTPUT_LAST; i++)
            fprintf(stderr, 
                    "  %-12s for %s\n", query_output_format_labels[i][0],
                   query_output_format_labels[i][1]);
          usage=1;
        }
        break;
        

      case 'e':
	if(optarg) {
          query_string=optarg;
          query_from_string=1;
        }
        break;

      case 'f':
        if(optarg) {
          if(!strcmp(optarg, "help")) {
            int i;
            
            fprintf(stderr, "%s: Valid query features are:\n", program);
            for(i=0; i < (int)rasqal_get_feature_count(); i++) {
              const char *feature_name;
              const char *feature_label;
              if(!rasqal_features_enumerate((rasqal_feature)i, &feature_name, NULL, &feature_label)) {
                const char *feature_type=(rasqal_feature_value_type((rasqal_feature)i) == 0) ? "" : " (string)";
                fprintf(stderr, "  %-20s  %s%s\n", feature_name, feature_label, 
                       feature_type);
              }
            }
            fputs("Features are set with `" HELP_ARG(f, feature) " FEATURE=VALUE or `-f FEATURE'\nand take a decimal integer VALUE except where noted, defaulting to 1 if omitted.\n", stderr);

            rasqal_free_world(world);
            exit(0);
          } else {
            int i;
            size_t arg_len=strlen(optarg);
            
            for(i=0; i < (int)rasqal_get_feature_count(); i++) {
              const char *feature_name;
              size_t len;
              
              if(rasqal_features_enumerate((rasqal_feature)i, &feature_name, NULL, NULL))
                continue;
              len=strlen(feature_name);
              if(!strncmp(optarg, feature_name, len)) {
                query_feature=(rasqal_feature)i;
                if(rasqal_feature_value_type(query_feature) == 0) {
                  if(len < arg_len && optarg[len] == '=')
                    query_feature_value=atoi(&optarg[len+1]);
                  else if(len == arg_len)
                    query_feature_value=1;
                } else {
                  if(len < arg_len && optarg[len] == '=')
                    query_feature_string_value=(unsigned char*)&optarg[len+1];
                  else if(len == arg_len)
                    query_feature_string_value=(unsigned char*)"";
                }
                break;
              }
            }
            
            if(query_feature_value < 0 && !query_feature_string_value) {
              fprintf(stderr, "%s: invalid argument `%s' for `" HELP_ARG(f, feature) "'\nTry '%s " HELP_ARG(f, feature) " help' for a list of valid features\n",
                      program, optarg, program);
              usage=1;
            }
          }
        }
        break;

      case 'h':
        help=1;
        break;

      case 'n':
        dryrun=1;
        break;

      case 'r':
        if(optarg) {
          if(results_formatter)
            rasqal_free_query_results_formatter(results_formatter);

          if(!strcmp(optarg, "simple"))
            results_formatter=NULL;
          else {
            results_formatter=rasqal_new_query_results_formatter(world, optarg, NULL);
            if(!results_formatter) {
              if(raptor_serializer_syntax_name_check(optarg))
                serializer_syntax_name=optarg;
              else {
                fprintf(stderr, 
                        "%s: invalid argument `%s' for `" HELP_ARG(r, results) "'\n",
                        program, optarg);
                usage=1;
                break;
              }
            }
          }
        }
        break;

      case 'i':
	if(rasqal_language_name_check(world, optarg))
          ql_name=optarg;
	else {
          int i;
          fprintf(stderr,
                  "%s: invalid argument `%s' for `" HELP_ARG(i, input) "'\n",
                  program, optarg);
          fprintf(stderr, "Valid arguments are:\n");
          for(i=0; 1; i++) {
            const char *help_name;
            const char *help_label;
            if(rasqal_languages_enumerate(world, i, &help_name, &help_label, NULL))
              break;
            fprintf(stderr, "  %-12s for %s\n", help_name, help_label);
          }
          usage=1;
	}
        break;

      case 'q':
        quiet=1;
        break;

      case 's':
      case 'D':
      case 'G':
        if(!access((const char*)optarg, R_OK)) {
          unsigned char* source_uri_string=raptor_uri_filename_to_uri_string((const char*)optarg);
          source_uri=raptor_new_uri(source_uri_string);
          raptor_free_memory(source_uri_string);
        } else
          source_uri=raptor_new_uri((const unsigned char*)optarg);

        if(!source_uri) {
          fprintf(stderr, "%s: Failed to create source URI for %s\n",
                  program, optarg);
          return(1);
        }

        seq_p= (c=='D') ? &data_source_uris : &named_source_uris;
        if(!*seq_p) {
          *seq_p=raptor_new_sequence((raptor_sequence_free_handler*)raptor_free_uri,
                                     (raptor_sequence_print_handler*)raptor_sequence_print_uri);
          if(!*seq_p) {
            fprintf(stderr, "%s: Failed to create source sequence\n", program);
            return(1);
          }
        }

        raptor_sequence_push(*seq_p, source_uri);
        break;

      case 'v':
        fputs(rasqal_version_string, stdout);
        fputc('\n', stdout);
        rasqal_free_world(world);
        exit(0);

      case 'w':
        fprintf(stderr,
                "%s: WARNING: `-w' is deprecated.  Please use `" HELP_ARG(d, dump-query) " structure' instead.\n",
                program);
        output_format=QUERY_OUTPUT_STRUCTURE;
        break;

#ifdef STORE_RESULTS_FLAG
      case STORE_RESULTS_FLAG:
        store_results=(!strcmp(optarg, "yes") || !strcmp(optarg, "YES"));
        break;
#endif

    }
    
  }
  
  if(!help && !usage) {
    if(query_string) {
      if(optind != argc && optind != argc-1)
        usage=2; /* Title and usage */
    } else {
      if(optind != argc-1 && optind != argc-2)
        usage=2; /* Title and usage */
    }
  }

  
  if(usage) {
    if(usage>1) {
      fprintf(stderr, title_format_string, rasqal_version_string);
      fputs("Rasqal home page: ", stderr);
      fputs(rasqal_home_url_string, stderr);
      fputc('\n', stderr);
      fputs(rasqal_copyright_string, stderr);
      fputs("\nLicense: ", stderr);
      fputs(rasqal_license_string, stderr);
      fputs("\n\n", stderr);
    }
    fprintf(stderr, "Try `%s " HELP_ARG(h, help) "' for more information.\n",
                    program);
    rasqal_free_world(world);

    exit(1);
  }

  if(help) {
    int i;
    
    printf(title_format_string, rasqal_version_string);
    puts("Run an RDF query giving variable bindings or RDF triples.");
    printf("Usage: %s [OPTIONS] <query URI> [base URI]\n", program);
    printf("       %s [OPTIONS] -e <query string> [base URI]\n\n", program);

    fputs(rasqal_copyright_string, stdout);
    fputs("\nLicense: ", stdout);
    puts(rasqal_license_string);
    fputs("Rasqal home page: ", stdout);
    puts(rasqal_home_url_string);

    puts("\nNormal operation is to execute the query retrieved from URI <query URI>");
    puts("and print the results in a simple text format.");
    puts("\nMain options:");
    puts(HELP_TEXT("e", "exec QUERY      ", "Execute QUERY string instead of <query URI>"));
    puts(HELP_TEXT("i", "input LANGUAGE  ", "Set query language name to one of:"));
    for(i=0; 1; i++) {
      const char *help_name;
      const char *help_label;
      if(rasqal_languages_enumerate(world, i, &help_name, &help_label, NULL))
        break;
      printf("    %-15s         %s", help_name, help_label);
      if(!i)
        puts(" (default)");
      else
        putchar('\n');
    }
    puts(HELP_TEXT("r", "results FORMAT  ", "Set query results output format to one of:"));
    puts("    For variable bindings and boolean results:");
    puts("      simple                A simple text format (default)");
    for(i=0; i < (int)raptor_get_feature_count(); i++) {
      const char *name;
      const char *label;
      int qr_flags=0;
      if(!rasqal_query_results_formats_enumerate(world, i, &name, &label, 
                                                 NULL, NULL, &qr_flags) &&
         (qr_flags & RASQAL_QUERY_RESULTS_FORMAT_FLAG_WRITER))
        printf("      %-10s            %s\n", name, label);
    }
    puts("    For RDF graph results:");
    for(i=0; 1; i++) {
      const char *help_name;
      const char *help_label;
      if(raptor_serializers_enumerate(i, &help_name, &help_label, NULL, NULL))
        break;
      printf("      %-15s       %s", help_name, help_label);
      if(!i)
        puts(" (default)");
      else
        putchar('\n');
    }
    puts("\nAdditional options:");
    puts(HELP_TEXT("c", "count             ", "Count triples - no output"));
    puts(HELP_TEXT("d", "dump-query FORMAT ", "Print the parsed query out in FORMAT:"));
    for(i=1; i <= QUERY_OUTPUT_LAST; i++)
      printf("      %-15s         %s\n", query_output_format_labels[i][0],
             query_output_format_labels[i][1]);
    puts(HELP_TEXT("f FEATURE(=VALUE)", "feature FEATURE(=VALUE)", HELP_PAD "Set query features" HELP_PAD "Use `-f help' for a list of valid features"));
    puts(HELP_TEXT("h", "help              ", "Print this help, then exit"));
    puts(HELP_TEXT("n", "dryrun            ", "Prepare but do not run the query"));
    puts(HELP_TEXT("q", "quiet             ", "No extra information messages"));
    puts(HELP_TEXT("D", "data URI          ", "RDF data source URI"));
    puts(HELP_TEXT("G", "named URI         ", "RDF named graph data source URI"));
    puts(HELP_TEXT("s", "source URI        ", "Same as `-G URI'"));
    puts(HELP_TEXT("v", "version           ", "Print the Rasqal version"));
    puts(HELP_TEXT("w", "walk-query        ", "Print query.  Same as '-d structure'"));
#ifdef STORE_RESULTS_FLAG
    puts(HELP_TEXT_LONG("store-results BOOL", "DEBUG: Set store results yes/no BOOL"));
#endif
    puts("\nReport bugs to http://bugs.librdf.org/");

    rasqal_free_world(world);
    
    exit(0);
  }


  if(query_string) {
    if(optind == argc-1)
      base_uri_string=(unsigned char*)argv[optind];
  } else {
    if(optind == argc-1)
      uri_string=(unsigned char*)argv[optind];
    else {
      uri_string=(unsigned char*)argv[optind++];
      base_uri_string=(unsigned char*)argv[optind];
    }
    
    /* If uri_string is "path-to-file", turn it into a file: URI */
    if(!strcmp((const char*)uri_string, "-")) {
      if(!base_uri_string) {
        fprintf(stderr, "%s: A Base URI is required when reading from standard input.\n",
                program);
        return(1);
      }
      uri_string=NULL;
    } else if(!access((const char*)uri_string, R_OK)) {
      filename=(char*)uri_string;
      uri_string=raptor_uri_filename_to_uri_string(filename);
      free_uri_string=1;
    }
    
    if(uri_string) {
      uri=raptor_new_uri(uri_string);
      if(!uri) {
        fprintf(stderr, "%s: Failed to create URI for %s\n",
                program, uri_string);
        return(1);
      }
    } else
      uri=NULL; /* stdin */
  }
  

  if(!base_uri_string) {
    if(uri)
      base_uri=raptor_uri_copy(uri);
  } else {
    base_uri=raptor_new_uri(base_uri_string);
    if(!base_uri) {
      fprintf(stderr, "%s: Failed to create URI for %s\n",
              program, base_uri_string);
      return(1);
    }
  }

  if(query_string) {
    /* NOP - already got it */
  } else if(!uri_string) {
    query_string=calloc(FILE_READ_BUF_SIZE, 1);
    rc=fread(query_string, FILE_READ_BUF_SIZE, 1, stdin);
    if(ferror(stdin)) {
      fprintf(stderr, "%s: query string stdin read failed - %s\n",
              program, strerror(errno));
      return(1);
    }
    query_from_string=0;
  } else if(filename) {
    raptor_stringbuffer *sb=raptor_new_stringbuffer();
    size_t len;
    FILE *fh;

    fh=fopen(filename, "r");
    if(!fh) {
      fprintf(stderr, "%s: file '%s' open failed - %s", 
              program, filename, strerror(errno));
      rc=1;
      goto tidy_setup;
    }
    
    while(!feof(fh)) {
      unsigned char buffer[FILE_READ_BUF_SIZE];
      size_t read_len;
      read_len=fread((char*)buffer, 1, FILE_READ_BUF_SIZE, fh);
      if(read_len > 0)
        raptor_stringbuffer_append_counted_string(sb, buffer, read_len, 1);
      if(read_len < FILE_READ_BUF_SIZE) {
        if(ferror(fh)) {
          fprintf(stderr, "%s: file '%s' read failed - %s\n",
                  program, filename, strerror(errno));
          fclose(fh);
          return(1);
        }
        break;
      }
    }
    fclose(fh);

    len=raptor_stringbuffer_length(sb);
    query_string=malloc(len+1);
    raptor_stringbuffer_copy_to_string(sb, (unsigned char*)query_string, len);
    raptor_free_stringbuffer(sb);
    query_from_string=0;
  } else {
    raptor_www *www=raptor_www_new();
    if(www) {
      raptor_www_set_error_handler(www, roqet_error_handler, NULL);
      raptor_www_fetch_to_string(www, uri, &query_string, NULL, malloc);
      raptor_www_free(www);
    }
    if(!query_string || error_count) {
      fprintf(stderr, "%s: Retrieving query at URI '%s' failed\n", 
              program, uri_string);
      rc=1;
      goto tidy_setup;
    }
    query_from_string=0;
  }


  if(!quiet) {
    if(query_from_string) {
      if(base_uri_string)
        fprintf(stderr, "%s: Running query '%s' with base URI %s\n", program,
                (char*)query_string, base_uri_string);
      else
        fprintf(stderr, "%s: Running query '%s'\n", program,
                (char*)query_string);
    } else if(filename) {
      if(base_uri_string)
        fprintf(stderr, "%s: Querying from file %s with base URI %s\n", program,
                filename, base_uri_string);
      else
        fprintf(stderr, "%s: Querying from file %s\n", program, filename);
    } else if(uri_string) {
      if(base_uri_string)
        fprintf(stderr, "%s: Querying URI %s with base URI %s\n", program,
                uri_string, base_uri_string);
      else
        fprintf(stderr, "%s: Querying URI %s\n", program, uri_string);
    }
  }
  
  rq=rasqal_new_query(world, (const char*)ql_name, (const unsigned char*)ql_uri);
  rasqal_query_set_error_handler(rq, NULL, roqet_error_handler);
  rasqal_query_set_fatal_error_handler(rq, NULL, roqet_error_handler);

  if(query_feature_value >= 0)
    rasqal_query_set_feature(rq, query_feature, query_feature_value);
  if(query_feature_string_value)
    rasqal_query_set_feature_string(rq, query_feature,
                                    query_feature_string_value);

#ifdef STORE_RESULTS_FLAG
  if(store_results >= 0)
    rasqal_query_set_store_results(rq, store_results);
#endif
  
  if(named_source_uris) {
    while(raptor_sequence_size(named_source_uris)) {
      raptor_uri* source_uri=(raptor_uri*)raptor_sequence_pop(named_source_uris);
      if(rasqal_query_add_data_graph(rq, source_uri, source_uri, 
                                     RASQAL_DATA_GRAPH_NAMED)) {
        fprintf(stderr, "%s: Failed to add named graph %s\n", program, 
                raptor_uri_as_string(source_uri));
      }
      raptor_free_uri(source_uri);
    }
  }
  if(data_source_uris) {
    while(raptor_sequence_size(data_source_uris)) {
      raptor_uri* source_uri=(raptor_uri*)raptor_sequence_pop(data_source_uris);
      if(rasqal_query_add_data_graph(rq, source_uri, NULL,
                                     RASQAL_DATA_GRAPH_BACKGROUND)) {
        fprintf(stderr, "%s: Failed to add to default graph %s\n", program, 
                raptor_uri_as_string(source_uri));
      }
      raptor_free_uri(source_uri);
    }
  }

  if(rasqal_query_prepare(rq, (const unsigned char*)query_string, base_uri)) {
    size_t len=strlen((const char*)query_string);
    
    fprintf(stderr, "%s: Parsing query '", program);
    if(len > MAX_QUERY_ERROR_REPORT_LEN) {
      (void)fwrite(query_string, MAX_QUERY_ERROR_REPORT_LEN, sizeof(char), stderr);
      fprintf(stderr, "...' (%d bytes) failed\n", (int)len);
    } else {
      (void)fwrite(query_string, len, sizeof(char), stderr);
      fputs("' failed\n", stderr);
    }
    rc=1;
    goto tidy_query;
  }

  if(output_format != QUERY_OUTPUT_UNKNOWN) {
    if(!quiet)
      fprintf(stderr, "Query:\n");
    switch(output_format) {
    case QUERY_OUTPUT_DEBUG:
      rasqal_query_print(rq, stdout);
      break;
  
    case QUERY_OUTPUT_STRUCTURE:
      roqet_query_walk(rq, stdout, 0);
      break;
      
    case QUERY_OUTPUT_SPARQL:
      if(1) {
        raptor_iostream* iostr=raptor_new_iostream_to_file_handle(stdout);
        rasqal_query_write(iostr, rq, NULL, base_uri);
        raptor_free_iostream(iostr);
      }
      break;
      
    case QUERY_OUTPUT_UNKNOWN:
    default:
      fprintf(stderr, "%s: Unknown query output format %d\n", program,
              output_format);
      abort();
    }
  }

  if(dryrun)
    goto tidy_query;

  if(!(results=rasqal_query_execute(rq))) {
    fprintf(stderr, "%s: Query execution failed\n", program);
    rc=1;
    goto tidy_query;
  }

  if(results_formatter != NULL) {
    raptor_iostream *iostr;
  
    iostr=raptor_new_iostream_to_file_handle(stdout);
    if(!iostr) {
      fprintf(stderr, "%s: Creating output iostream failed\n", program);
      rc=1;
      goto tidy_query;
    }

    rasqal_query_results_formatter_write(iostr, results_formatter,
                                         results, base_uri);
    raptor_free_iostream(iostr);
  } else {
    if(rasqal_query_results_is_bindings(results)) {
      if(!quiet)
        fprintf(stderr, "%s: Query has a variable bindings result\n", program);

      while(!rasqal_query_results_finished(results)) {
        if(!count) {
          int i;
          
          fputs("result: [", stdout);
          for(i=0; i<rasqal_query_results_get_bindings_count(results); i++) {
            const unsigned char *name=rasqal_query_results_get_binding_name(results, i);
            rasqal_literal *value=rasqal_query_results_get_binding_value(results, i);
            
            if(i>0)
              fputs(", ", stdout);
            fprintf(stdout, "%s=", name);
            if(value)
              rasqal_literal_print(value, stdout);
            else
              fputs("NULL", stdout);
          }
          fputs("]\n", stdout);
        }
        
        rasqal_query_results_next(results);
      }

      if(!quiet)
        fprintf(stderr, "%s: Query returned %d results\n", program, 
                rasqal_query_results_get_count(results));

    } else if (rasqal_query_results_is_boolean(results)) {
      fprintf(stderr, "%s: Query has a boolean result: %s\n", program,
              rasqal_query_results_get_boolean(results) ? "true" : "false");
    }
    else if (rasqal_query_results_is_graph(results)) {
      int triple_count=0;
      rasqal_prefix* prefix;
      int i;
      
      if(!quiet)
        fprintf(stderr, "%s: Query has a graph result:\n", program);

      serializer=raptor_new_serializer(serializer_syntax_name);
      if(!serializer) {
        fprintf(stderr, "%s: Failed to create raptor serializer type %s\n",
                program, serializer_syntax_name);
        return(1);
      }

      /* Declare any query namespaces in the output serializer */
      for(i=0; (prefix=rasqal_query_get_prefix(rq, i)); i++)
        raptor_serialize_set_namespace(serializer, prefix->uri, prefix->prefix);

      raptor_serialize_start_to_file_handle(serializer, base_uri, stdout);

      while(1) {
        raptor_statement *rs=rasqal_query_results_get_triple(results);
        if(!rs)
          break;
        raptor_serialize_statement(serializer, rs);
        triple_count++;

        if(rasqal_query_results_next_triple(results))
          break;
      }

      raptor_serialize_end(serializer);
      raptor_free_serializer(serializer);

      if(!quiet)
        fprintf(stderr, "%s: Total %d triples\n", program, triple_count);
    } else {
      fprintf(stderr, "%s: Query returned unknown result format\n", program);
      rc=1;
    }
  }

  rasqal_free_query_results(results);
  
 tidy_query:  
  if(results_formatter)
    rasqal_free_query_results_formatter(results_formatter);

  rasqal_free_query(rq);

  if(!query_from_string)
    free(query_string);

 tidy_setup:

  if(data_source_uris)
    raptor_free_sequence(data_source_uris);
  if(named_source_uris)
    raptor_free_sequence(named_source_uris);
  if(base_uri)
    raptor_free_uri(base_uri);
  if(uri)
    raptor_free_uri(uri);
  if(free_uri_string)
    raptor_free_memory(uri_string);

  rasqal_free_world(world);
  
  return (rc);
}
