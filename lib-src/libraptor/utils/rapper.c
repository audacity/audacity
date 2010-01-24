/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rapper.c - Raptor RDF Parsing and Serializing utility
 *
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
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <string.h>

/* Raptor includes */
#include <raptor.h>

/* for access() and R_OK */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* many places for getopt */
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else
#include <raptor_getopt.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif




#ifdef NEED_OPTIND_DECLARATION
extern int optind;
extern char *optarg;
#endif

static void print_triples(void *user_data, const raptor_statement *statement);
static void print_graph(void *user_data, raptor_uri *graph);
static void print_namespaces(void* user_data, raptor_namespace *nspace);
static void relay_namespaces(void* user_data, raptor_namespace *nspace);
int main(int argc, char *argv[]);


static char *program=NULL;

/* replace newlines in literal string output with spaces */
static int replace_newlines=0;

/* extra noise? */
static int quiet=0;
/* just count, no printing */
static int count=0;

static int triple_count=0;

static raptor_serializer* serializer=NULL;

static int guess=0;

static int reported_guess=0;

static int report_namespace=0;

static int report_graph=0;

static
void print_triples(void *user_data, const raptor_statement *triple) 
{
  raptor_parser* rdf_parser=(raptor_parser*)user_data;
  triple_count++;

  if(guess && !quiet && !reported_guess) {
     fprintf(stderr, "%s: Guessed parser name '%s'\n", program,
             raptor_get_name(rdf_parser));
     reported_guess=1;
  }


  if(count)
    return;

  /* replace newlines with spaces if object is a literal string */
  if(replace_newlines && 
     triple->object_type == RAPTOR_IDENTIFIER_TYPE_LITERAL) {
    char *s;
    for(s=(char*)triple->object; *s; s++)
      if(*s == '\n')
        *s=' ';
  }

  raptor_serialize_statement(serializer, triple);
  return;
}

static
void print_graph(void *user_data, raptor_uri *graph)
{
  if(!report_graph)
    return;

  if(graph)
    fprintf(stderr, "%s: Named graph: URI %s\n", program,
            raptor_uri_as_string(graph));
  else
    fprintf(stderr, "%s: Named graph: default\n", program);
}

static void
print_namespaces(void* user_data, raptor_namespace *nspace)
{
  /* raptor_parser* rdf_parser=(raptor_parser*)user_data; */
  unsigned char *s;
  
  if(!report_namespace)
    return;

  s=raptor_namespaces_format(nspace, NULL);
  fprintf(stderr, "%s: Namespace declared: %s\n", program, s);
  raptor_free_memory(s);
  
  return;
}


static void
relay_namespaces(void* user_data, raptor_namespace *nspace)
{
  raptor_serializer* rdf_serializer=(raptor_serializer*)user_data;

  if(report_namespace)
    print_namespaces(user_data, nspace);

  raptor_serialize_set_namespace_from_namespace(rdf_serializer, nspace);
}



#ifdef HAVE_GETOPT_LONG
#define HELP_TEXT(short, long, description) "  -" short ", --" long "  " description
#define HELP_TEXT_LONG(long, description) "      --" long "  " description
#define HELP_ARG(short, long) "--" #long
#define HELP_ARG_BOTH(short, long) " -" #short ", --" #long
#define HELP_PAD "\n                          "
#else
#define HELP_TEXT(short, long, description) "  -" short "  " description
#define HELP_TEXT_LONG(long, description)
#define HELP_ARG(short, long) "-" #short
#define HELP_ARG_BOTH(short, long) "-" short
#define HELP_PAD "\n      "
#endif


#define GETOPT_STRING "nsaf:ghrqo:O:wecm:i:I:vt"

#ifdef HAVE_GETOPT_LONG
#define SHOW_NAMESPACES_FLAG 0x100
#define SHOW_GRAPHS_FLAG 0x200

static struct option long_options[] =
{
  /* name, has_arg, flag, val */
  {"count", 0, 0, 'c'},
  {"ignore-errors", 0, 0, 'e'},
  {"feature", 1, 0, 'f'},
  {"guess", 0, 0, 'g'},
  {"help", 0, 0, 'h'},
  {"input", 1, 0, 'i'},
  {"input-uri", 1, 0, 'I'},
  {"mode", 1, 0, 'm'},
  {"ntriples", 0, 0, 'n'},
  {"output", 1, 0, 'o'},
  {"output-uri", 1, 0, 'O'},
  {"quiet", 0, 0, 'q'},
  {"replace-newlines", 0, 0, 'r'},
  {"scan", 0, 0, 's'},
  {"show-graphs", 0, 0, SHOW_GRAPHS_FLAG},
  {"show-namespaces", 0, 0, SHOW_NAMESPACES_FLAG},
  {"trace", 0, 0, 't'},
  {"ignore-warnings", 0, 0, 'w'},
  {"version", 0, 0, 'v'},
  {NULL, 0, 0, 0}
};
#endif


static int error_count=0;
static int warning_count=0;

static int ignore_warnings=0;
static int ignore_errors=0;

static const char *title_format_string="Raptor RDF syntax parsing and serializing utility %s\n";


static void
rdfdump_error_handler(void *data, raptor_locator *locator,
                      const char *message)
{
  if(!ignore_errors) {
    fprintf(stderr, "%s: Error - ", program);
    raptor_print_locator(stderr, locator);
    fprintf(stderr, " - %s\n", message);
    
    raptor_parse_abort((raptor_parser*)data);
  }

  error_count++;
}


static void
rdfdump_warning_handler(void *data, raptor_locator *locator,
                        const char *message) 
{
  if(!ignore_warnings) {
    fprintf(stderr, "%s: Warning - ", program);
    raptor_print_locator(stderr, locator);
    fprintf(stderr, " - %s\n", message);
  }
  
  warning_count++;
}

struct namespace_decl
{
  unsigned char *prefix;
  unsigned char *uri_string;
};


static void
rdfdump_free_namespace_decl(void* data) {
  struct namespace_decl* nsd=(struct namespace_decl*)data;
  if(nsd->prefix)
    raptor_free_memory(nsd->prefix);
  if(nsd->uri_string)
    raptor_free_memory(nsd->uri_string);
  raptor_free_memory(nsd);
}


static int 
rapper_uri_trace(void *user_data, raptor_uri* uri)
{
  fprintf(stderr, "%s: Processing URI %s\n", program,
          raptor_uri_as_string(uri));
  return 0;
}


typedef struct
{
  raptor_feature feature;
  int i_value;
  unsigned char* s_value;
} feature_value;



int
main(int argc, char *argv[]) 
{
  raptor_parser* rdf_parser=NULL;
  unsigned char *uri_string=NULL;
  int free_uri_string=0;
  unsigned char *base_uri_string=NULL;
  unsigned char *output_base_uri_string=NULL;
  int rc;
  int scanning=0;
  const char *syntax_name="rdfxml";
  const char *serializer_syntax_name="ntriples";
  int strict_mode=0;
  int usage=0;
  int help=0;
  int trace=0;
  raptor_uri *base_uri=NULL;
  raptor_uri *output_base_uri=NULL;
  raptor_uri *uri;
  char *p;
  char *filename=NULL;
  raptor_sequence* parser_features=NULL;
  raptor_sequence* serializer_features=NULL;
  raptor_sequence *namespace_declarations=NULL;

  program=argv[0];
  if((p=strrchr(program, '/')))
    program=p+1;
  else if((p=strrchr(program, '\\')))
    program=p+1;
  argv[0]=program;

  raptor_init();

  while (!usage && !help)
  {
    int c;
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
        
      case 'a':
        break;

      case 'c':
        count=1;
        break;

      case 'f':
        if(optarg) {
          if(!strcmp(optarg, "help")) {
            int i;
            
            fprintf(stderr, "%s: Valid parser features are:\n", program);
            for(i=0; i < (int)raptor_get_feature_count(); i++) {
              const char *feature_name;
              const char *feature_label;
              if(!raptor_features_enumerate((raptor_feature)i, &feature_name, NULL, &feature_label)) {
                const char *feature_type=(raptor_feature_value_type((raptor_feature)i) == 0) ? "" : " (string)";
                fprintf(stderr, "  %-20s  %s%s\n", feature_name, feature_label, 
                       feature_type);
              }
            }
            fprintf(stderr, "%s: Valid serializer features are:\n", program);
            for(i=0; i < (int)raptor_get_feature_count(); i++) {
              const char *feature_name;
              const char *feature_label;
              if(!raptor_serializer_features_enumerate((raptor_feature)i, &feature_name, NULL, &feature_label)) {
                const char *feature_type=(raptor_feature_value_type((raptor_feature)i) == 0) ? "" : " (string)";
                fprintf(stderr, "  %-20s  %s%s\n", feature_name, feature_label, 
                       feature_type);
              }
            }
            fputs("Features are set with `" HELP_ARG(f, feature) " FEATURE=VALUE or `-f FEATURE'\nand take a decimal integer VALUE except where noted, defaulting to 1 if omitted.\n", stderr);
            fputs("\nA feature of the form xmlns:PREFIX=\"URI\" can be used to declare output\nnamespace prefixes and names for serializing using an XML-style syntax\nEither or both of PREFIX or URI can be omitted such as -f xmlns=\"URI\"\nThis form can be repeated for multiple declarations.\n", stderr);

            raptor_finish();
            exit(0);
          } else if(!strncmp(optarg, "xmlns", 5)) {
            struct namespace_decl *nd;
            nd=(struct namespace_decl *)raptor_alloc_memory(sizeof(struct namespace_decl));
            if(raptor_new_namespace_parts_from_string((unsigned char*)optarg,
                                                      &nd->prefix,
                                                      &nd->uri_string)) {
              fprintf(stderr, "%s: Bad xmlns syntax in '%s'\n", program, 
                      optarg);
              rdfdump_free_namespace_decl(nd);

              raptor_finish();
              exit(0);
            }

            if(!namespace_declarations)
              namespace_declarations=raptor_new_sequence(rdfdump_free_namespace_decl, NULL);

            raptor_sequence_push(namespace_declarations, nd);
          } else {
            int i;
            size_t arg_len=strlen(optarg);
            feature_value* fv;
            int ok=0;
            
            /* parser features */
            for(i=0; i < (int)raptor_get_feature_count(); i++) {
              const char *feature_name;
              size_t len;
              
              if(raptor_features_enumerate((raptor_feature)i, &feature_name, NULL, NULL))
                continue;

              len=strlen(feature_name);
              if(!strncmp(optarg, feature_name, len)) {
                fv=(feature_value*)raptor_alloc_memory(sizeof(feature_value));

                fv->feature=(raptor_feature)i;
                if(raptor_feature_value_type(fv->feature) == 0) {
                  if(len < arg_len && optarg[len] == '=')
                    fv->i_value=atoi(&optarg[len+1]);
                  else if(len == arg_len)
                    fv->i_value=1;
                } else {
                  if(len < arg_len && optarg[len] == '=')
                    fv->s_value=(unsigned char*)&optarg[len+1];
                  else if(len == arg_len)
                    fv->s_value=(unsigned char*)"";
                }

                if(!parser_features)
                  parser_features=raptor_new_sequence(raptor_free_memory, NULL);
                raptor_sequence_push(parser_features, fv);
                ok=1;
                break;
              }
            }
            
            for(i=0; i < (int)raptor_get_feature_count(); i++) {
              const char *feature_name;
              size_t len;
              
              if(raptor_serializer_features_enumerate((raptor_feature)i, &feature_name, NULL, NULL))
                continue;

              len=strlen(feature_name);
              if(!strncmp(optarg, feature_name, len)) {
                fv=(feature_value*)raptor_alloc_memory(sizeof(feature_value));

                fv->feature=(raptor_feature)i;
                if(raptor_feature_value_type(fv->feature) == 0) {
                  if(len < arg_len && optarg[len] == '=')
                    fv->i_value=atoi(&optarg[len+1]);
                  else if(len == arg_len)
                    fv->i_value=1;
                } else {
                  if(len < arg_len && optarg[len] == '=')
                    fv->s_value=(unsigned char*)&optarg[len+1];
                  else if(len == arg_len)
                    fv->s_value=(unsigned char*)"";
                }

                if(!serializer_features)
                  serializer_features=raptor_new_sequence(raptor_free_memory, NULL);
                raptor_sequence_push(serializer_features, fv);
                ok=1;
                break;
              }
            }
            
            if(!ok) {
              fprintf(stderr, "%s: invalid argument `%s' for `" HELP_ARG(f, feature) "'\nTry '%s " HELP_ARG(f, feature) " help' for a list of valid features\n",
                      program, optarg, program);
              usage=1;
            }
          }
        }
        break;

      case 'g':
        guess=1;
        break;

      case 'h':
        help=1;
        break;

      case 'n':
        syntax_name="ntriples";
        break;

      case 's':
        scanning=1;
        break;

      case 't':
        trace=1;
        break;

      case 'q':
        quiet=1;
        break;

      case 'r':
        replace_newlines=1;
        break;

      case 'm':
        if(optarg) {
          if(!strcmp(optarg, "strict"))
            strict_mode=1;
          else if (!strcmp(optarg, "lax"))
            strict_mode=0;
          else {
            fprintf(stderr, "%s: invalid argument `%s' for `" HELP_ARG(m, mode) "'\n",
                    program, optarg);
            fprintf(stderr, "Valid arguments are:\n  - `lax'\n  - `strict'\n");
            usage=1;
          }
        }
        break;

      case 'o':
        if(optarg) {
          if(raptor_serializer_syntax_name_check(optarg))
            serializer_syntax_name=optarg;
          else {
            int i;
            
            fprintf(stderr, "%s: invalid argument `%s' for `" HELP_ARG(o, output) "'\n",
                    program, optarg);
            fprintf(stderr, "Valid arguments are:\n");
            for(i=0; 1; i++) {
              const char *help_name;
              const char *help_label;
              if(raptor_serializers_enumerate(i, &help_name, &help_label, NULL, NULL))
                break;
              fprintf(stderr, "  %-12s for %s\n", help_name, help_label);
            }
            usage=1;
            break;
            
          }
        }
        break;

      case 'O':
        if(optarg)
          output_base_uri_string=(unsigned char*)optarg;
        break;
        
      case 'i':
        if(optarg) {
          if(raptor_syntax_name_check(optarg))
            syntax_name=optarg;
          else {
            int i;
            
            fprintf(stderr, "%s: invalid argument `%s' for `" HELP_ARG(i, input) "'\n",
                    program, optarg);
            fprintf(stderr, "Valid arguments are:\n");
            for(i=0; 1; i++) {
              const char *help_name;
              const char *help_label;
              if(raptor_syntaxes_enumerate(i, &help_name, &help_label, NULL, NULL))
                break;
              fprintf(stderr, "  %-12s for %s\n", help_name, help_label);
            }
            usage=1;
            break;
            
          }
        }
        break;

      case 'I':
        if(optarg)
          base_uri_string=(unsigned char*)optarg;
        break;
        
      case 'w':
        ignore_warnings=1;
        break;
        
      case 'e':
        ignore_errors=1;
        break;

      case 'v':
        fputs(raptor_version_string, stdout);
        fputc('\n', stdout);

        raptor_finish();
        exit(0);

#ifdef SHOW_NAMESPACES_FLAG
      case SHOW_NAMESPACES_FLAG:
        report_namespace=1;
        break;
#endif

#ifdef SHOW_GRAPHS_FLAG
      case SHOW_GRAPHS_FLAG:
        report_graph=1;
        break;
#endif

    } /* end switch */

  }

  if(optind != argc-1 && optind != argc-2 && !help && !usage) {
    usage=2; /* Title and usage */
  }

  
  if(usage) {
    if(usage>1) {
      fprintf(stderr, title_format_string, raptor_version_string);
      fputs("Raptor home page: ", stderr);
      fputs(raptor_home_url_string, stderr);
      fputc('\n', stderr);
      fputs(raptor_copyright_string, stderr);
      fputs("\nLicense: ", stderr);
      fputs(raptor_license_string, stderr);
      fputs("\n\n", stderr);
    }
    fprintf(stderr, "Try `%s " HELP_ARG(h, help) "' for more information.\n",
                    program);

    raptor_finish();
    exit(1);
  }

  if(help) {
    int i;
    
    printf(title_format_string, raptor_version_string);
    puts("Parse RDF syntax from a source into serialized RDF triples.");
    printf("Usage: %s [OPTIONS] INPUT-URI [INPUT-BASE-URI]\n\n", program);

    fputs(raptor_copyright_string, stdout);
    fputs("\nLicense: ", stdout);
    puts(raptor_license_string);
    fputs("Raptor home page: ", stdout);
    puts(raptor_home_url_string);

    puts("\nArguments:");
    puts("  INPUT-URI       a filename, URI or '-' for standard input (stdin).");
    puts("  INPUT-BASE-URI  the input/parser base URI or '-' for none."
         "\n"
         "    Default is INPUT-URI"
         "\n"
         "    Equivalent to" HELP_ARG_BOTH("I INPUT-BASE-URI", "input-uri INPUT-BASE-URI"));

    puts("\nMain options:");
    puts(HELP_TEXT("i FORMAT", "input FORMAT ", "Set the input format/parser to one of:"));
    for(i=0; 1; i++) {
      const char *help_name;
      const char *help_label;
      if(raptor_syntaxes_enumerate(i, &help_name, &help_label, NULL, NULL))
        break;
      printf("    %-14s  %s", help_name, help_label);
      if(!i)
        puts(" (default)");
      else
        putchar('\n');
    }
    puts(HELP_TEXT("I URI", "input-uri URI   ", "Set the input/parser base URI. '-' for none.") HELP_PAD "    Default is INPUT-BASE-URI argument value.");
    putchar('\n');

    puts(HELP_TEXT("o FORMAT", "output FORMAT", "Set the output format/serializer to one of:"));
    for(i=0; 1; i++) {
      const char *help_name;
      const char *help_label;
      if(raptor_serializers_enumerate(i, &help_name, &help_label, NULL, NULL))
        break;
      printf("    %-14s  %s", help_name, help_label);
      if(!i)
        puts(" (default)");
      else
        putchar('\n');
    }
    puts(HELP_TEXT("O URI", "output-uri URI  ", "Set the output/serializer base URI. '-' for none.")  HELP_PAD "    Default is input/parser base URI.");
    putchar('\n');

    puts("General options:");
    puts(HELP_TEXT("c", "count           ", "Count triples only - do not print them."));
    puts(HELP_TEXT("e", "ignore-errors   ", "Ignore error messages"));
    puts(HELP_TEXT("f FEATURE(=VALUE)", "feature FEATURE(=VALUE)", HELP_PAD "Set parser or serializer features" HELP_PAD "Use `-f help' for a list of valid features"));
    puts(HELP_TEXT("g", "guess           ", "Guess the input syntax (same as -i guess)"));
    puts(HELP_TEXT("h", "help            ", "Print this help, then exit"));
    puts(HELP_TEXT("m MODE", "mode MODE  ", "Set parser mode - 'lax' (default) or 'strict'"));
    puts(HELP_TEXT("q", "quiet           ", "No extra information messages"));
    puts(HELP_TEXT("r", "replace-newlines", "Replace newlines with spaces in literals"));
    puts(HELP_TEXT("s", "scan            ", "Scan for <rdf:RDF> element in source"));
#ifdef SHOW_GRAPHS_FLAG
    puts(HELP_TEXT_LONG("show-graphs     ", "Show named graphs as they are declared"));
#endif
#ifdef SHOW_NAMESPACES_FLAG
    puts(HELP_TEXT_LONG("show-namespaces ", "Show namespaces as they are declared"));
#endif
    puts(HELP_TEXT("t", "trace           ", "Trace URIs retrieved during parsing"));
    puts(HELP_TEXT("w", "ignore-warnings ", "Ignore warning messages"));
    puts(HELP_TEXT("v", "version         ", "Print the Raptor version"));
    puts("\nReport bugs to http://bugs.librdf.org/");

    raptor_finish();
    exit(0);
  }


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
    if(!uri_string) {
      fprintf(stderr, "%s: Failed to create URI for file %s.\n",
              program, filename);
      return(1);
    }
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


  /* Set the input/parser base URI */
  if(base_uri_string) {
    if(strcmp((const char*)base_uri_string, "-")) {
      base_uri=raptor_new_uri(base_uri_string);
      if(!base_uri) {
        fprintf(stderr, "%s: Failed to create URI for %s\n",
                program, base_uri_string);
        return(1);
      }
    }
  }


  /* Set the output/serializer base URI from the argument if explicitly
   * set, otherwise default to the input base URI if present.
   */
  if(!output_base_uri_string) {
    if(base_uri)
      output_base_uri=raptor_uri_copy(base_uri);
  } else {
    if(strcmp((const char*)output_base_uri_string, "-")) {
      output_base_uri=raptor_new_uri((const unsigned char*)output_base_uri_string);
      if(!output_base_uri) {
        fprintf(stderr, "%s: Failed to create output base URI for %s\n",
                program, output_base_uri_string);
        return(1);
      }
    } else
      output_base_uri=NULL;
  }
    

  if(guess)
    syntax_name="guess";

  rdf_parser=raptor_new_parser(syntax_name);
  if(!rdf_parser) {
    fprintf(stderr, "%s: Failed to create raptor parser type %s\n", program,
            syntax_name);
    return(1);
  }

  raptor_set_error_handler(rdf_parser, rdf_parser, rdfdump_error_handler);
  raptor_set_warning_handler(rdf_parser, rdf_parser, rdfdump_warning_handler);
  
  raptor_set_parser_strict(rdf_parser, strict_mode);
  
  if(scanning)
    raptor_set_feature(rdf_parser, RAPTOR_FEATURE_SCANNING, 1);

  if(parser_features) {
    feature_value *fv;
    while((fv=(feature_value*)raptor_sequence_pop(parser_features))) {
      if(fv->s_value)
        raptor_parser_set_feature_string(rdf_parser, fv->feature, fv->s_value);
      else
        raptor_set_feature(rdf_parser, fv->feature, fv->i_value);
      raptor_free_memory(fv);
    }
    raptor_free_sequence(parser_features);
    parser_features=NULL;
  }

  if(trace)
    raptor_parser_set_uri_filter(rdf_parser, rapper_uri_trace, NULL);


  if(!quiet) {
    if (filename) {
      if(base_uri_string)
        fprintf(stderr, "%s: Parsing file %s with parser %s and base URI %s\n", program,
                filename, syntax_name, base_uri_string);
      else
        fprintf(stderr, "%s: Parsing file %s with parser %s\n", program,
                filename, syntax_name);
    } else {
      if(base_uri_string)
        fprintf(stderr, "%s: Parsing URI %s with parser %s and base URI %s\n",
                program, uri_string, syntax_name, base_uri_string);
      else
        fprintf(stderr, "%s: Parsing URI %s with parser %s\n", program,
                uri_string, syntax_name);
    }
  }
  
  raptor_set_statement_handler(rdf_parser, rdf_parser, print_triples);

  if(report_graph)
    raptor_set_graph_handler(rdf_parser, rdf_parser, print_graph);

  if(report_namespace)
    raptor_set_namespace_handler(rdf_parser, rdf_parser, print_namespaces);


  if(serializer_syntax_name) {    
    if(!quiet) {
      if(output_base_uri)
        fprintf(stderr, "%s: Serializing with serializer %s and base URI %s\n",
                program, serializer_syntax_name,
                raptor_uri_as_string(output_base_uri));
      else
        fprintf(stderr, "%s: Serializing with serializer %s\n",
                program, serializer_syntax_name);
    }

    serializer=raptor_new_serializer(serializer_syntax_name);
    if(!serializer) {
      fprintf(stderr, 
              "%s: Failed to create raptor serializer type %s\n", program,
              serializer_syntax_name);
      return(1);
    }

    if(namespace_declarations) {
      int i;
      for(i=0; i< raptor_sequence_size(namespace_declarations); i++) {
        struct namespace_decl *nd=(struct namespace_decl *)raptor_sequence_get_at(namespace_declarations, i);
        raptor_uri *ns_uri=NULL;
        if(nd->uri_string)
          ns_uri=raptor_new_uri(nd->uri_string);
        
        raptor_serialize_set_namespace(serializer, ns_uri, nd->prefix);
        if(ns_uri)
          raptor_free_uri(ns_uri);
      }
      raptor_free_sequence(namespace_declarations);
      namespace_declarations=NULL;
    }
    
    if(serializer_features) {
      feature_value *fv;
      while((fv=(feature_value*)raptor_sequence_pop(serializer_features))) {
        if(fv->s_value)
          raptor_serializer_set_feature_string(serializer, fv->feature,
                                               fv->s_value);
        else
          raptor_serializer_set_feature(serializer, fv->feature, fv->i_value);
        raptor_free_memory(fv);
      }
      raptor_free_sequence(serializer_features);
      serializer_features=NULL;
    }

    raptor_serialize_start_to_file_handle(serializer, 
                                          output_base_uri, stdout);

    if(!report_namespace)
      raptor_set_namespace_handler(rdf_parser, serializer, relay_namespaces);
  }
  

  /* PARSE the URI as RDF/XML */
  rc=0;
  if(!uri || filename) {
    if(raptor_parse_file(rdf_parser, uri, base_uri)) {
      fprintf(stderr, "%s: Failed to parse file %s %s content\n", program, 
              filename, syntax_name);
      rc=1;
    }
  } else {
    if(raptor_parse_uri(rdf_parser, uri, base_uri)) {
      fprintf(stderr, "%s: Failed to parse URI %s %s content\n", program, 
              uri_string, syntax_name);
      rc=1;
    }
  }

  raptor_free_parser(rdf_parser);

  if(serializer) {
    raptor_serialize_end(serializer);
    raptor_free_serializer(serializer);
  }
  

  if(!quiet)
    fprintf(stderr, "%s: Parsing returned %d triple%s\n", program,
            triple_count, (triple_count == 1 ? "" : "s"));

  if(output_base_uri)
    raptor_free_uri(output_base_uri);
  if(base_uri)
    raptor_free_uri(base_uri);
  if(uri)
    raptor_free_uri(uri);
  if(free_uri_string)
    raptor_free_memory(uri_string);

  if(namespace_declarations)
    raptor_free_sequence(namespace_declarations);
  if(parser_features)
    raptor_free_sequence(parser_features);
  if(serializer_features)
    raptor_free_sequence(serializer_features);

  raptor_finish();

  if(error_count && !ignore_errors)
    return 1;

  if(warning_count && !ignore_warnings)
    return 2;

  return(rc);
}
