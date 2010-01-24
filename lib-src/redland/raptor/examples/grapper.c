/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * grapper.c - Raptor GTK GUI example code
 *
 * Copyright (C) 2003-2007, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <win32_config.h>
#endif

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

/* Raptor includes */
#include <raptor.h>

/* Gtk 2.0 */
#include <gtk/gtk.h>

/* Gconf */
#include <gconf/gconf.h>
#include <gconf/gconf-client.h>

/* Qnames button does nothing */
#undef GRAPPER_QNAMES


static const char *application_name="Grapper";
static const char *application_title="Grapper GUI RDF Parser Utility";
static const char *application_description="GUI RDF parser utility based on the Raptor RDF parsing library";


/* Top level window */
static GtkWidget *grapper_window;


/* GConf */
static GConfClient *gconf_client=NULL;

#define GCONF_GRAPPER_NAMESPACE "/apps/grapper"

/* configuration dir listened to */
static const gchar* gconf_namespace= GCONF_GRAPPER_NAMESPACE;

/* window width key */
static const gchar* width_gconf_key=(const gchar*) GCONF_GRAPPER_NAMESPACE "/width";
/* window height key */
static const gchar* height_gconf_key=(const gchar*) GCONF_GRAPPER_NAMESPACE "/height";

#define MIN_WINDOW_WIDTH 400
#define MIN_WINDOW_HEIGHT 300


typedef struct
{
  /* model data */
#ifdef GRAPPER_QNAMES
  int qnames;
#endif
  int guess;
  unsigned int syntax;

  int features[RAPTOR_FEATURE_LAST];
  int features_set[RAPTOR_FEATURE_LAST];
  int ignore_warnings;

  unsigned char *url;

  /* last picked filename or NULL */
  gchar *filename;

  /* GList *triples_list; */
  int triples_count;
  int warnings_count;
  int errors_count;
  gchar *error;

  /* view/controller data */
  GtkWidget *window;
  GtkWidget *v_box;
  GtkWidget *url_entry;
  GtkListStore *triples_store;
  GtkWidget *file_selection;
  GtkWidget *status;
  GtkWidget *triples_frame;
  GtkWidget *errors_frame;
  GtkListStore *errors_store;

} grapper_state;


typedef struct 
{
  grapper_state* state;
  int feature;
} grapper_widget_data;


enum {
  SUBJECT_COLUMN,
  PREDICATE_COLUMN,
  OBJECT_COLUMN,
  N_COLUMNS
};


/* Local prototypes */
static void grapper_model_parse(grapper_state *state);


static void
grapper_view_url_changed(grapper_state *state) 
{
  GtkWidget *url_entry=state->url_entry;

  gtk_entry_set_text(GTK_ENTRY(url_entry), (const gchar*)state->url);
}

#ifdef GRAPPER_QNAMES
static void
grapper_view_qnames_changed(grapper_state *state) 
{

}
#endif

static void
grapper_view_guess_changed(grapper_state *state) 
{

}

static void
grapper_view_feature_changed(grapper_state *state, int feature) 
{

}

static void
grapper_view_syntax_changed(grapper_state *state) 
{
  
}

static void
grapper_view_set_triples_count(grapper_state *state, int count)
{
#define TC_BUF_LEN 18
  char buf[TC_BUF_LEN+1];
  if(count>0)
    snprintf(buf, TC_BUF_LEN, "Triples: %d", count);
  else
    strcpy(buf, "Triples");
  
  gtk_frame_set_label(GTK_FRAME(state->triples_frame), buf);
}

static void
grapper_view_add_triple(grapper_state *state, unsigned char* nodes[3], int i)  
{
  GtkListStore *store=state->triples_store;
  GtkTreeIter iter;

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter,
                     SUBJECT_COLUMN,   nodes[0],
                     PREDICATE_COLUMN, nodes[1],
                     OBJECT_COLUMN,    nodes[2],
                     -1);  
}

static void
grapper_view_empty_triples(grapper_state *state)
{
  gtk_list_store_clear(state->triples_store);
  gtk_list_store_clear(state->errors_store);
}

static void
grapper_view_reset_status(grapper_state *state) {
  gtk_list_store_clear(state->errors_store);
}

static void
grapper_view_update_error_count(grapper_state *state) {
#define EC_BUF_LEN 18
  char buf[EC_BUF_LEN+1];
  int count=state->errors_count;

  if(count>0)
    snprintf(buf, EC_BUF_LEN, "Errors: %d", count);
  else
    strcpy(buf, "Errors");

  gtk_frame_set_label(GTK_FRAME(state->errors_frame), buf);
}

static void
grapper_view_add_error_message(grapper_state *state, gchar *error,
                               raptor_locator *locator, int is_error) {
  if(error) {
    GtkListStore *store=state->errors_store;
    GtkTreeIter iter;
    int line=(locator && locator->line >=0) ? locator->line : 0;

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 
                       0, line,
                       1, (is_error ? "Error" : "Warning"),
                       2, error,
                       -1);  
    grapper_view_update_error_count(state);
  }
}


static void
grapper_model_add_triple(grapper_state *state, unsigned char *nodes[3])  
{
  /* g_list_append(state->triples_list, nodes); */
  state->triples_count++;

  grapper_view_add_triple(state, nodes, state->triples_count-1);
  grapper_view_set_triples_count(state, state->triples_count);
}


static void
grapper_model_empty_triples(grapper_state *state)  
{
  /* g_list_free(state->triples_list); */

  grapper_view_empty_triples(state);
}


static void
grapper_model_set_url(grapper_state *state, const unsigned char *url) 
{
  if(state->url) {
    if(!strcmp((const char*)state->url, (const char*)url))
      return;
    g_free(state->url);
  }
  
  state->url=(unsigned char*)g_strdup((const char*)url);
  strcpy((char*)state->url, (const char*)url);

  grapper_view_url_changed(state);
}

#ifdef GRAPPER_QNAMES
static void
grapper_model_set_qnames (grapper_state *state, int qnames) {
  if(state->qnames == qnames)
    return;
  
  state->qnames=qnames;
  grapper_view_qnames_changed(state);
}
#endif

static void
grapper_model_set_guess (grapper_state *state, int guess) {
  if(state->guess == guess)
    return;
  
  state->guess=guess;
  grapper_view_guess_changed(state);
}

static void
grapper_model_set_feature(grapper_state *state, int feature, int value) {
  if(state->features[feature] == value)
    return;
  
  state->features[feature]=value;
  state->features_set[feature]=1;
  grapper_view_feature_changed(state, feature);
}

static void
grapper_model_set_syntax (grapper_state *state, unsigned int syntax) {
  if(state->syntax == syntax)
    return;
  
  state->syntax=syntax;
  grapper_view_syntax_changed(state);
}

static void
grapper_model_reset_counts(grapper_state *state) 
{
  state->triples_count=0;
  state->warnings_count=0;
  state->errors_count=0;
  grapper_view_update_error_count(state);
}


static void
grapper_model_reset_error(grapper_state *state) 
{
  if(state->error) {
    g_free(state->error);
    state->error=NULL;
  }
  grapper_view_reset_status(state);
}


static void
grapper_model_error_handler(void *data, raptor_locator *locator,
                            const char *message)
{
  grapper_state* state=(grapper_state*)data;
  
  state->errors_count++;
  if(state->error)
    g_free(state->error);
  state->error=g_strdup(message);
  
  grapper_view_add_error_message(state, state->error, locator, 1);
}


static void
grapper_model_warning_handler(void *data, raptor_locator *locator,
                              const char *message) 
{
  grapper_state* state=(grapper_state*)data;

  state->warnings_count++;

  if(state->ignore_warnings)
    return;
  
  if(state->error)
    g_free(state->error);
  state->error=g_strdup(message);

  grapper_view_add_error_message(state, state->error, locator, 0);
}


static void
grapper_model_statements_handler(void *data,
                                 const raptor_statement *statement) {
  grapper_state* state=(grapper_state*)data;
  unsigned char* nodes[3];
  
  nodes[0]=raptor_statement_part_as_string(statement->subject,
                                           statement->subject_type,
                                           NULL, NULL);
  nodes[1]=raptor_statement_part_as_string(statement->predicate,
                                           statement->predicate_type,
                                           NULL, NULL);
  nodes[2]=raptor_statement_part_as_string(statement->object,
                                           statement->object_type,
                                           statement->object_literal_datatype,
                                           statement->object_literal_language);
  
  grapper_model_add_triple(state, nodes);
  free(nodes[0]);
  free(nodes[1]);
  free(nodes[2]);
}


static void
grapper_model_parse(grapper_state *state) 
{
  raptor_uri* uri;
  raptor_parser* rdf_parser;
  const char *syntax_name;
  int i;
  
  if(!state->url)
    return;

  grapper_model_empty_triples(state);

  grapper_model_reset_counts(state);
  grapper_model_reset_error(state);

  uri=raptor_new_uri(state->url);
  raptor_parsers_enumerate(state->syntax, &syntax_name, NULL);

  if(state->guess) {
    rdf_parser=raptor_new_parser_for_content(NULL, NULL, NULL, 0, state->url);
    if(!rdf_parser) {
      fprintf(stderr, "Failed to create guessed raptor parser from uri %s\n",
              state->url);
      exit(1);
    }
    fprintf(stdout, "Guessed parser name '%s' from uri %s\n",
            raptor_get_name(rdf_parser), state->url);
  } else {
    rdf_parser=raptor_new_parser(syntax_name);
  }
  

  for(i=0; i <= RAPTOR_FEATURE_LAST; i++) {
    if(state->features_set[i])
      raptor_set_feature(rdf_parser, i, state->features[i]);
  }

  raptor_set_error_handler(rdf_parser, state, grapper_model_error_handler);
  raptor_set_warning_handler(rdf_parser, state, grapper_model_warning_handler);
  raptor_set_statement_handler(rdf_parser, state, grapper_model_statements_handler);

  raptor_parse_uri(rdf_parser, uri, NULL);

  raptor_free_parser(rdf_parser);
  raptor_free_uri(uri);
}



/* go button clicked / url entry activated callback */
static void
url_entry_callback(GtkWidget *widget, gpointer data)
{
  grapper_state* state=(grapper_state*)data;
  GtkWidget *url_entry=state->url_entry;
  grapper_model_set_url(state, (const unsigned char*)gtk_entry_get_text(GTK_ENTRY(url_entry)));
  grapper_model_parse(state);
}

#if GTK_CHECK_VERSION(2,4,0)
#else
/* file selection OK button clicked callback */
static void
fs_ok_button_callback(GtkWidget *widget, gpointer data)
{
  grapper_state* state=(grapper_state*)data;
  GtkWidget *files=state->file_selection;
  unsigned char *uri_string;
  
  state->filename=(gchar*)gtk_file_selection_get_filename(GTK_FILE_SELECTION (files));
  
  uri_string=raptor_uri_filename_to_uri_string(state->filename);
  
  gtk_widget_destroy(files);
  state->file_selection=NULL;

  grapper_model_set_url(state, uri_string);
  free(uri_string);

  grapper_model_parse(state);
}
#endif

/* open button clicked callback */
static void
open_button_callback(GtkWidget *widget, gpointer data)
{
  grapper_state* state=(grapper_state*)data;

#if GTK_CHECK_VERSION(2,4,0)
  unsigned char *uri_string;
  GtkWidget *files=gtk_file_chooser_dialog_new("Open",
                                               GTK_WINDOW(state->window),
                                               GTK_FILE_CHOOSER_ACTION_OPEN,
                                               GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                               GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                               NULL);

  if(state->filename)
    gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(files), state->filename);

  if (gtk_dialog_run(GTK_DIALOG (files)) == GTK_RESPONSE_ACCEPT) {
    state->filename=gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(files));
    uri_string=(unsigned char*)gtk_file_chooser_get_uri(GTK_FILE_CHOOSER(files));
    grapper_model_set_url(state, uri_string);
    g_free(uri_string);

    grapper_model_parse(state);
  }
  gtk_widget_destroy(files);

#else  
  GtkWidget *files=gtk_file_selection_new("Open");

  if(state->filename)
    gtk_file_selection_set_filename(GTK_FILE_SELECTION(files), state->filename);
  
  state->file_selection=files;
  
  /* Connect the ok_button to fs_ok_button_callback */
  g_signal_connect (G_OBJECT (GTK_FILE_SELECTION (files)->ok_button),
                    "clicked", G_CALLBACK(fs_ok_button_callback), state);
  /* Connect the cancel_button to destroy the widget */
  g_signal_connect_swapped (G_OBJECT(GTK_FILE_SELECTION(files)->cancel_button),
                            "clicked", G_CALLBACK(gtk_widget_destroy),
                            G_OBJECT(files));

  gtk_widget_show(files);
#endif
}


/* quit callback */
static void
quit_callback(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}


/* preferences feature menu item toggled callback */
static void
feature_menu_toggled(GtkCheckMenuItem *checkmenuitem, gpointer data)
{
  grapper_widget_data* sbdata=(grapper_widget_data*)data;
  int active=gtk_check_menu_item_get_active(checkmenuitem);

  grapper_model_set_feature(sbdata->state, sbdata->feature, active);
}


#ifdef GRAPPER_QNAMES
/* qnames button clicked callback */
static void
qnames_button_callback(GtkWidget *widget, gpointer data)
{
  grapper_state* state=(grapper_state*)data;
  int active=(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (widget)) != 0);
  
  grapper_model_set_qnames(state, active);
}
#endif

/* guess button clicked callback */
static void
guess_button_callback(GtkWidget *widget, gpointer data)
{
  grapper_state* state=(grapper_state*)data;
  int active=(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (widget)) != 0);
  
  grapper_model_set_guess(state, active);
}


/* syntax menu changed callback */
static void
syntax_menu_callback(GtkWidget *widget, gpointer data)
{
  grapper_state* state=(grapper_state*)data;
 
  unsigned int syntax=(unsigned int)gtk_option_menu_get_history(GTK_OPTION_MENU(widget));
  
  grapper_model_set_syntax(state, syntax);
}


/* delete (window) event callback */
static gboolean
delete_event_callback(GtkWidget *widget, GdkEvent  *event, gpointer data)
{
  return FALSE; /* continue normal event handing */
}

/* destroy callback */
static void
destroy_callback(GtkWidget *widget, gpointer data)
{
  gtk_main_quit ();
}


static void
open_menu_callback(gpointer data, guint action, GtkWidget *widget) 
{
  open_button_callback(widget, data);
}


static void
quit_menu_callback(gpointer data, guint action, GtkWidget *widget)
{
  quit_callback(widget, data);
}


static void
about_menu_callback(gpointer data, guint action, GtkWidget *widget)
{
  grapper_state* state=(grapper_state*)data;

#if GTK_CHECK_VERSION(2,5,0)
  /* 2.5.x about widget */
  const gchar* authors[2]= { "Dave Beckett http://purl.org/net/dajobe/", NULL };

#if 1
  /* using 2.5.x stock about */
  gtk_show_about_dialog(GTK_WINDOW(state->window), 
                        "authors",   authors,
                        "comments",  application_description,
                        "copyright", raptor_short_copyright_string,
                        "license",   raptor_license_string,
                        "name",      application_name,
                        "version",   raptor_version_string,
                        "website",   raptor_home_url_string,
                        "website-label", "Raptor",
                        NULL);
#else
  /* using 2.5.x by hand about */
  GtkWidget *about;

  about=gtk_about_dialog_new();
  gtk_about_dialog_set_name(GTK_ABOUT_DIALOG(about), application_name);
  gtk_about_dialog_set_version(GTK_ABOUT_DIALOG(about), raptor_version_string);
  gtk_about_dialog_set_copyright(GTK_ABOUT_DIALOG(about), raptor_short_copyright_string);
  gtk_about_dialog_set_comments(GTK_ABOUT_DIALOG(about), application_description);
  gtk_about_dialog_set_license(GTK_ABOUT_DIALOG(about), raptor_license_string);
  gtk_about_dialog_set_website(GTK_ABOUT_DIALOG(about), raptor_home_url_string);
  gtk_about_dialog_set_website_label(GTK_ABOUT_DIALOG(about), "Raptor");
  gtk_about_dialog_set_authors(GTK_ABOUT_DIALOG(about), authors);

  gtk_widget_show_all(about);
#endif

#else  
  GtkWidget *about;
  GtkWidget *label;

  label=(GtkWidget*)application_description; /* mention it for gcc -Wannoy */
  
  /* GTK < 2.6.0 */
  about=gtk_dialog_new_with_buttons("About Grapper", 
                                    GTK_WINDOW(state->window), 
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_STOCK_OK,
                                    GTK_RESPONSE_NONE,
                                    NULL);
  label = gtk_label_new ("Grapper\nGUI RDF parser utility\n (C) 2003-2004 Dave Beckett");
  /* Connect the dialog response to about_response_callback */
  g_signal_connect_swapped (G_OBJECT (about), "response",
                    G_CALLBACK(gtk_widget_destroy), GTK_OBJECT(about));

  gtk_container_add (GTK_CONTAINER (GTK_DIALOG(about)->vbox), label);
  gtk_widget_show_all(about);
#endif
}


static GtkItemFactoryEntry menu_item_factory_entries[] = {
  /* path,     accelerator,  callback, callback_action, item_type, extra_data */
  { (gchar*)"/_File",         NULL,      NULL,         0, (gchar*)"<Branch>" },
  { (gchar*)"/File/_Open...", (gchar*)"<CTRL>O", (GtkItemFactoryCallback)open_menu_callback, 1, (gchar*)"<StockItem>", GTK_STOCK_OPEN },
  { (gchar*)"/File/sep1",     NULL,      NULL,         0, (gchar*)"<Separator>" },
  { (gchar*)"/File/_Quit",    (gchar*)"<CTRL>Q", (GtkItemFactoryCallback)quit_menu_callback, 1, (gchar*)"<StockItem>", GTK_STOCK_QUIT },
  { (gchar*)"/_Preferences",  NULL,      NULL,         0, (gchar*)"<Branch>" },
  { (gchar*)"/_Help",         NULL,      NULL,         0, (gchar*)"<LastBranch>" },
  { (gchar*)"/Help/About",    NULL,     (GtkItemFactoryCallback)about_menu_callback, 1, (gchar*)"<Item>" } 
};

static gint menu_item_factory_nentries = sizeof(menu_item_factory_entries) / sizeof(menu_item_factory_entries[0]);


static void
init_grapper_window(GtkWidget *window, grapper_state *state) 
{

  GtkAccelGroup *accel_group;
  GtkItemFactory* menu_item_factory;
  GtkWidget *menu_bar;
  GtkMenu *prefs_menu;
  GtkWidget *v_paned;
  GtkWidget *v_box;
  GtkWidget *box;
  GtkWidget *go_button;
  GtkWidget* feature_items[RAPTOR_FEATURE_LAST];
#ifdef GRAPPER_QNAMES
  GtkWidget *qnames_button;
#endif
  GtkWidget *guess_button;
  GtkWidget *syntax_optionmenu;
  GtkWidget *syntax_menu;
  GtkWidget *url_entry;
  GtkWidget *triples_frame, *prefs_frame;
  GtkWidget *triples_scrolled_window;
  GtkWidget *triples_treeview;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
#ifdef GRAPPER_QNAMES
  GtkTooltips *qnames_tooltips;
#endif
  GtkTooltips *guess_tooltips;
  GtkTooltips *syntax_tooltips;
  GtkWidget *prefs_box;
  GtkListStore *store;
  int i;
  GtkWidget *errors_frame, *errors_scrolled_window;
  GtkWidget *errors_treeview;
  GtkListStore *errors_store;

  state->window=window;
  
  /* connect window delete event to callback */
  g_signal_connect (G_OBJECT (window), "delete_event",
                    G_CALLBACK (delete_event_callback), NULL);

  /* connect window destroy event to callback */
  g_signal_connect (G_OBJECT (window), "destroy",
                    G_CALLBACK (destroy_callback), NULL);



  /* vertical box */
  v_box = gtk_vbox_new (FALSE, 0);

  /* gtk_container_set_border_width (GTK_CONTAINER (v_box), 10); */

  state->v_box=v_box;
  

  /* acceleration group for menu bar*/
  accel_group=gtk_accel_group_new();


  /* Menu bar */
  menu_item_factory=gtk_item_factory_new(GTK_TYPE_MENU_BAR, 
                                         "<Main>", accel_group);
  gtk_item_factory_create_items(menu_item_factory,
                                menu_item_factory_nentries,
                                menu_item_factory_entries,
                                state);
  gtk_window_add_accel_group(GTK_WINDOW(window), accel_group);
  
  menu_bar=gtk_item_factory_get_widget (menu_item_factory, "<Main>");
  gtk_widget_show(menu_bar);
  

  gtk_box_pack_start (GTK_BOX (v_box), menu_bar, FALSE, FALSE, 0);


  /* horizontal box for url entry, OK, Open buttons in vertical box (v_box) */
  box = gtk_hbox_new (FALSE, 0);


  /* url text entry in horizontal box */
  url_entry=gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(url_entry), 200);
  /* connect text entry activate (enter key) callback */
  g_signal_connect (G_OBJECT(url_entry), "activate",
                    G_CALLBACK(url_entry_callback), state);
  gtk_editable_set_editable(GTK_EDITABLE(url_entry), TRUE);

  /* pack into the invisible box */
  gtk_box_pack_start(GTK_BOX(box), url_entry, TRUE, TRUE, 0);

  gtk_widget_show(url_entry);
  state->url_entry=url_entry;

  /* go button in horizontal box */
  go_button = gtk_button_new_from_stock(GTK_STOCK_OK);

  /* connect button clicked event to callback */
  g_signal_connect (G_OBJECT (go_button), "clicked",
                    G_CALLBACK (url_entry_callback), state);

  /* pack into the invisible box */
  gtk_box_pack_start (GTK_BOX(box), go_button, FALSE, TRUE, 0);

  gtk_widget_show (go_button);


  gtk_widget_show (box);


  /* add hbox to vbox */
  gtk_box_pack_start (GTK_BOX (v_box), box, FALSE, FALSE, 0);


  /* horizontal box for syntax prefs in vertical box (v_box) */
  prefs_frame = gtk_frame_new ("RDF Syntax");

  prefs_box = gtk_hbutton_box_new();

  gtk_button_box_set_layout(GTK_BUTTON_BOX(prefs_box),GTK_BUTTONBOX_START);

#ifdef GRAPPER_QNAMES
  /* qnames button in horizontal box */
  qnames_button = gtk_check_button_new_with_label("QNames");

  qnames_tooltips = gtk_tooltips_new ();
  gtk_tooltips_set_tip (qnames_tooltips, qnames_button, "Display URIs as XML QNames", NULL);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(qnames_button), (state->qnames));

  /* connect button clicked event to callback */
  g_signal_connect (G_OBJECT (qnames_button), "clicked",
                    G_CALLBACK (qnames_button_callback), state);

  /* pack into the invisible box */
  gtk_box_pack_start (GTK_BOX(prefs_box), qnames_button, TRUE, TRUE, 0);

  gtk_widget_show (qnames_button);
#endif  

  /* guess button in horizontal box */
  guess_button = gtk_check_button_new_with_label("Guess Syntax");

  guess_tooltips = gtk_tooltips_new ();
  gtk_tooltips_set_tip (guess_tooltips, guess_button, "Try to guess the syntax from the URI", NULL);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(guess_button), (state->guess));

  /* connect button clicked event to callback */
  g_signal_connect (G_OBJECT (guess_button), "clicked",
                    G_CALLBACK (guess_button_callback), state);

  /* pack into the invisible box */
  gtk_box_pack_start (GTK_BOX(prefs_box), guess_button, TRUE, TRUE, 0);

  gtk_widget_show (guess_button);

  /* add prefs frame to vbox */
  gtk_container_add(GTK_CONTAINER(prefs_frame), prefs_box);

  gtk_widget_show (prefs_box);

  /* add prefs frame to start of vbox */
  gtk_box_pack_start (GTK_BOX (v_box), prefs_frame, FALSE, TRUE, 0);

  gtk_widget_show (prefs_frame);



  /* paned in vertical box */
  v_paned = gtk_vpaned_new ();


  /* triples frame in vertical paned */
  triples_frame=gtk_frame_new("Triples");
  state->triples_frame=triples_frame;
  
  gtk_paned_pack1(GTK_PANED (v_paned), triples_frame, TRUE, FALSE);
  gtk_widget_show(triples_frame);


  /* scroll window in triples frame */
  triples_scrolled_window=gtk_scrolled_window_new(NULL, NULL);

  gtk_container_set_border_width(GTK_CONTAINER(triples_scrolled_window), 10);
    
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(triples_scrolled_window),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(triples_frame), triples_scrolled_window);
  gtk_widget_show(triples_scrolled_window);

  store = gtk_list_store_new (N_COLUMNS,
                              G_TYPE_STRING,
                              G_TYPE_STRING,
                              G_TYPE_STRING);
  state->triples_store=store;

  triples_treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(triples_treeview), TRUE);
  
  /* set columns renderer for treeview */
  renderer= gtk_cell_renderer_text_new ();
  column= gtk_tree_view_column_new_with_attributes ("Subject",
                                                    renderer,
                                                    "text", SUBJECT_COLUMN,
                                                    NULL);
  gtk_tree_view_column_set_sort_column_id(column, SUBJECT_COLUMN);
  gtk_tree_view_column_set_resizable(column, 1);
  gtk_tree_view_append_column (GTK_TREE_VIEW (triples_treeview), column);

  renderer= gtk_cell_renderer_text_new ();
  column= gtk_tree_view_column_new_with_attributes ("Predicate",
                                                    renderer,
                                                    "text", PREDICATE_COLUMN,
                                                    NULL);
  gtk_tree_view_column_set_sort_column_id(column, PREDICATE_COLUMN);
  gtk_tree_view_column_set_resizable(column, 1);
  gtk_tree_view_append_column (GTK_TREE_VIEW (triples_treeview), column);

  renderer= gtk_cell_renderer_text_new ();
  column= gtk_tree_view_column_new_with_attributes ("Object",
                                                    renderer,
                                                    "text", OBJECT_COLUMN,
                                                    NULL);
  gtk_tree_view_column_set_sort_column_id(column, OBJECT_COLUMN);
  gtk_tree_view_column_set_resizable(column, 1);
  gtk_tree_view_append_column (GTK_TREE_VIEW (triples_treeview), column);


  /* pack the store into the scrolled window */
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(triples_scrolled_window), triples_treeview);
  gtk_widget_show(triples_treeview);


  /* errors frame in vertical paned */
  errors_frame = gtk_frame_new ("Errors");
  state->errors_frame=errors_frame;

  gtk_paned_pack2(GTK_PANED (v_paned), errors_frame, TRUE, FALSE);
  gtk_widget_show(errors_frame);


  gtk_box_pack_start (GTK_BOX (v_box), v_paned, TRUE, TRUE, 0);
  gtk_widget_show(v_paned);


  /* scroll window in errors frame */
  errors_scrolled_window=gtk_scrolled_window_new(NULL, NULL);

  gtk_container_set_border_width(GTK_CONTAINER(errors_scrolled_window), 10);
    
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(errors_scrolled_window),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(errors_frame), errors_scrolled_window);
  gtk_widget_show(errors_scrolled_window);

  errors_store = gtk_list_store_new (3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
  state->errors_store = errors_store;
  
  errors_treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(errors_store));
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(errors_treeview), TRUE);
  
  renderer= gtk_cell_renderer_text_new ();
  column= gtk_tree_view_column_new_with_attributes ("Line",
                                                    renderer,
                                                    "text", 0,
                                                    NULL);
  gtk_tree_view_column_set_resizable(column, 1);
  gtk_tree_view_append_column (GTK_TREE_VIEW (errors_treeview), column);

  renderer= gtk_cell_renderer_text_new ();
  column= gtk_tree_view_column_new_with_attributes ("Type",
                                                    renderer,
                                                    "text", 1,
                                                    NULL);
  gtk_tree_view_column_set_resizable(column, 1);
  gtk_tree_view_append_column (GTK_TREE_VIEW (errors_treeview), column);

  renderer= gtk_cell_renderer_text_new ();
  column= gtk_tree_view_column_new_with_attributes ("Message",
                                                    renderer,
                                                    "text", 2,
                                                    NULL);
  gtk_tree_view_column_set_resizable(column, 1);
  gtk_tree_view_append_column (GTK_TREE_VIEW (errors_treeview), column);

  gtk_tooltips_set_tip (gtk_tooltips_new (), errors_treeview, 
                        "Errors and warnings from parsing the content.", NULL);


  /* pack the errors store into the errors scrolled window */
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(errors_scrolled_window), errors_treeview);
  gtk_widget_show(errors_treeview);




  prefs_menu=GTK_MENU(gtk_item_factory_get_widget(menu_item_factory, "/Preferences"));

  /* features in the preferences menu */
  for(i=0; i <= RAPTOR_FEATURE_LAST; i++) {
    const char *feature_name;
    const char *feature_label;
    grapper_widget_data* sbdata;

    if(raptor_features_enumerate((raptor_feature)i, 
                                 &feature_name, NULL, &feature_label))
      break;

    sbdata=(grapper_widget_data*)malloc(sizeof(grapper_widget_data));
    sbdata->state=state;
    sbdata->feature=i;

    /* add to the preferences menu */
    feature_items[i] = gtk_check_menu_item_new_with_label(feature_label);
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(feature_items[i]), 
                                   state->features[i]);
    gtk_menu_shell_append(GTK_MENU_SHELL(prefs_menu), feature_items[i]);

    g_signal_connect(G_OBJECT(feature_items[i]), "toggled",
                     G_CALLBACK(feature_menu_toggled), (gpointer)sbdata);
    gtk_widget_show (feature_items[i]);
  }


  /* syntax button in horizontal box */
  syntax_optionmenu = gtk_option_menu_new();

  syntax_menu=gtk_menu_new();
  for(i=0; 1; i++) {
    const char *syntax_label;
    GtkWidget *syntax_menu_item;
    
    if(raptor_parsers_enumerate(i, NULL, &syntax_label))
      break;

    syntax_menu_item = gtk_menu_item_new_with_label((const gchar*)syntax_label);
    gtk_widget_show (syntax_menu_item);
    gtk_menu_shell_append(GTK_MENU_SHELL(syntax_menu), syntax_menu_item);
  }

  g_signal_connect (GTK_OBJECT(syntax_optionmenu), "changed",
                    G_CALLBACK (syntax_menu_callback), state);

  gtk_option_menu_set_menu(GTK_OPTION_MENU(syntax_optionmenu), syntax_menu);

  /* Default is item 0 (should be RDF/XML) */
  gtk_option_menu_set_history(GTK_OPTION_MENU(syntax_optionmenu), 0);

  syntax_tooltips = gtk_tooltips_new ();
  gtk_tooltips_set_tip (syntax_tooltips, syntax_optionmenu, "Chose the Syntax to parse", NULL);

  /* pack into the invisible box */
  gtk_box_pack_start (GTK_BOX(prefs_box), syntax_optionmenu, TRUE, TRUE, 0);

  gtk_widget_show (syntax_optionmenu);


  /* add vbox to window */
  gtk_container_add (GTK_CONTAINER (window), v_box);
  gtk_widget_show (v_box);


}



static void
grapper_gconfclient_notify(GConfClient* client, guint cnxn_id,
                           GConfEntry *entry, gpointer user_data)
{
  /* grapper_state* state=(grapper_state*)user_data; */
  GError* err=NULL;
  int width, height;

  gtk_window_get_size(GTK_WINDOW(grapper_window), &width, &height);


  width=gconf_client_get_int(gconf_client, width_gconf_key, &err);
  if(err) {
    g_error_free(err);
    err=NULL;
    width= -1;
  } else
    fprintf(stderr, "gconf width changed to %d\n", width);
  
  height=gconf_client_get_int(gconf_client, height_gconf_key, &err);
  if(err) {
    g_error_free(err);
    err=NULL;
    height= -1;
  } else
    fprintf(stderr, "gconf height changed to %d\n", width);

  /* let's not make it too small */
  if(width < MIN_WINDOW_WIDTH)
    width = MIN_WINDOW_WIDTH;
  if(height < MIN_WINDOW_HEIGHT)
    height = MIN_WINDOW_HEIGHT;

  gtk_window_resize(GTK_WINDOW(grapper_window), width, height);
}


static void
grapper_gconflient_free(gpointer user_data)
{

}


static gint
configure_callback(GtkWidget *widget, GdkEventConfigure *event)
{
  gint width, height;
  GError* err=NULL;

  gtk_window_get_size(GTK_WINDOW(grapper_window), &width, &height);

  if(!gconf_client_set_int(gconf_client, width_gconf_key, width, &err)) {
    fprintf(stderr, "gconf error writing width: %s\n", err->message);
    g_error_free(err);
    err=NULL;
  }

  if(!gconf_client_set_int(gconf_client, height_gconf_key, height, &err)) {
    fprintf(stderr, "gconf error writing width: %s\n", err->message);
    g_error_free(err);
    err=NULL;
  }

  return FALSE;
}
 


int
main(int argc, char *argv[])
{
  grapper_state state;
  GError* err=NULL;
  guint cnxn;
  int width, height;
  
  gtk_init (&argc, &argv);

  g_set_application_name(application_name);
  
  raptor_init();
  
  memset(&state, 0, sizeof(grapper_state));
  
  gconf_client=gconf_client_get_default();

  cnxn=gconf_client_notify_add(gconf_client, gconf_namespace,
                               grapper_gconfclient_notify,
                               (gpointer)&state, /* user data */
                               grapper_gconflient_free,
                               &err);

  /* create the main window */
  grapper_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title (GTK_WINDOW(grapper_window), application_title);

  init_grapper_window(grapper_window, &state);

  width=gconf_client_get_int(gconf_client, width_gconf_key, &err);
  if(err) {
    fprintf(stderr, "gconf error reading width: %s\n", err->message);
    g_error_free(err);
    err=NULL;
    width= -1;
  }

  height=gconf_client_get_int(gconf_client, height_gconf_key, &err);
  if(err) {
    fprintf(stderr, "gconf error reading height: %s\n", err->message);
    g_error_free(err);
    err=NULL;
    height= -1;
  }

  /* let's not make it too small */
  if(width < MIN_WINDOW_WIDTH)
    width = MIN_WINDOW_WIDTH;
  if(height < MIN_WINDOW_HEIGHT)
    height = MIN_WINDOW_HEIGHT;
  
  gtk_window_set_default_size(GTK_WINDOW(grapper_window), width, height);

  /* Connect the window resize event to configure_callback */
  gtk_signal_connect (GTK_OBJECT(grapper_window),
                      "configure_event",
                      (GtkSignalFunc)configure_callback, &state);

  /* finally make it all visible */
  gtk_widget_show (grapper_window);

  if(argc>1) {
    if(!access(argv[1], R_OK)) {
      /* it's a file - make a URL out of it */
      unsigned char *uri_string=raptor_uri_filename_to_uri_string(argv[1]);
      grapper_model_set_url(&state, uri_string);
      free(uri_string);
    } else
      grapper_model_set_url(&state, (unsigned char*)argv[1]);

    grapper_model_parse(&state);
  }

  /* main loop, exited when gtk_main_quit() is called */
  gtk_main ();

  raptor_finish();

  gconf_client_notify_remove(gconf_client, cnxn);
  
  return 0;
}

