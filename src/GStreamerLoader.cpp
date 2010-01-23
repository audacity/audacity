/**********************************************************************

Audacity: A Digital Audio Editor

GStreamerLoader.cpp

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

\class GStreamerLoader
\brief Class used to dynamically load and initialize GStreamer

*//*******************************************************************/

#include "Audacity.h"	// for config*.h
#include "GStreamerLoader.h"
#include "import/ImportGStreamer.h"
#include "AudacityApp.h"

#ifdef _DEBUG
   #ifdef _MSC_VER
      #undef THIS_FILE
      static char*THIS_FILE= __FILE__;
      #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
   #endif
#endif

#if !defined(USE_GSTREAMER)
/// GStreamer support may or may not be compiled in,
/// but Preferences dialog requires this function nevertheless
wxString GetGStreamerVersion(wxWindow *parent)
{
   return wxString(wxT("GStreamer support is not compiled in"));
}

void GStreamerStartup()
{
}

#else

void GLogHandlerFunction(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data);

GStreamerLoader *GStreamerInst = NULL;

// This function should dump stream information to debug log.
gboolean LogStructure(GQuark field_id, const GValue *value, gpointer user_data)
{
   GString *strinfo = (GString*)user_data;
   // Get a name of a field
   const gchar *field_name = g_quark_to_string(field_id);

   gchar *value_str_value = gst_value_serialize(value);

   // TODO: push this into future over and over until wxLogMessage becomes threadsafe (code already in trunk)
#if wxCHECK_VERSION(2, 9, 0)
   wxLogMessage(wxT("Field %s = %s"), wxString::FromUTF8(field_name).c_str(), wxString::FromUTF8(value_str_value).c_str());
#endif
   if (strinfo)
      g_string_append_printf(strinfo, " %s[%s]",field_name, value_str_value);
   g_free(value_str_value);
   return TRUE;
}


void GstGLogHandlerFunction(const gchar *string)
{
#if 0
   static bool runtimedebugvar = false;
   if (runtimedebugvar)
   {
      FILE *f;
      errno_t err = 1;
      while (err != 0)
         err = fopen_s(&f,"gstreamer.debug.log","ab");
      fprintf(f,"%s",string);
      fclose(f);
   }
#else
  g_printerr("%s",string);
#endif
}

bool GStreamerLoader::LoadGStreamer(bool showerror)
{
   // First, we need to intialize GLib threading system
   if (!g_thread_supported())
      g_thread_init(NULL);
   else
      return false;
  
   // This will be used in future to give GStreamer additional arguments
   wxString arguments = wxEmptyString;
   if (gPrefs) {
      arguments = gPrefs->Read(wxT("/GStreamer/Arguments"), wxEmptyString);
   }

   // TODO: convert argument string into argc/argv pair.
   // TODO: Also, convert prefs control states into arguments and append to argument string
   int argc = 0;
   char **argv = NULL;

   wxLogMessage(wxT("Audacity is built against GStreamer version %d.%d.%d-%d"),GST_VERSION_MAJOR,GST_VERSION_MINOR,GST_VERSION_MICRO,GST_VERSION_NANO);
   wxLogMessage(wxT("Initializing GStreamer with arguments: \"%s\""),arguments.c_str());
   GError *errorptr = NULL;

   if (!gst_init_check(&argc, &argv,&errorptr))
   {
      wxLogMessage(wxT("Failed to initialize GStreamer. Error %d: %s"),errorptr->code, wxString::FromUTF8(errorptr->message).c_str());
      g_error_free(errorptr);
      return false;
   }

   gst_version(&major, &minor, &micro, &nano);
   wxLogMessage(wxT("Linked to GStreamer version %d.%d.%d-%d"),major,minor,micro,nano);

   g_set_printerr_handler(GstGLogHandlerFunction);
   // Debugging makes GStreamer roughly 4-5 times slower when it is not writing anything and 10 times slower with it writes info into a file
#if 0
   gst_debug_set_active(TRUE);
   gst_debug_set_default_threshold(GST_LEVEL_LOG);
#endif
   mGStreamerLoaded = true;

   return true;
}

// Obtains a list of supported extensions from typefind factories
// TODO: improve the list. It is obviously incomplete and also contains unuseful extensions (such as .jpg)
wxArrayString GStreamerLoader::GetExtensions()
{
   wxArrayString result;
   if (!mGStreamerLoaded) return result;

   GList *factories, *list;
   factories = list = gst_type_find_factory_get_list();
   while (list)
   {
      GstTypeFindFactory *factory = GST_TYPE_FIND_FACTORY(list->data);
      list = g_list_next(list);

      gchar **extensions = gst_type_find_factory_get_extensions(factory);
      if (!extensions)
        continue;
      for (gint i = 0; extensions[i]; i++)
      {
         wxString extension = wxString::FromUTF8(extensions[i]);
         if (result.Index(extension.c_str(), false) == wxNOT_FOUND)
         {
            result.Add(extension);
         }
      }
   }
   gst_plugin_feature_list_free(factories);

   result.Sort();
   wxString extensions = wxT("Extensions:");
   for (size_t i = 0; i < result.GetCount(); i++)
     extensions = extensions + wxT(" ") + result[i];
   wxLogMessage(wxT("%s"),extensions.c_str());

   return result;
}

// This one is for Glib.
void GLogHandlerFunction(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data)
{
   wxString level;
   switch (log_level)
   {
   case G_LOG_LEVEL_CRITICAL:
      level = wxT("Critical");
      break;
   case G_LOG_LEVEL_DEBUG:
      level = wxT("Debug");
      break;
   case G_LOG_LEVEL_ERROR:
      level = wxT("Error");
      break;
   case G_LOG_LEVEL_INFO:
      level = wxT("Info");
      break;
   case G_LOG_LEVEL_WARNING:
      level = wxT("Warning");
      break;
   case G_LOG_LEVEL_MESSAGE:
      level = wxT("Message");
      break;
   default:
      level = wxT("Unknown");
   }
#if wxCHECK_VERSION(2, 9, 0)
   wxLogMessage(_("GStreamer %s: %s"),level.c_str(),wxString::FromUTF8(message).c_str());
#endif
}

void GStreamerStartup()
{
   GStreamerInst = new GStreamerLoader;
   guint handler_id = g_log_set_handler(NULL,GLogLevelFlags(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),GLogHandlerFunction,(gpointer)GStreamerInst);
   
   bool enabled = false;
   gPrefs->Read(wxT("/GStreamer/Enabled"),&enabled);
#if _DEBUG //experimental debugging
   enabled = true;
#endif
   // 'false' means that no errors should be shown whatsoever
   if (enabled && !GStreamerInst->LoadGStreamer(false))
   {
     wxMessageBox(_("GStreamer was configured in preferences and successfully loaded before,\n\
                      but this time Audacity failed to load it at startup.\n\
                      You may want to go back to Preferences > Libraries and re-configure it."),
                      _("GStreamer startup failed"));
     delete GStreamerInst;
   }
}

wxString GetGStreamerVersion(wxWindow *parent)
{
   wxString versionString = wxT("GStreamer is not found");

   if (GStreamerInst && GStreamerInst->Loaded()) {
      versionString = GStreamerInst->GetVersion();
   }

   return versionString;
}
//----------------------------------------------------------------------------
// GStreamerLoader
//----------------------------------------------------------------------------

GStreamerLoader::GStreamerLoader()
{
   mGStreamerLoaded = false;
   major = minor = micro = nano = 0;
}

GStreamerLoader::~GStreamerLoader()
{
};

#endif //USE_GSTREAMER
