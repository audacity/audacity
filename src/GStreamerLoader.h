/**********************************************************************

Audacity: A Digital Audio Editor

GStreamerLoader.h

Audacity(R) is copyright (c) 1999-2008 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

Describes shared object that is used to access GStreamer

*//*******************************************************************/

#if !defined(__AUDACITY_GSTREAMER__)
#define __AUDACITY_GSTREAMER__

#include "Audacity.h"
/* rather earlier than normal, but pulls in config*.h and other program stuff
 * we need for the next bit */
#include <wx/string.h>
#include <wx/dynlib.h>
#include <wx/log.h>			// for wxLogNull
#include <wx/msgdlg.h>		// for wxMessageBox
#include <wx/utils.h>
#include "widgets/LinkingHtmlWindow.h"
#include "FileDialog.h"
#include "ShuttleGui.h"
#include "Prefs.h"
#include <wx/checkbox.h>
#include <wx/textctrl.h>
#include <wx/thread.h>

class GStreamerLoader;

extern GStreamerLoader *GStreamerInst;

//----------------------------------------------------------------------------
// Get GStreamer version
//----------------------------------------------------------------------------
wxString GetGStreamerVersion(wxWindow *parent);

//----------------------------------------------------------------------------
// Attempt to load and enable/disable GStreamer at startup
//----------------------------------------------------------------------------
void GStreamerStartup();

/* From here on in, this stuff only applies when gstreamer is available */
#if defined(USE_GSTREAMER)

// On Windows we don't have configure script to turn this on or off,
// so let's use msw-specific pragma to add required libraries.
// Of course, library search path still has to be updated manually
#   if defined(__WXMSW__)
#      pragma comment(lib,"libgstreamer-0.10.lib")
#      pragma comment(lib,"libgstapp-0.10.lib")
#      pragma comment(lib,"libgstbase-0.10.lib")
#      pragma comment(lib,"glib-2.0.lib")
#      pragma comment(lib,"gobject-2.0.lib")
#      pragma comment(lib,"gthread-2.0.lib")
#      pragma comment(lib,"libxml2.lib")
#   endif
    extern "C" {
#      include <gst/gst.h>
#      include <glib.h>
#      include <glib/gstdio.h>
    }

bool LoadGStreamer(bool showerror);

gboolean LogStructure(GQuark field_id, const GValue *value, gpointer user_data);

class GStreamerLoader
{
public:
   GStreamerLoader();
   ~GStreamerLoader();

   bool Loaded()
   {
      return mGStreamerLoaded;
   }

   wxString GetVersion()
   {
      return wxString::Format(wxT("%d.%d.%d-%d"),major, minor, micro, nano);
   }

   wxArrayString GetExtensions();

   bool LoadGStreamer(bool showerr);

private:

   unsigned int major, minor, micro, nano;

   bool mGStreamerLoaded;
};

#endif // USE_GSTREAMER
#endif // __AUDACITY_GSTREAMER__

