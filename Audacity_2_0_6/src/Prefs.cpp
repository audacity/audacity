/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file Prefs.cpp
\brief Utility functions for working with our wxConf (gPrefs)


  Audacity uses wxWidgets' wxConfig class to handle preferences.
  See Prefs.h for more information on how it works...

\verbatim
  Note: The info below is very outdated and incomplete

  Preference field specification:
   /
      Version					- Audacity Version that created these prefs
      DefaultOpenPath			- Default directory for new file selector
   /FileFormats
      CopyOrEditUncompressedData - Copy data from uncompressed files or
         [ "copy", "edit"]   - edit in place?
      ExportFormat_SF1		   - Format to export PCM data in
                             (this number is a libsndfile1.0 format)
   /SamplingRate
      DefaultProjectSampleRate- New projects will have this rate
         [ 8000, 11025, 16000, 22050, 44100, 48000 ]
   /AudioIO
      PlaybackDevice			- device to use for playback
      RecordingDevice			- device to use for recording
         (these are device names understood by PortAudio)
   /Display
      WaveformColor			- 0xRRGGBB  --since it will be stored in
      ShadowColor				- 			  decimal, it will be somewhat
      SpectrumLowColor		- 			  non-intuitive to edit, but
      SpectrumHighColor		- 			  much easier to parse.
   /Locale
      Language				- two-letter language code for translations

   (*): wxGTK
   (+): wxWin
   ($): wxMac
\endverbatim

*//*******************************************************************/


#include "Audacity.h"

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/app.h>
#include <wx/config.h>
#include <wx/intl.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>

#include "FileNames.h"

#include "sndfile.h"

#ifdef __WXMAC__
#include <CoreServices/CoreServices.h>

/* prototype of MoreFiles fn, included in wxMac already */
pascal OSErr FSpGetFullPath(const FSSpec * spec,
                            short *fullPathLength, Handle * fullPath);
#endif

#include "Prefs.h"

wxFileConfig *gPrefs = NULL;
int gMenusDirty = 0;

// Copy one entry from one wxConfig object to another
static void CopyEntry(wxString path, wxConfigBase *src, wxConfigBase *dst, wxString entry)
{
   switch(src->GetEntryType(entry)) {
   case wxConfigBase::Type_Unknown:
   case wxConfigBase::Type_String: {
      wxString value = src->Read(entry, wxT(""));
      dst->Write(path + entry, value);
      break;
   }
   case wxConfigBase::Type_Boolean: {
      bool value = false;
      src->Read(entry, &value, value);
      dst->Write(path + entry, value);
      break;
   }
   case wxConfigBase::Type_Integer: {
      long value = false;
      src->Read(entry, &value, value);
      dst->Write(path + entry, value);
      break;
   }
   case wxConfigBase::Type_Float: {
      double value = false;
      src->Read(entry, &value, value);
      dst->Write(path + entry, value);
      break;
   }
   }
}

// Recursive routine to copy all groups and entries from one wxConfig object to another
static void CopyEntriesRecursive(wxString path, wxConfigBase *src, wxConfigBase *dst)
{
   wxString entryName;
   long entryIndex;
   bool entryKeepGoing;

   entryKeepGoing = src->GetFirstEntry(entryName, entryIndex);
   while (entryKeepGoing) {
      CopyEntry(path, src, dst, entryName);
      entryKeepGoing = src->GetNextEntry(entryName, entryIndex);
   }

   wxString groupName;
   long groupIndex;
   bool groupKeepGoing;

   groupKeepGoing = src->GetFirstGroup(groupName, groupIndex);
   while (groupKeepGoing) {
      wxString subPath = path+groupName+wxT("/");
      src->SetPath(subPath);
      CopyEntriesRecursive(subPath, src, dst);
      src->SetPath(path);
      groupKeepGoing = src->GetNextGroup(groupName, groupIndex);
   }
}

void InitPreferences()
{
   wxString appName = wxTheApp->GetAppName();

   wxFileName configFileName(FileNames::DataDir(), wxT("audacity.cfg"));

   gPrefs = new wxFileConfig(appName, wxEmptyString,
                             configFileName.GetFullPath(),
                             wxEmptyString, wxCONFIG_USE_LOCAL_FILE);

   wxConfigBase::Set(gPrefs);

   static wxString resourcesDir;
   resourcesDir = wxStandardPaths::Get().GetResourcesDir();
   wxFileName fn;
   fn = wxFileName( resourcesDir, wxT("resetPrefs.txt") );
   if( fn.FileExists() )   // it will exist if the (win) installer put it there on request
   {
      // pop up a dialogue
      wxString prompt = _("Reset Preferences?\n\nThis is a one-time question, after an 'install' where you asked to have the Preferences reset.");
      int action = wxMessageBox(prompt, _("Reset Audacity Preferences"),
                                wxYES_NO, NULL);
      if(action == wxYES)   // reset
      {
         gPrefs->DeleteAll();
         gPrefs->Write(wxT("/NewPrefsInitialized"), true);
      }
      bool gone = wxRemoveFile(fn.GetFullPath());  // remove resetPrefs.txt
      if(!gone)
      {
         wxString fileName = fn.GetFullPath();
         wxMessageBox(wxString::Format( _("Failed to remove %s"), fileName.c_str()), _("Failed!"));
      }
   }

   // We introduced new file-based preferences in version 1.3.1; the
   // first time this version of Audacity is run we try to migrate
   // old preferences.
   bool newPrefsInitialized = false;
   gPrefs->Read(wxT("/NewPrefsInitialized"), &newPrefsInitialized, false);
   if (!newPrefsInitialized) {
      wxConfigBase *legacyConfig = new wxConfig(appName);
      CopyEntriesRecursive(wxT("/"), legacyConfig, gPrefs);
      delete legacyConfig;
      gPrefs->Write(wxT("/NewPrefsInitialized"), true);
   }

   gPrefs->Write(wxT("/Version"), wxString(AUDACITY_VERSION_STRING));

   // BG: Make sure the users prefs are up to date
   // BG: Otherwise reset some of them to their defaults
   wxString prefsversion;
   prefsversion = gPrefs->Read(wxT("/PrefsVersion"), wxT(""));

   if(prefsversion.CmpNoCase(wxString(wxT(AUDACITY_PREFS_VERSION_STRING))))
   {
      // BG: Reset the prefs by removing them
      if(gPrefs->Exists(wxT("/Keyboard")))
         gPrefs->DeleteGroup(wxT("/Keyboard"));
      if(gPrefs->Exists(wxT("/Locale")))
         gPrefs->DeleteGroup(wxT("/Locale"));
      gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));
   }

   // Check if some prefs updates need to happen based on audacity version.
   // Unfortunately we can't use the PrefsVersion prefs key because that resets things.
   // In the future we may want to integrate that better.
   // these are done on a case-by-case basis for now so they must be backwards compatible
   // (meaning the changes won't mess audacity up if the user goes back to an earlier version)
   int vMajor = gPrefs->Read(wxT("/Version/Major"), (long) 0);
   int vMinor = gPrefs->Read(wxT("/Version/Minor"), (long) 0);
   int vMicro = gPrefs->Read(wxT("/Version/Micro"), (long) 0);

   // These integer version keys were introduced april 4 2011 for 1.3.13
   // The device toolbar needs to be enabled due to removal of source selection features in
   // the mixer toolbar.
   if ((vMajor < 1) ||
       (vMajor == 1 && vMinor < 3) ||
       (vMajor == 1 && vMinor == 3 && vMicro < 13)) {


      // Do a full reset of the Device Toolbar to get it on the screen.
      if (gPrefs->Exists(wxT("/GUI/ToolBars/Device")))
         gPrefs->DeleteGroup(wxT("/GUI/ToolBars/Device"));

      // We keep the mixer toolbar prefs (shown/not shown)
      // the width of the mixer toolbar may have shrunk, the prefs will keep the larger value
      // if the user had a device that had more than one source.
      if (gPrefs->Exists(wxT("/GUI/ToolBars/Mixer"))) {
         // Use the default width
         gPrefs->Write(wxT("/GUI/ToolBars/Mixer/W"), -1);
      }
   }

   gPrefs->Write(wxT("/Version/Major"), AUDACITY_VERSION);
   gPrefs->Write(wxT("/Version/Minor"), AUDACITY_RELEASE);
   gPrefs->Write(wxT("/Version/Micro"), AUDACITY_REVISION);

   gPrefs->Flush();
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      delete gPrefs;
      gPrefs = NULL;
   }
}
