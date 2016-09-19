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
      DefaultOpenPath			- Default directory for NEW file selector
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

#include "AudacityApp.h"
#include "FileNames.h"
#include "Languages.h"

#include "Prefs.h"

std::unique_ptr<wxFileConfig> ugPrefs {};
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

#if 0
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
#endif

void InitPreferences()
{
   wxString appName = wxTheApp->GetAppName();

   wxFileName configFileName(FileNames::DataDir(), wxT("audacity.cfg"));

   ugPrefs = std::make_unique<wxFileConfig>
      (appName, wxEmptyString,
       configFileName.GetFullPath(),
       wxEmptyString, wxCONFIG_USE_LOCAL_FILE);
   gPrefs = ugPrefs.get();

   wxConfigBase::Set(gPrefs);

   bool resetPrefs = false;
   wxString langCode = gPrefs->Read(wxT("/Locale/Language"), wxEmptyString);
   bool writeLang = false;

   const wxFileName fn(wxStandardPaths::Get().GetResourcesDir(), wxT("FirstTime.ini"));
   if (fn.FileExists())   // it will exist if the (win) installer put it there
   {
      const wxString fullPath{fn.GetFullPath()};

      wxFileConfig ini(wxEmptyString,
                       wxEmptyString,
                       fullPath,
                       wxEmptyString,
                       wxCONFIG_USE_LOCAL_FILE);

      wxString lang;
      if (ini.Read(wxT("/FromInno/Language"), &lang))
      {
         // Only change "langCode" if the language was actually specified in the ini file.
         langCode = lang;
         writeLang = true;

         // Inno Setup doesn't allow special characters in the Name values, so "0" is used
         // to represent the "@" character.
         langCode.Replace(wxT("0"), wxT("@"));
      }

      ini.Read(wxT("/FromInno/ResetPrefs"), &resetPrefs, false);

      bool gone = wxRemoveFile(fullPath);  // remove FirstTime.ini
      if (!gone)
      {
         wxMessageBox(wxString::Format(_("Failed to remove %s"), fullPath.c_str()), _("Failed!"));
      }
   }

   // Use the system default language if one wasn't specified or if the user selected System.
   if (langCode.IsEmpty())
   {
      langCode = GetSystemLanguageCode();
   }

   // Initialize the language
   langCode = wxGetApp().InitLang(langCode);

   // User requested that the preferences be completely reset
   if (resetPrefs)
   {
      // pop up a dialogue
      wxString prompt = _("Reset Preferences?\n\nThis is a one-time question, after an 'install' where you asked to have the Preferences reset.");
      int action = wxMessageBox(prompt, _("Reset Audacity Preferences"),
                                wxYES_NO, NULL);
      if (action == wxYES)   // reset
      {
         gPrefs->DeleteAll();
         writeLang = true;
      }
   }

   // Save the specified language
   if (writeLang)
   {
      gPrefs->Write(wxT("/Locale/Language"), langCode);
   }

   // In AUdacity 2.1.0 support for the legacy 1.2.x preferences (depreciated since Audacity
   // 1.3.1) is dropped. As a result we can drop the import flag
   // first time this version of Audacity is run we try to migrate
   // old preferences.
   bool newPrefsInitialized = false;
   gPrefs->Read(wxT("/NewPrefsInitialized"), &newPrefsInitialized, false);
   if (newPrefsInitialized) {
      gPrefs->DeleteEntry(wxT("/NewPrefsInitialized"), true);  // take group as well if empty
   }

   // record the Prefs version for future checking (this has not been used for a very
   // long time).
   gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));

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

   // In 2.1.0, the Meter toolbar was split and lengthened, but strange arrangements happen
   // if upgrading due to the extra length.  So, if a user is upgrading, use the pre-2.1.0
   // lengths, but still use the NEW split versions.
   if (gPrefs->Exists(wxT("/GUI/ToolBars/Meter")) &&
      !gPrefs->Exists(wxT("/GUI/ToolBars/CombinedMeter"))) {

      // Read in all of the existing values
      long dock, order, show, x, y, w, h;
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Dock"), &dock, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Order"), &order, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Show"), &show, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/X"), &x, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Y"), &y, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/W"), &w, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/H"), &h, -1);

      // "Order" must be adjusted since we're inserting two NEW toolbars
      if (dock > 0) {
         wxString oldPath = gPrefs->GetPath();
         gPrefs->SetPath(wxT("/GUI/ToolBars"));

         wxString bar;
         long ndx = 0;
         bool cont = gPrefs->GetFirstGroup(bar, ndx);
         while (cont) {
            long o;
            if (gPrefs->Read(bar + wxT("/Order"), &o) && o >= order) {
               gPrefs->Write(bar + wxT("/Order"), o + 2);
            }
            cont = gPrefs->GetNextGroup(bar, ndx);
         }
         gPrefs->SetPath(oldPath);

         // And override the height
         h = 27;
      }

      // Write the split meter bar values
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Dock"), dock);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Order"), order);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Show"), show);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/X"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Y"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/W"), w);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/H"), h);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Dock"), dock);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Order"), order + 1);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Show"), show);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/X"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Y"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/W"), w);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/H"), h);

      // And hide the old combined meter bar
      gPrefs->Write(wxT("/GUI/ToolBars/Meter/Dock"), -1);
   }

   // write out the version numbers to the prefs file for future checking
   gPrefs->Write(wxT("/Version/Major"), AUDACITY_VERSION);
   gPrefs->Write(wxT("/Version/Minor"), AUDACITY_RELEASE);
   gPrefs->Write(wxT("/Version/Micro"), AUDACITY_REVISION);

   gPrefs->Flush();
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      ugPrefs.reset();
      gPrefs = NULL;
   }
}
