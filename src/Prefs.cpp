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
void CopyEntry(wxString path, wxConfigBase *src, wxConfigBase *dst, wxString entry)
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
void CopyEntriesRecursive(wxString path, wxConfigBase *src, wxConfigBase *dst)
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
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      delete gPrefs;
      gPrefs = NULL;
   }
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 4a8a9054-aec5-4093-8e02-fd65b646aeca

