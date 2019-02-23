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
#include "Prefs.h"

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/intl.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>

#include "Internat.h"
#include "MemoryX.h"

std::unique_ptr<AudacityPrefs> ugPrefs {};

AudacityPrefs *gPrefs = NULL;
int gMenusDirty = 0;

wxDEFINE_EVENT(EVT_PREFS_UPDATE, wxCommandEvent);

struct PrefsListener::Impl : wxEvtHandler
{
   Impl( PrefsListener &owner );
   ~Impl();
   void OnEvent(wxCommandEvent&);
   PrefsListener &mOwner;
};

PrefsListener::Impl::Impl( PrefsListener &owner )
   : mOwner{ owner }
{
   wxTheApp->Bind(EVT_PREFS_UPDATE, &PrefsListener::Impl::OnEvent, this);
}

PrefsListener::Impl::~Impl()
{
}

PrefsListener::PrefsListener()
   : mpImpl{ std::make_unique<Impl>( *this ) }
{
}

PrefsListener::~PrefsListener()
{
}

void PrefsListener::UpdateSelectedPrefs( int )
{
}

void PrefsListener::Impl::OnEvent( wxCommandEvent &evt )
{
   evt.Skip();
   auto id = evt.GetId();
   if (id <= 0)
      mOwner.UpdatePrefs();
   else
      mOwner.UpdateSelectedPrefs( id );
}

#if 0
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
#endif

AudacityPrefs::AudacityPrefs(const wxString& appName,
               const wxString& vendorName,
               const wxString& localFilename,
               const wxString& globalFilename,
               long style,
               const wxMBConv& conv) :
   wxFileConfig(appName,
               vendorName,
               localFilename,
               globalFilename,
               style,
               conv)
{
}



// Bug 825 is essentially that SyncLock requires EditClipsCanMove.
// SyncLock needs rethinking, but meanwhile this function 
// fixes the issues of Bug 825 by allowing clips to move when in 
// SyncLock.
bool AudacityPrefs::GetEditClipsCanMove()
{
   bool mIsSyncLocked;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &mIsSyncLocked, false);
   if( mIsSyncLocked )
      return true;
   bool editClipsCanMove;
   Read(wxT("/GUI/EditClipCanMove"), &editClipsCanMove, true);
   return editClipsCanMove;
}

void InitPreferences( const wxFileName &configFileName )
{
   wxString appName = wxTheApp->GetAppName();

   ugPrefs = std::make_unique<AudacityPrefs>
      (appName, wxEmptyString,
       configFileName.GetFullPath(),
       wxEmptyString, wxCONFIG_USE_LOCAL_FILE);
   gPrefs = ugPrefs.get();

   wxConfigBase::Set(gPrefs);
}

bool CheckWritablePreferences()
{
   return gPrefs->Write("/TEST", true) && gPrefs->Flush();
}

wxString UnwritablePreferencesErrorMessage( const wxFileName &configFileName )
{
   return wxString::Format(
     _("Audacity cannot start because the settings file at %s is not writable."),
     configFileName.GetFullPath()
   );
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      ugPrefs.reset();
      gPrefs = NULL;
   }
}

//////////
wxString ChoiceSetting::Read() const
{
   const auto &defaultValue = Default().Internal();
   wxString value;
   if ( !gPrefs->Read(mKey, &value, defaultValue) )
      if (!mMigrated) {
         const_cast<ChoiceSetting*>(this)->Migrate( value );
         mMigrated = true;
      }

   // Remap to default if the string is not known -- this avoids surprises
   // in case we try to interpret config files from future versions
   auto index = Find( value );
   if ( index >= mSymbols.size() )
      value = defaultValue;
   return value;
}

size_t ChoiceSetting::Find( const wxString &value ) const
{
   auto start = begin();
   return size_t(
      std::find( start, end(), EnumValueSymbol{ value, {} } )
         - start );
}

void ChoiceSetting::Migrate( wxString &value )
{
   (void)value;// Compiler food
}

bool ChoiceSetting::Write( const wxString &value )
{
   auto index = Find( value );
   if (index >= mSymbols.size())
      return false;

   auto result = gPrefs->Write( mKey, value );
   mMigrated = true;
   return result;
}

EnumSetting::EnumSetting(
   const wxString &key,
   EnumValueSymbols symbols,
   size_t defaultSymbol,

   std::vector<int> intValues, // must have same size as symbols
   const wxString &oldKey
)
   : ChoiceSetting{ key, std::move( symbols ), defaultSymbol }
   , mIntValues{ std::move( intValues ) }
   , mOldKey{ oldKey }
{
   auto size = mSymbols.size();
   if( mIntValues.size() != size ) {
      wxASSERT( false );
      mIntValues.resize( size );
   }
}

int EnumSetting::ReadInt() const
{
   auto index = Find( Read() );
   wxASSERT( index < mIntValues.size() );
   return mIntValues[ index ];
}

size_t EnumSetting::FindInt( int code ) const
{
   const auto start = mIntValues.begin();
   return size_t(
      std::find( start, mIntValues.end(), code )
         - start );
}

void EnumSetting::Migrate( wxString &value )
{
   int intValue = 0;
   if ( !mOldKey.empty() &&
        gPrefs->Read(mOldKey, &intValue, 0) ) {
      // Make the migration, only once and persistently.
      // Do not DELETE the old key -- let that be read if user downgrades
      // Audacity.  But further changes will be stored only to the NEW key
      // and won't be seen then.
      auto index = FindInt( intValue );
      if ( index >= mSymbols.size() )
         index = mDefaultSymbol;
      value = mSymbols[index].Internal();
      Write(value);
      gPrefs->Flush();
   }
}

bool EnumSetting::WriteInt( int code ) // you flush gPrefs afterward
{
   auto index = FindInt( code );
   if ( index >= mSymbols.size() )
      return false;
   return Write( mSymbols[index].Internal() );
}

wxString WarningDialogKey(const wxString &internalDialogName)
{
   return wxT("/Warnings/") + internalDialogName;
}
