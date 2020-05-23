/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileIO.h"

#include <wx/frame.h>

#include "AutoRecovery.h"
#include "DirManager.h"
#include "FileNames.h"
#include "Project.h"
#include "ProjectFileIORegistry.h"
#include "ProjectSettings.h"
#include "Tags.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/NumericTextCtrl.h"

wxDEFINE_EVENT(EVT_PROJECT_TITLE_CHANGE, wxCommandEvent);

static void RefreshAllTitles(bool bShowProjectNumbers )
{
   for ( auto pProject : AllProjects{} ) {
      if ( !GetProjectFrame( *pProject ).IsIconized() ) {
         ProjectFileIO::Get( *pProject ).SetProjectTitle(
            bShowProjectNumbers ? pProject->GetProjectNumber() : -1 );
      }
   }
}

TitleRestorer::TitleRestorer(
   wxTopLevelWindow &window, AudacityProject &project )
{
   if( window.IsIconized() )
      window.Restore();
   window.Raise(); // May help identifying the window on Mac

   // Construct this project's name and number.
   sProjName = project.GetProjectName();
   if ( sProjName.empty() ) {
      sProjName = _("<untitled>");
      UnnamedCount = std::count_if(
         AllProjects{}.begin(), AllProjects{}.end(),
         []( const AllProjects::value_type &ptr ){
            return ptr->GetProjectName().empty();
         }
      );
      if ( UnnamedCount > 1 ) {
         sProjNumber.Printf(
            _("[Project %02i] "), project.GetProjectNumber() + 1 );
         RefreshAllTitles( true );
      } 
   }
   else
      UnnamedCount = 0;
}

TitleRestorer::~TitleRestorer() {
   if( UnnamedCount > 1 )
      RefreshAllTitles( false );
}

static const AudacityProject::AttachedObjects::RegisteredFactory sFileIOKey{
   []( AudacityProject &parent ){
      auto result = std::make_shared< ProjectFileIO >( parent );
      return result;
   }
};

ProjectFileIO &ProjectFileIO::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectFileIO >( sFileIOKey );
}

const ProjectFileIO &ProjectFileIO::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

// PRL: I preserve this handler function for an event that was never sent, but
// I don't know the intention.

ProjectFileIO::ProjectFileIO( AudacityProject &project )
   : mProject{ project }
{
   UpdatePrefs();
}

ProjectFileIO::~ProjectFileIO() = default;

void ProjectFileIO::UpdatePrefs()
{
   SetProjectTitle();
}

// Pass a number in to show project number, or -1 not to.
void ProjectFileIO::SetProjectTitle( int number)
{
   auto &project = mProject;
   auto pWindow = project.GetFrame();
   if ( !pWindow )
      return;
   auto &window = *pWindow;
   wxString name = project.GetProjectName();

   // If we are showing project numbers, then we also explicitly show "<untitled>" if there
   // is none.
   if( number >= 0 ){
      /* i18n-hint: The %02i is the project number, the %s is the project name.*/
      name = wxString::Format( _("[Project %02i] Audacity \"%s\""), number+1 ,
         name.empty() ? "<untitled>" : (const char *)name );
   }
   // If we are not showing numbers, then <untitled> shows as 'Audacity'.
   else if( name.empty() )
   {
      SetLoadedFromAup( false );
      name = _TS("Audacity");
   }

   if ( IsRecovered() )
   {
      name += wxT(" ");
      /* i18n-hint: E.g this is recovered audio that had been lost.*/
      name += _("(Recovered)");
   }

   if ( name != window.GetTitle() ) {
      window.SetTitle( name );
      window.SetName(name);       // to make the nvda screen reader read the correct title

      project.QueueEvent(
         safenew wxCommandEvent{ EVT_PROJECT_TITLE_CHANGE } );
   }
}

// Most of this string was duplicated 3 places. Made the warning consistent in this global.
// The %s is to be filled with the version string.
// PRL:  Do not statically allocate a string in _() !
static TranslatableString gsLegacyFileWarning() { return
XO("This file was saved by Audacity version %s. The format has changed. \
\n\nAudacity can try to open and save this file, but saving it in this \
\nversion will then prevent any 1.2 or earlier version opening it. \
\n\nAudacity might corrupt the file in opening it, so you should \
back it up first. \
\n\nOpen this file now?");
}

bool ProjectFileIO::WarnOfLegacyFile( )
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   auto msg = gsLegacyFileWarning().Format( XO("1.0 or earlier") );

   // Stop icon, and choose 'NO' by default.
   int action =
      AudacityMessageBox(
         msg,
         XO("Warning - Opening Old Project File"),
         wxYES_NO | wxICON_STOP | wxNO_DEFAULT | wxCENTRE,
         &window);
   return (action != wxNO);
}

bool ProjectFileIO::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &dirManager = DirManager::Get( project );
   auto &settings = ProjectSettings::Get( project );
   bool bFileVersionFound = false;
   wxString fileVersion;
   wxString audacityVersion = _("<unrecognized version -- possibly corrupt project file>");
   int requiredTags = 0;
   long longVpos = 0;

   // The auto-save data dir the project has been recovered from
   FilePath recoveryAutoSaveDataDir;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value || !XMLValueChecker::IsGoodString(value))
         break;

      if (viewInfo.ReadXMLAttribute(attr, value)) {
         // We need to save vpos now and restore it below
         longVpos = std::max(longVpos, long(viewInfo.vpos));
         continue;
      }

      if (!wxStrcmp(attr, wxT("datadir")))
      {
         //
         // This is an auto-saved version whose data is in another directory
         //
         // Note: This attribute must currently be written and parsed before
         //       any other attributes
         //
         if ((value[0] != 0) && XMLValueChecker::IsGoodPathString(value))
         {
            // Remember that this is a recovered project
            mIsRecovered = true;
            recoveryAutoSaveDataDir = value;
         }
      }

      else if (!wxStrcmp(attr, wxT("version")))
      {
         fileVersion = value;
         bFileVersionFound = true;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("audacityversion"))) {
         audacityVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("projname"))) {
         FilePath projName;
         FilePath projPath;

         if (mIsRecovered) {
            // Fake the filename as if we had opened the original file
            // (which was lost by the crash) rather than the one in the
            // auto save folder
            wxFileName realFileDir;
            realFileDir.AssignDir(recoveryAutoSaveDataDir);
            realFileDir.RemoveLastDir();

            wxString realFileName = value;
            if (realFileName.length() >= 5 &&
                realFileName.Right(5) == wxT("_data"))
            {
               realFileName = realFileName.Left(realFileName.length() - 5);
            }

            if (realFileName.empty())
            {
               // A previously unsaved project has been recovered, so fake
               // an unsaved project. The data files just stay in the temp
               // directory
               dirManager.SetLocalTempDir(recoveryAutoSaveDataDir);
               project.SetFileName( wxT("") );
               projName = wxT("");
               projPath = wxT("");
            }
            else
            {
               realFileName += wxT(".aup");
               projPath = realFileDir.GetFullPath();
               project.SetFileName(
                  wxFileName{ projPath, realFileName }.GetFullPath() );
               mbLoadedFromAup = true;
               projName = value;
            }

            SetProjectTitle();
         }
         else {
            projName = value;
            projPath = wxPathOnly( project.GetFileName() );
         }

         if (!projName.empty())
         {
            // First try to load the data files based on the _data dir given in the .aup file
            // If this fails then try to use the filename of the .aup as the base directory
            // This is because unzipped projects e.g. those that get transfered between mac-pc
            // have encoding issues and end up expanding the wrong filenames for certain
            // international characters (such as capital 'A' with an umlaut.)
            if (!dirManager.SetProject(projPath, projName, false))
            {
               projName = project.GetProjectName() + wxT("_data");
               if (!dirManager.SetProject(projPath, projName, false)) {
                  AudacityMessageBox(
                     XO("Couldn't find the project data folder: \"%s\"")
                        .Format( projName ),
                     XO("Error Opening Project"),
                     wxOK | wxCENTRE,
                     &window);
                  return false;
               }
            }
         }

         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("rate"))) {
         double rate;
         Internat::CompatibleToDouble(value, &rate);
         settings.SetRate( rate );
      }

      else if (!wxStrcmp(attr, wxT("snapto"))) {
         settings.SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      else if (!wxStrcmp(attr, wxT("selectionformat")))
         settings.SetSelectionFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );

      else if (!wxStrcmp(attr, wxT("audiotimeformat")))
         settings.SetAudioTimeFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );

      else if (!wxStrcmp(attr, wxT("frequencyformat")))
         settings.SetFrequencySelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::FREQUENCY, value ) );

      else if (!wxStrcmp(attr, wxT("bandwidthformat")))
         settings.SetBandwidthSelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::BANDWIDTH, value ) );
   } // while

   if (longVpos != 0) {
      // PRL: It seems this must happen after SetSnapTo
       viewInfo.vpos = longVpos;
   }

   // Specifically detect newer versions of Audacity
   // WARNING: This will need review/revision if we ever have a version string
   // such as 1.5.10, i.e. with 2 digit numbers.
   // We're able to do a shortcut and use string comparison because we know
   // that does not happen.
   // TODO: Um.  We actually have released 0.98 and 1.3.14 so the comment
   // above is inaccurate.

   if (!bFileVersionFound ||
         (fileVersion.length() != 5) || // expecting '1.1.0', for example
         // JKC: I commented out next line.  IsGoodInt is not for
         // checking dotted numbers.
         //!XMLValueChecker::IsGoodInt(fileVersion) ||
         (fileVersion > wxT(AUDACITY_FILE_FORMAT_VERSION)))
   {
      /* i18n-hint: %s will be replaced by the version number.*/
      auto msg = XO("This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file.")
         .Format(audacityVersion,
                 AUDACITY_VERSION_STRING);
      AudacityMessageBox(
         msg,
         XO("Can't open project file"),
         wxOK | wxICON_EXCLAMATION | wxCENTRE,
         &window);
      return false;
   }

   // NOTE: It looks as if there was some confusion about fileversion and audacityversion.
   // fileversion NOT being increased when file formats changed, so unfortunately we're
   // using audacityversion to rescue the situation.

   // KLUDGE: guess the true 'fileversion' by stripping away any '-beta-Rc' stuff on
   // audacityVersion.
   // It's fairly safe to do this as it has already been established that the
   // supported file version was five chars long.
   fileVersion = audacityVersion.Mid(0,5);

   bool bIsOld = fileVersion < wxT(AUDACITY_FILE_FORMAT_VERSION);
   bool bIsVeryOld = fileVersion < wxT("1.1.9" );
   // Very old file versions could even have the file version starting
   // with text: 'AudacityProject Version 0.95'
   // Atoi return zero in this case.
   bIsVeryOld |= wxAtoi( fileVersion )==0;
   // Specifically detect older versions of Audacity
   if ( bIsOld | bIsVeryOld ) {
      auto msg = gsLegacyFileWarning().Format( audacityVersion );

      int icon_choice = wxICON_EXCLAMATION;
      if( bIsVeryOld )
         // Stop icon, and choose 'NO' by default.
         icon_choice = wxICON_STOP | wxNO_DEFAULT;
      int action = AudacityMessageBox(
         msg,
         XO("Warning - Opening Old Project File"),
         wxYES_NO | icon_choice | wxCENTRE,
         &window);
      if (action == wxNO)
         return false;
   }

   if (wxStrcmp(tag, wxT("audacityproject")) &&
       wxStrcmp(tag, wxT("project"))) {
      // If the tag name is not one of these two (the NEW name is
      // "project" with an Audacity namespace, but we don't detect
      // the namespace yet), then we don't know what the error is
      return false;
   }

   if (requiredTags < 3)
      return false;

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *ProjectFileIO::HandleXMLChild(const wxChar *tag)
{
   auto &project = mProject;
   auto fn = ProjectFileIORegistry::Lookup( tag );
   if (fn)
      return fn( project );

   return nullptr;
}

void ProjectFileIO::WriteXMLHeader(XMLWriter &xmlFile) const
{
   xmlFile.Write(wxT("<?xml "));
   xmlFile.Write(wxT("version=\"1.0\" "));
   xmlFile.Write(wxT("standalone=\"no\" "));
   xmlFile.Write(wxT("?>\n"));

   xmlFile.Write(wxT("<!DOCTYPE "));
   xmlFile.Write(wxT("project "));
   xmlFile.Write(wxT("PUBLIC "));
   xmlFile.Write(wxT("\"-//audacityproject-1.3.0//DTD//EN\" "));
   xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd\" "));
   xmlFile.Write(wxT(">\n"));
}

void ProjectFileIO::WriteXML(
   XMLWriter &xmlFile, FilePaths *strOtherNamesArray)
// may throw
{
   auto &proj = mProject;
   auto &tracks = TrackList::Get( proj );
   auto &viewInfo = ViewInfo::Get( proj );
   auto &dirManager = DirManager::Get( proj );
   auto &tags = Tags::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

   bool bWantSaveCopy = (strOtherNamesArray != nullptr);

   //TIMER_START( "AudacityProject::WriteXML", xml_writer_timer );
   // Warning: This block of code is duplicated in Save, for now...
   wxFileName project { proj.GetFileName() };
   if (project.GetExt() == wxT("aup"))
      project.SetExt( {} );
   auto projName = project.GetFullName() + wxT("_data");
   // End Warning -DMM

   xmlFile.StartTag(wxT("project"));
   xmlFile.WriteAttr(wxT("xmlns"), wxT("http://audacity.sourceforge.net/xml/"));

   if (mAutoSaving)
   {
      //
      // When auto-saving, remember full path to data files directory
      //
      // Note: This attribute must currently be written and parsed before
      //       all other attributes
      //
      xmlFile.WriteAttr(wxT("datadir"), dirManager.GetDataFilesDir());

      // Note that the code at the start assumes that if mFileName has a value
      // then the file has been saved.  This is not necessarily true when
      // autosaving as it gets set by AddImportedTracks (presumably as a proposal).
      // I don't think that mDirManager.projName gets set without a save so check that.
      if( !IsProjectSaved() )
         projName = wxT("_data");
   }

   xmlFile.WriteAttr(wxT("projname"), projName);
   xmlFile.WriteAttr(wxT("version"), wxT(AUDACITY_FILE_FORMAT_VERSION));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   viewInfo.WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), settings.GetRate());
   xmlFile.WriteAttr(wxT("snapto"), settings.GetSnapTo() ? wxT("on") : wxT("off"));
   xmlFile.WriteAttr(wxT("selectionformat"),
                     settings.GetSelectionFormat().Internal());
   xmlFile.WriteAttr(wxT("frequencyformat"),
                     settings.GetFrequencySelectionFormatName().Internal());
   xmlFile.WriteAttr(wxT("bandwidthformat"),
                     settings.GetBandwidthSelectionFormatName().Internal());

   tags.WriteXML(xmlFile);

   unsigned int ndx = 0;
   tracks.Any().Visit(
      [&](WaveTrack *pWaveTrack) {
         if (bWantSaveCopy) {
            if (!pWaveTrack->IsLeader())
               return;

            //vvv This should probably be a method, WaveTrack::WriteCompressedTrackXML().
            xmlFile.StartTag(wxT("import"));
            xmlFile.WriteAttr(wxT("filename"),
               (*strOtherNamesArray)[ndx]); // Assumes mTracks order hasn't changed!

            // Don't store "channel" and "linked" tags because the importer can figure that out,
            // e.g., from stereo Ogg files.
            //    xmlFile.WriteAttr(wxT("channel"), t->GetChannel());
            //    xmlFile.WriteAttr(wxT("linked"), t->GetLinked());

            const auto offset =
               TrackList::Channels( pWaveTrack ).min( &WaveTrack::GetOffset );
            xmlFile.WriteAttr(wxT("offset"), offset, 8);
            xmlFile.WriteAttr(wxT("mute"), pWaveTrack->GetMute());
            xmlFile.WriteAttr(wxT("solo"), pWaveTrack->GetSolo());
            pWaveTrack->Track::WriteCommonXMLAttributes( xmlFile, false );

            // Don't store "rate" tag because the importer can figure that out.
            //    xmlFile.WriteAttr(wxT("rate"), pWaveTrack->GetRate());
            xmlFile.WriteAttr(wxT("gain"), (double)pWaveTrack->GetGain());
            xmlFile.WriteAttr(wxT("pan"), (double)pWaveTrack->GetPan());
            xmlFile.EndTag(wxT("import"));

            ndx++;
         }
         else {
            pWaveTrack->SetAutoSaveIdent(mAutoSaving ? ++ndx : 0);
            pWaveTrack->WriteXML(xmlFile);
         }
      },
      [&](Track *t) {
         t->WriteXML(xmlFile);
      }
   );

   if (!mAutoSaving)
   {
      // Only write closing bracket when not auto-saving, since we may add
      // recording log data to the end of the file later
      xmlFile.EndTag(wxT("project"));
   }
   //TIMER_STOP( xml_writer_timer );

}

static wxString CreateUniqueName()
{
   static int count = 0;
   return wxDateTime::Now().Format(wxT("%Y-%m-%d %H-%M-%S")) +
          wxString::Format(wxT(" N-%i"), ++count);
}

//
// This small template class resembles a try-finally block
//
// It sets var to val_entry in the constructor and
// var to val_exit in the destructor.
//
template <typename T>
class VarSetter
{
public:
   VarSetter(T* var, T val_entry, T val_exit)
   {
      mVar = var;
      mValExit = val_exit;
      *var = val_entry;
   }

   ~VarSetter()
   {
      *mVar = mValExit;
   }
private:
   T* mVar;
   T mValExit;
};

void ProjectFileIO::AutoSave()
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   //    SonifyBeginAutoSave(); // part of RBD's r10680 stuff now backed out

   // To minimize the possibility of race conditions, we first write to a
   // file with the extension ".tmp", then rename the file to .autosave
   wxString projName;

   auto fileName = project.GetFileName();
   if (fileName.empty())
      projName = wxT("New Project");
   else
      projName = wxFileName{ fileName }.GetName();

   wxString fn = wxFileName(FileNames::AutoSaveDir(),
      projName + wxString(wxT(" - ")) + CreateUniqueName()).GetFullPath();

   // PRL:  I found a try-catch and rewrote it,
   // but this guard is unnecessary because AutoSaveFile does not throw
   bool success = GuardedCall< bool >( [&]
   {
      VarSetter<bool> setter(&mAutoSaving, true, false);

      AutoSaveFile buffer;
      WriteXMLHeader( buffer );
      WriteXML( buffer, nullptr );

      wxFFile saveFile;
      saveFile.Open(fn + wxT(".tmp"), wxT("wb"));
      return buffer.Write(saveFile);
   } );

   if (!success)
      return;

   // Now that we have a NEW auto-save file, DELETE the old one
   DeleteCurrentAutoSaveFile();

   if (!mAutoSaveFileName.empty())
      return; // could not remove auto-save file

   if (!wxRenameFile(fn + wxT(".tmp"), fn + wxT(".autosave")))
   {
      AudacityMessageBox(
         XO("Could not create autosave file: %s")
            .Format( fn + wxT(".autosave") ),
         XO("Error"),
         wxICON_STOP,
         &window);
      return;
   }

   mAutoSaveFileName += fn + wxT(".autosave");
   // no-op cruft that's not #ifdefed for NoteTrack
   // See above for further comments.
   //   SonifyEndAutoSave();
}

void ProjectFileIO::DeleteCurrentAutoSaveFile()
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   if (!mAutoSaveFileName.empty())
   {
      if (wxFileExists(mAutoSaveFileName))
      {
         if (!wxRemoveFile(mAutoSaveFileName))
         {
            AudacityMessageBox(
               XO("Could not remove old autosave file: %s")
                  .Format( mAutoSaveFileName ),
               XO("Error"),
               wxICON_STOP,
               &window);
            return;
         }
      }

      mAutoSaveFileName = wxT("");
   }
}

bool ProjectFileIO::IsProjectSaved() const {
   auto &project = mProject;
   auto &dirManager = DirManager::Get( project );
   // This is true if a project was opened from an .aup
   // Otherwise it becomes true only when a project is first saved successfully
   // in DirManager::SetProject
   return (!dirManager.GetProjectName().empty());
}

void ProjectFileIO::Reset()
{
   mProject.SetFileName( {} );
   mIsRecovered = false;
   mbLoadedFromAup = false;
   SetProjectTitle();
}
