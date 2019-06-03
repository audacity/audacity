/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileIO.h"

#include <wx/crt.h>

#include "AutoRecovery.h"
#include "Dependencies.h"
#include "DirManager.h"
#include "FileNames.h"
#include "Project.h"
#include "ProjectFileIORegistry.h"
#include "ProjectSettings.h"
#include "SelectionState.h"
#include "Tags.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "export/Export.h"
#include "ondemand/ODComputeSummaryTask.h"
#include "ondemand/ODManager.h"
#include "ondemand/ODTask.h"
#include "toolbars/SelectionBar.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ErrorDialog.h"
#include "widgets/FileHistory.h"
#include "widgets/NumericTextCtrl.h"
#include "widgets/Warning.h"
#include "xml/XMLFileReader.h"
#include "xml/XMLWriter.h"

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
   auto &window = GetProjectFrame( project );
   wxString name = project.GetProjectName();

   // If we are showing project numbers, then we also explicitly show "<untitled>" if there
   // is none.
   if( number >= 0 ){
      /* i18n-hint: The %02i is the project number, the %s is the project name.*/
      name = wxString::Format( _TS("[Project %02i] Audacity \"%s\""), number+1 ,
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

   window.SetTitle( name );
   window.SetName(name);       // to make the nvda screen reader read the correct title
}

// Most of this string was duplicated 3 places. Made the warning consistent in this global.
// The %s is to be filled with the version string.
// PRL:  Do not statically allocate a string in _() !
static wxString gsLegacyFileWarning() { return
_("This file was saved by Audacity version %s. The format has changed. \
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
   wxString msg;
   msg.Printf(gsLegacyFileWarning(), _("1.0 or earlier"));

   // Stop icon, and choose 'NO' by default.
   int action =
      AudacityMessageBox(msg,
                   _("Warning - Opening Old Project File"),
                   wxYES_NO | wxICON_STOP | wxNO_DEFAULT | wxCENTRE,
                   &window);
   return (action != wxNO);
}

auto ProjectFileIO::ReadProjectFile( const FilePath &fileName )
  -> ReadProjectResults
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );

   project.SetFileName( fileName );
   mbLoadedFromAup = true;

   mRecoveryAutoSaveDataDir = wxT("");
   mIsRecovered = false;

   SetProjectTitle();

   const wxString autoSaveExt = wxT("autosave");
   if ( wxFileNameWrapper{ fileName }.GetExt() == autoSaveExt )
   {
      AutoSaveFile asf;
      if (!asf.Decode(fileName))
      {
         auto message = AutoSaveFile::FailureMessage( fileName );
         AudacityMessageBox(
            message,
            _("Error decoding file"),
            wxOK | wxCENTRE, &window );
         // Important: Prevent deleting any temporary files!
         DirManager::SetDontDeleteTempFiles();
         return { true };
      }
   }

   ///
   /// Parse project file
   ///

   XMLFileReader xmlFile;

   // 'Lossless copy' projects have dependencies. We need to always copy-in
   // these dependencies when converting to a normal project.
   wxString oldAction =
      gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("copy"));
   bool oldAsk =
      gPrefs->ReadBool(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), true);
   if (oldAction != wxT("copy"))
      gPrefs->Write(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("copy"));
   if (oldAsk)
      gPrefs->Write(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), (long) false);
   gPrefs->Flush();

   auto cleanup = finally( [&] {
      // and restore old settings if necessary.
      if (oldAction != wxT("copy"))
         gPrefs->Write(wxT("/FileFormats/CopyOrEditUncompressedData"), oldAction);
      if (oldAsk)
         gPrefs->Write(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), (long) true);
      gPrefs->Flush();
   } );

   bool bParseSuccess = xmlFile.Parse(this, fileName);
   
   bool err = false;

   if (bParseSuccess) {
      // By making a duplicate set of pointers to the existing blocks
      // on disk, we add one to their reference count, guaranteeing
      // that their reference counts will never reach zero and thus
      // the version saved on disk will be preserved until the
      // user selects Save().

      mLastSavedTracks = TrackList::Create();

      auto &tracks = TrackList::Get( project );
      for (auto t : tracks.Any()) {
         if (t->GetErrorOpening())
         {
            wxLogWarning(
               wxT("Track %s had error reading clip values from project file."),
               t->GetName());
            err = true;
         }

         err = ( !t->LinkConsistencyCheck() ) || err;

         mLastSavedTracks->Add(t->Duplicate());
      }
   }

   return { false, bParseSuccess, err, xmlFile.GetErrorStr() };
}

void ProjectFileIO::EnqueueODTasks()
{
   //check the ODManager to see if we should add the tracks to the ODManager.
   //this flag would have been set in the HandleXML calls from above, if there were
   //OD***Blocks.
   if(ODManager::HasLoadedODFlag())
   {
      auto &project = mProject;
      auto &tracks = TrackList::Get( project );

      std::vector<std::unique_ptr<ODTask>> newTasks;
      //std::vector<ODDecodeTask*> decodeTasks;
      unsigned int createdODTasks=0;
      for (auto wt : tracks.Any<WaveTrack>()) {
         //check the track for blocks that need decoding.
         //There may be more than one type e.g. FLAC/FFMPEG/lame
         unsigned int odFlags = wt->GetODFlags();

         //add the track to the already created tasks that correspond to the od flags in the wavetrack.
         for(unsigned int i=0;i<newTasks.size();i++) {
            if(newTasks[i]->GetODType() & odFlags)
               newTasks[i]->AddWaveTrack(wt);
         }

         //create whatever NEW tasks we need to.
         //we want at most one instance of each class for the project
         while((odFlags|createdODTasks) != createdODTasks)
         {
            std::unique_ptr<ODTask> newTask;
#ifdef EXPERIMENTAL_OD_FLAC
            if(!(createdODTasks&ODTask::eODFLAC) && (odFlags & ODTask::eODFLAC)) {
               newTask = std::make_unique<ODDecodeFlacTask>();
               createdODTasks = createdODTasks | ODTask::eODFLAC;
            }
            else
#endif
            if(!(createdODTasks&ODTask::eODPCMSummary) && (odFlags & ODTask::eODPCMSummary)) {
               newTask = std::make_unique<ODComputeSummaryTask>();
               createdODTasks = createdODTasks | ODTask::eODPCMSummary;
            }
            else {
               wxPrintf("unrecognized OD Flag in block file.\n");
               //TODO:ODTODO: display to user.  This can happen when we build audacity on a system that doesnt have libFLAC
               break;
            }
            if(newTask)
            {
               newTask->AddWaveTrack(wt);
               newTasks.push_back(std::move(newTask));
            }
         }
      }
      for(unsigned int i=0;i<newTasks.size();i++)
         ODManager::Instance()->AddNewTask(std::move(newTasks[i]));
   }
}

bool ProjectFileIO::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto &project = mProject;
   auto &window = ProjectWindow::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &dirManager = DirManager::Get( project );
   auto &settings = ProjectSettings::Get( project );
   bool bFileVersionFound = false;
   wxString fileVersion = _("<unrecognized version -- possibly corrupt project file>");
   wxString audacityVersion = _("<unrecognized version -- possibly corrupt project file>");
   int requiredTags = 0;
   long longVpos = 0;

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
            mRecoveryAutoSaveDataDir = value;
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
            realFileDir.AssignDir(mRecoveryAutoSaveDataDir);
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
               dirManager.SetLocalTempDir(mRecoveryAutoSaveDataDir);
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
                  AudacityMessageBox(wxString::Format(_("Couldn't find the project data folder: \"%s\""),
                                             projName),
                                             _("Error Opening Project"),
                                             wxOK | wxCENTRE, &window);
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
         SelectionBar::Get( project ).SetRate( rate );
      }

      else if (!wxStrcmp(attr, wxT("snapto"))) {
         settings.SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      else if (!wxStrcmp(attr, wxT("selectionformat")))
         settings.SetSelectionFormat(
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
       window.mbInitializingScrollbar = true;
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
         // JKC: I commentted out next line.  IsGoodInt is not for
         // checking dotted numbers.
         //!XMLValueChecker::IsGoodInt(fileVersion) ||
         (fileVersion > wxT(AUDACITY_FILE_FORMAT_VERSION)))
   {
      wxString msg;
      /* i18n-hint: %s will be replaced by the version number.*/
      msg.Printf(_("This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file."),
                 audacityVersion,
                 AUDACITY_VERSION_STRING);
      AudacityMessageBox(msg,
                   _("Can't open project file"),
                   wxOK | wxICON_EXCLAMATION | wxCENTRE, &window);
      return false;
   }

   // NOTE: It looks as if there was some confusion about fileversion and audacityversion.
   // fileversion NOT being increased when file formats changed, so unfortunately we're
   // using audacityversion to rescue the situation.

   // KLUDGE: guess the true 'fileversion' by stripping away any '-beta-Rc' stuff on
   // audacityVersion.
   // It's fairly safe to do this as it has already been established that the
   // puported file version was five chars long.
   fileVersion = audacityVersion.Mid(0,5);

   bool bIsOld = fileVersion < wxT(AUDACITY_FILE_FORMAT_VERSION);
   bool bIsVeryOld = fileVersion < wxT("1.1.9" );
   // Very old file versions could even have the file version starting
   // with text: 'AudacityProject Version 0.95'
   // Atoi return zero in this case.
   bIsVeryOld |= wxAtoi( fileVersion )==0;
   // Specifically detect older versions of Audacity
   if ( bIsOld | bIsVeryOld ) {
      wxString msg;
      msg.Printf(gsLegacyFileWarning(), audacityVersion);

      int icon_choice = wxICON_EXCLAMATION;
      if( bIsVeryOld )
         // Stop icon, and choose 'NO' by default.
         icon_choice = wxICON_STOP | wxNO_DEFAULT;
      int action =
         AudacityMessageBox(msg,
                      _("Warning - Opening Old Project File"),
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

void ProjectFileIO::WriteXML(XMLWriter &xmlFile, bool bWantSaveCopy)
// may throw
{
   auto &proj = mProject;
   auto &tracks = TrackList::Get( proj );
   auto &viewInfo = ViewInfo::Get( proj );
   auto &dirManager = DirManager::Get( proj );
   auto &tags = Tags::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

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
      // then the file has been saved.  This is not neccessarily true when
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
            xmlFile.WriteAttr(wxT("filename"), mStrOtherNamesArray[ndx]); // Assumes mTracks order hasn't changed!

            // Don't store "channel" and "linked" tags because the importer can figure that out,
            // e.g., from stereo Ogg files.
            //    xmlFile.WriteAttr(wxT("channel"), t->GetChannel());
            //    xmlFile.WriteAttr(wxT("linked"), t->GetLinked());

            const auto offset =
               TrackList::Channels( pWaveTrack ).min( &WaveTrack::GetOffset );
            xmlFile.WriteAttr(wxT("offset"), offset, 8);
            xmlFile.WriteAttr(wxT("mute"), pWaveTrack->GetMute());
            xmlFile.WriteAttr(wxT("solo"), pWaveTrack->GetSolo());
            xmlFile.WriteAttr(wxT("height"), pWaveTrack->GetActualHeight());
            xmlFile.WriteAttr(wxT("minimized"), pWaveTrack->GetMinimized());

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

#if 0
// I added this to "fix" bug #334.  At that time, we were on wxWidgets 2.8.12 and
// there was a window between the closing of the "Save" progress dialog and the
// end of the actual save where the user was able to close the project window and
// recursively enter the Save code (where they could inadvertently cause the issue
// described in #334).
//
// When we converted to wx3, this "disabler" caused focus problems when returning
// to the project after the save (bug #1172) because the focus and activate events
// weren't being dispatched and the focus would get lost.
//
// After some testing, it looks like the window described above no longer exists,
// so I've disabled the disabler.  However, I'm leaving it here in case we run
// into the problem in the future.  (even though it can't be used as-is)
class ProjectDisabler
{
public:
   ProjectDisabler(wxWindow *w)
   :  mWindow(w)
   {
      mWindow->GetEventHandler()->SetEvtHandlerEnabled(false);
   }
   ~ProjectDisabler()
   {
      mWindow->GetEventHandler()->SetEvtHandlerEnabled(true);
   }
private:
   wxWindow *mWindow;
};
#endif

bool ProjectFileIO::Save()
{
   // Prompt for file name?
   bool bPromptingRequired = !IsProjectSaved();

   if (bPromptingRequired)
      return SaveAs();

   return DoSave(false, false);
}

// Assumes AudacityProject::mFileName has been set to the desired path.
bool ProjectFileIO::DoSave (const bool fromSaveAs,
                              const bool bWantSaveCopy,
                              const bool bLossless /*= false*/)
{
   // See explanation above
   // ProjectDisabler disabler(this);
   auto &proj = mProject;
   const auto &fileName = proj.GetFileName();
   auto &window = GetProjectFrame( proj );
   auto &dirManager = DirManager::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

   wxASSERT_MSG(!bWantSaveCopy || fromSaveAs, "Copy Project SHOULD only be availabele from SaveAs");

   // Some confirmation dialogs
   if (!bWantSaveCopy)
   {
      auto &tracks = TrackList::Get( proj );
      if ( ! tracks.Any() )
      {
         if ( UndoManager::Get( proj ).UnsavedChanges()
         && settings.EmptyCanBeDirty()) {
            int result = AudacityMessageBox(_("Your project is now empty.\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nClick 'No', Edit > Undo until all tracks\nare open, then File > Save Project.\n\nSave anyway?"),
                                      _("Warning - Empty Project"),
                                      wxYES_NO | wxICON_QUESTION, &window);
            if (result == wxNO)
               return false;
         }
      }

      // If the user has recently imported dependencies, show
      // a dialog where the user can see audio files that are
      // aliased by this project.  The user may make the project
      // self-contained during this dialog, it modifies the project!
      if (mImportedDependencies)
      {
         bool bSuccess = ShowDependencyDialogIfNeeded(&proj, true);
         if (!bSuccess)
            return false;
         mImportedDependencies = false; // do not show again
      }
   }
   // End of confirmations

   //
   // Always save a backup of the original project file
   //

   wxString safetyFileName;
   if (wxFileExists(fileName)) {

#ifdef __WXGTK__
      safetyFileName = fileName + wxT("~");
#else
      safetyFileName = fileName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      if ( !wxRenameFile(fileName, safetyFileName) ) {
         AudacityMessageBox(
            wxString::Format(
               _("Could not create safety file: %s"), safetyFileName ),
            _("Error"), wxICON_STOP, &window);
         return false;
      }
   }

   bool success = true;
   FilePath project, projName, projPath;

   auto cleanup = finally( [&] {
      if (!safetyFileName.empty()) {
         if (wxFileExists(fileName))
            wxRemove(fileName);
         wxRename(safetyFileName, fileName);
      }

      // mStrOtherNamesArray is a temporary array of file names, used only when
      // saving compressed
      if (!success) {
         AudacityMessageBox(wxString::Format(_("Could not save project. Perhaps %s \nis not writable or the disk is full."),
                                       project),
                      _("Error Saving Project"),
                      wxICON_ERROR, &window);

         // Make the export of tracks succeed all-or-none.
         auto dir = project + wxT("_data");
         for ( auto &name : mStrOtherNamesArray )
            wxRemoveFile( dir + wxFileName::GetPathSeparator() + name);
         // This has effect only if the folder is empty
         wxFileName::Rmdir( dir );
      }
      // Success or no, we can forget the names
      mStrOtherNamesArray.clear();
   } );

   if (fromSaveAs) {
      // This block of code is duplicated in WriteXML, for now...
      project = fileName;
      wxFileName projFName{ fileName };
      if (projFName.GetExt() == wxT("aup"))
         projFName.SetExt( {} ), project = projFName.GetFullPath();
      projName = wxFileNameFromPath(project) + wxT("_data");
      projPath = wxPathOnly(project);

      if( !wxDir::Exists( projPath ) ){
         AudacityMessageBox(wxString::Format(
            _("Could not save project. Path not found. Try creating \ndirectory \"%s\" before saving project with this name."),
            projPath),
                      _("Error Saving Project"),
                      wxICON_ERROR, &window);
         return (success = false);
      }

      if (bWantSaveCopy)
      {
         // Do this before saving the .aup, because we accumulate
         // mStrOtherNamesArray which affects the contents of the .aup

         // This populates the array mStrOtherNamesArray
         success = this->SaveCopyWaveTracks(project, bLossless);
      }

      if (!success)
         return false;
   }

   // Write the .aup now, before DirManager::SetProject,
   // because it's easier to clean up the effects of successful write of .aup
   // followed by failed SetProject, than the other way about.
   // And that cleanup is done by the destructor of saveFile, if PostCommit() is
   // not done.
   // (SetProject, when it fails, cleans itself up.)
   XMLFileWriter saveFile{ fileName, _("Error Saving Project") };
   success = GuardedCall< bool >( [&] {
         WriteXMLHeader(saveFile);
         WriteXML(saveFile, bWantSaveCopy);
         // Flushes files, forcing space exhaustion errors before trying
         // SetProject():
         saveFile.PreCommit();
         return true;
      },
      MakeSimpleGuard(false),
      // Suppress the usual error dialog for failed write,
      // which is redundant here:
      [](void*){}
   );

   if (!success)
      return false;

   {
   std::vector<std::unique_ptr<WaveTrack::Locker>> lockers;
   Maybe<DirManager::ProjectSetter> pSetter;
   bool moving = true;

   if (fromSaveAs && !bWantSaveCopy) {
      // We are about to move files from the current directory to
      // the NEW directory.  We need to make sure files that belonged
      // to the last saved project don't get erased, so we "lock" them, so that
      // ProjectSetter's constructor copies instead of moves the files.
      // (Otherwise the NEW project would be fine, but the old one would
      // be empty of all of its files.)

      if (mLastSavedTracks) {
         moving = false;
         lockers.reserve(mLastSavedTracks->size());
         for (auto wt : mLastSavedTracks->Any<WaveTrack>())
            lockers.push_back(
               std::make_unique<WaveTrack::Locker>(wt));
      }

      // This renames the project directory, and moves or copies
      // all of our block files over.
      pSetter.create( dirManager, projPath, projName, true, moving );

      if (!pSetter->Ok()){
         success = false;
         return false;
      }
   }

   // Commit the writing of the .aup only now, after we know that the _data
   // folder also saved with no problems.
   // It is very unlikely that errors will happen:
   // only renaming and removing of files, not writes that might exhaust space.
   // So DO give a second dialog in case the unusual happens.
   success = success && GuardedCall< bool >( [&] {
         saveFile.PostCommit();
         return true;
   } );

   if (!success)
      return false;

   // SAVE HAS SUCCEEDED -- following are further no-fail commit operations.

   if (pSetter)
      pSetter->Commit();
   }

   if ( !bWantSaveCopy )
   {
      // Now that we have saved the file, we can DELETE the auto-saved version
      DeleteCurrentAutoSaveFile();

      if (mIsRecovered)
      {
         // This was a recovered file, that is, we have just overwritten the
         // old, crashed .aup file. There may still be orphaned blockfiles in
         // this directory left over from the crash, so we DELETE them now
         dirManager.RemoveOrphanBlockfiles();

         // Before we saved this, this was a recovered project, but now it is
         // a regular project, so remember this.
         mIsRecovered = false;
         mRecoveryAutoSaveDataDir = wxT("");
         SetProjectTitle();
      }
      else if (fromSaveAs)
      {
         // On save as, always remove orphaned blockfiles that may be left over
         // because the user is trying to overwrite another project
         dirManager.RemoveOrphanBlockfiles();
      }

      if (mLastSavedTracks)
         mLastSavedTracks->Clear();
      mLastSavedTracks = TrackList::Create();

      auto &tracks = TrackList::Get( proj );
      for ( auto t : tracks.Any() ) {
         mLastSavedTracks->Add(t->Duplicate());

         //only after the xml has been saved we can mark it saved.
         //thus is because the OD blockfiles change on  background thread while this is going on.
         //         if(const auto wt = track_cast<WaveTrack*>(dupT))
         //            wt->MarkSaved();
      }

      UndoManager::Get( proj ).StateSaved();
   }

   // If we get here, saving the project was successful, so we can DELETE
   // the .bak file (because it now does not fit our block files anymore
   // anyway).
   if (!safetyFileName.empty())
      wxRemoveFile(safetyFileName),
      // cancel the cleanup:
      safetyFileName = wxT("");

   window.GetStatusBar()->SetStatusText(
      wxString::Format(_("Saved %s"), fileName), mainStatusBarField);

   return true;
}

bool ProjectFileIO::SaveCopyWaveTracks(const FilePath & strProjectPathName,
                                         const bool bLossless /*= false*/)
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = TrackFactory::Get( project );

   wxString extension, fileFormat;
#ifdef USE_LIBVORBIS
   if (bLossless) {
      extension = wxT("wav");
      fileFormat = wxT("WAVFLT");
   } else {
      extension = wxT("ogg");
      fileFormat = wxT("OGG");
   }
#else
   extension = wxT("wav");
   fileFormat = wxT("WAVFLT");
#endif
   // Some of this is similar to code in ExportMultiple::ExportMultipleByTrack
   // but that code is really tied into the dialogs.

      // Copy the tracks because we're going to do some state changes before exporting.
      unsigned int numWaveTracks = 0;

   auto ppSavedTrackList = TrackList::Create();
   auto &pSavedTrackList = *ppSavedTrackList;

   auto trackRange = tracks.Any< WaveTrack >();
   for (auto pWaveTrack : trackRange)
   {
      numWaveTracks++;
      pSavedTrackList.Add( trackFactory.DuplicateWaveTrack( *pWaveTrack ) );
   }
   auto cleanup = finally( [&] {
      // Restore the saved track states and clean up.
      auto savedTrackRange = pSavedTrackList.Any<const WaveTrack>();
      auto ppSavedTrack = savedTrackRange.begin();
      for (auto ppTrack = trackRange.begin();

           *ppTrack && *ppSavedTrack;

           ++ppTrack, ++ppSavedTrack)
      {
         auto pWaveTrack = *ppTrack;
         auto pSavedWaveTrack = *ppSavedTrack;
         pWaveTrack->SetSelected(pSavedWaveTrack->GetSelected());
         pWaveTrack->SetMute(pSavedWaveTrack->GetMute());
         pWaveTrack->SetSolo(pSavedWaveTrack->GetSolo());

         pWaveTrack->SetGain(pSavedWaveTrack->GetGain());
         pWaveTrack->SetPan(pSavedWaveTrack->GetPan());
      }
   } );

   if (numWaveTracks == 0)
      // Nothing to save compressed => success. Delete the copies and go.
      return true;

   // Okay, now some bold state-faking to default values.
   for (auto pWaveTrack : trackRange)
   {
      pWaveTrack->SetSelected(false);
      pWaveTrack->SetMute(false);
      pWaveTrack->SetSolo(false);

      pWaveTrack->SetGain(1.0);
      pWaveTrack->SetPan(0.0);
   }

   FilePath strDataDirPathName = strProjectPathName + wxT("_data");
   if (!wxFileName::DirExists(strDataDirPathName) &&
         !wxFileName::Mkdir(strDataDirPathName, 0777, wxPATH_MKDIR_FULL))
      return false;
   strDataDirPathName += wxFileName::GetPathSeparator();

   // Export all WaveTracks to OGG.
   bool bSuccess = true;

   // This accumulates the names of the track files, to be written as
   // dependencies in the .aup file
   mStrOtherNamesArray.clear();

   Exporter theExporter;
   wxFileName uniqueTrackFileName;
   for (auto pTrack : (trackRange + &Track::IsLeader))
   {
      SelectionStateChanger changer{ SelectionState::Get( project ), tracks };
      auto channels = TrackList::Channels(pTrack);

      for (auto channel : channels)
         channel->SetSelected(true);
      uniqueTrackFileName = wxFileName(strDataDirPathName, pTrack->GetName(), extension);
      FileNames::MakeNameUnique(mStrOtherNamesArray, uniqueTrackFileName);
      const auto startTime = channels.min( &Track::GetStartTime );
      const auto endTime = channels.max( &Track::GetEndTime );
      bSuccess =
         theExporter.Process(&project, channels.size(),
                              fileFormat, uniqueTrackFileName.GetFullPath(), true,
                              startTime, endTime);

      if (!bSuccess)
         // If only some exports succeed, the cleanup is not done here
         // but trusted to the caller
         break;
   }

   return bSuccess;
}

bool ProjectFileIO::SaveAs(const wxString & newFileName, bool bWantSaveCopy /*= false*/, bool addToHistory /*= true*/)
{
   auto &project = mProject;

   // This version of SaveAs is invoked only from scripting and does not
   // prompt for a file name
   auto oldFileName = project.GetFileName();

   bool bOwnsNewAupName = mbLoadedFromAup && (oldFileName == newFileName);
   //check to see if the NEW project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if( !bOwnsNewAupName && wxFileExists(newFileName)) {
      AudacityMessageDialog m(
         NULL,
         _("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   project.SetFileName( newFileName );
   bool success = false;
   auto cleanup = finally( [&] {
      if (!success || bWantSaveCopy)
         // Restore file name on error
         project.SetFileName( oldFileName );
   } );

   //Don't change the title, unless we succeed.
   //SetProjectTitle();

   success = DoSave(!bOwnsNewAupName || bWantSaveCopy, bWantSaveCopy);

   if (success && addToHistory) {
      FileHistory::Global().AddFileToHistory( project.GetFileName() );
   }
   if (!success || bWantSaveCopy) // bWantSaveCopy doesn't actually change current project.
   {
   }
   else {
      mbLoadedFromAup = true;
      SetProjectTitle();
   }

   return(success);
}


bool ProjectFileIO::SaveAs(bool bWantSaveCopy /*= false*/, bool bLossless /*= false*/)
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   TitleRestorer Restorer( &project ); // RAII
   bool bHasPath = true;
   wxFileName filename{ project.GetFileName() };
   // Save a copy of the project with 32-bit float tracks.
   if (bLossless)
      bWantSaveCopy = true;

   // Bug 1304: Set a default file path if none was given.  For Save/SaveAs
   if( !FileNames::IsPathAvailable( filename.GetPath( wxPATH_GET_VOLUME| wxPATH_GET_SEPARATOR) ) ){
      bHasPath = false;
      filename = FileNames::DefaultToDocumentsFolder(wxT("/SaveAs/Path"));
   }

   wxString title;
   wxString message;
   if (bWantSaveCopy)
   {
      if (bLossless)
      {
         title = wxString::Format(_("%sSave Lossless Copy of Project \"%s\" As..."),
                                           Restorer.sProjNumber,Restorer.sProjName);
         message = _("\
'Save Lossless Copy of Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Lossless copies of project are a good way to backup your project, \n\
with no loss of quality, but the projects are large.\n");
      }
      else
      {
         title = wxString::Format(_("%sSave Compressed Copy of Project \"%s\" As..."),
                                           Restorer.sProjNumber,Restorer.sProjName);
         message = _("\
'Save Compressed Copy of Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Compressed project files are a good way to transmit your project online, \n\
but they have some loss of fidelity.\n");
      }
   }
   else
   {
      title = wxString::Format(_("%sSave Project \"%s\" As..."),
                               Restorer.sProjNumber, Restorer.sProjName);
      message = _("\
'Save Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n");
   }
   if (ShowWarningDialog(&window, wxT("FirstProjectSave"), message, true) != wxID_OK)
   {
      return false;
   }

   bool bPrompt = (project.mBatchMode == 0) || (project.GetFileName().empty());
   wxString fName;

   if (bPrompt) {
      // JKC: I removed 'wxFD_OVERWRITE_PROMPT' because we are checking
      // for overwrite ourselves later, and we disallow it.
      // We disallow overwrite because we would have to DELETE the many
      // smaller files too, or prompt to move them.
      fName = FileNames::SelectFile(FileNames::Operation::Export,
         title,
         filename.GetPath(),
         filename.GetFullName(),
         wxT("aup"),
         _("Audacity projects") + wxT(" (*.aup)|*.aup"),
         wxFD_SAVE | wxRESIZE_BORDER,
         &window);

      if (fName.empty())
         return false;

      filename = fName;
   };

   filename.SetExt(wxT("aup"));
   fName = filename.GetFullPath();

   if ((bWantSaveCopy||!bPrompt) && filename.FileExists()) {
      // Saving a copy of the project should never overwrite an existing project.
      AudacityMessageDialog m(
         NULL,
         _("Saving a copy must not overwrite an existing saved project.\nPlease try again and select an original name."),
         _("Error Saving Copy of Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   bool bOwnsNewAupName = mbLoadedFromAup && ( project.GetFileName() == fName );
   // Check to see if the project file already exists, and if it does
   // check that the project file 'belongs' to this project.
   // otherwise, prompt the user before overwriting.
   if (!bOwnsNewAupName && filename.FileExists()) {
      // Ensure that project of same name is not open in another window.
      // fName is the destination file.
      // mFileName is this project.
      // It is possible for mFileName == fName even when this project is not
      // saved to disk, and we then need to check the destination file is not
      // open in another window.
      int mayOverwrite = ( project.GetFileName() == fName ) ? 2 : 1;
      for ( auto p : AllProjects{} ) {
         const wxFileName openProjectName{ p->GetFileName() };
         if (openProjectName.SameAs(fName)) {
            mayOverwrite -= 1;
            if (mayOverwrite == 0)
               break;
         }
      }

      if (mayOverwrite > 0) {
         /* i18n-hint: In each case, %s is the name
          of the file being overwritten.*/
         wxString Message = wxString::Format(_("\
Do you want to overwrite the project:\n\"%s\"?\n\n\
If you select \"Yes\" the project\n\"%s\"\n\
will be irreversibly overwritten."), fName, fName);

         // For safety, there should NOT be an option to hide this warning.
         int result = AudacityMessageBox(Message,
                                         /* i18n-hint: Heading: A warning that a project is about to be overwritten.*/
                                         _("Overwrite Project Warning"),
                                         wxYES_NO | wxNO_DEFAULT | wxICON_WARNING,
                                         &window);
         if (result != wxYES) {
            return false;
         }
      }
      else
      {
         // Overwrite disalowed. The destination project is open in another window.
         AudacityMessageDialog m(
            NULL,
            _("The project will not saved because the selected project is open in another window.\nPlease try again and select an original name."),
            _("Error Saving Project"),
            wxOK|wxICON_ERROR);
         m.ShowModal();
         return false;
      }
   }

   auto oldFileName = project.GetFileName();
   project.SetFileName( fName );
   bool success = false;
   auto cleanup = finally( [&] {
      if (!success || bWantSaveCopy)
         // Restore file name on error
         project.SetFileName( oldFileName );
   } );

   success = DoSave(!bOwnsNewAupName || bWantSaveCopy, bWantSaveCopy, bLossless);

   if (success) {
      FileHistory::Global().AddFileToHistory( project.GetFileName() );
      if( !bHasPath )
      {
         gPrefs->Write( wxT("/SaveAs/Path"), filename.GetPath());
         gPrefs->Flush();
      }
   }
   if (!success || bWantSaveCopy) // bWantSaveCopy doesn't actually change current project.
   {
   }
   else {
      mbLoadedFromAup = true;
      SetProjectTitle();
   }


   return(success);
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
      WriteXML( buffer, false );
      mStrOtherNamesArray.clear();

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
         wxString::Format( _("Could not create autosave file: %s"),
            fn + wxT(".autosave") ),
         _("Error"), wxICON_STOP, &window);
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
               wxString::Format(
                  _("Could not remove old autosave file: %s"), mAutoSaveFileName ),
               _("Error"), wxICON_STOP, &window);
            return;
         }
      }

      mAutoSaveFileName = wxT("");
   }
}

bool ProjectFileIO::IsProjectSaved() {
   auto &project = mProject;
   auto &dirManager = DirManager::Get( project );
   // This is true if a project was opened from an .aup
   // Otherwise it becomes true only when a project is first saved successfully
   // in DirManager::SetProject
   return (!dirManager.GetProjectName().empty());
}

void ProjectFileIO::ResetProjectFileIO()
{
   // mLastSavedTrack code copied from OnCloseWindow.
   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we DELETE the blockfiles
   // in memory.  After it's locked, DELETE the data structure so that
   // there's no memory leak.
   CloseLock();

   //mLastSavedTracks = TrackList::Create();
   mProject.SetFileName( {} );
   mIsRecovered = false;
   mbLoadedFromAup = false;
   SetProjectTitle();
}

bool ProjectFileIO::SaveFromTimerRecording(wxFileName fnFile)
{
   auto &project = mProject;

   // MY: Will save the project to a NEW location a-la Save As
   // and then tidy up after itself.

   wxString sNewFileName = fnFile.GetFullPath();

   // MY: To allow SaveAs from Timer Recording we need to check what
   // the value of mFileName is before we change it.
   FilePath sOldFilename;
   if (IsProjectSaved()) {
      sOldFilename = project.GetFileName();
   }

   // MY: If the project file already exists then bail out
   // and send populate the message string (pointer) so
   // we can tell the user what went wrong.
   if (wxFileExists(sNewFileName)) {
      return false;
   }

   project.SetFileName( sNewFileName );
   bool bSuccess = false;
   auto cleanup = finally( [&] {
      if (!bSuccess)
         // Restore file name on error
         project.SetFileName( sOldFilename );
   } );

   bSuccess = DoSave(true, false);

   if (bSuccess) {
      FileHistory::Global().AddFileToHistory( project.GetFileName() );
      mbLoadedFromAup = true;
      SetProjectTitle();
   }

   return bSuccess;
}

void ProjectFileIO::CloseLock()
{
   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we DELETE the blockfiles
   // in memory.  After it's locked, DELETE the data structure so that
   // there's no memory leak.
   if (mLastSavedTracks) {
      for (auto wt : mLastSavedTracks->Any<WaveTrack>())
         wt->CloseLock();

      mLastSavedTracks->Clear();
      mLastSavedTracks.reset();
   }
}
