#include "../Audacity.h" // for USE_* macros
#include "../Experimental.h"

#include "../AudacityApp.h"
#include "../BatchCommands.h"
#include "../FileNames.h"
#include "../LabelTrack.h"
#include "../NoteTrack.h"
#include "../Prefs.h"
#include "../Printing.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../export/Export.h"
#include "../export/ExportMultiple.h"

#ifdef USE_MIDI
#include "../import/ImportMIDI.h"
#endif // USE_MIDI

#include "../ondemand/ODManager.h"

#include <wx/menu.h>

// private helper classes and functions
namespace {
void DoExport
(AudacityProject &project, const wxString & Format )
{
   auto tracks = project.GetTracks();

   Exporter e;

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   double t0 = 0.0;
   double t1 = tracks->GetEndTime();

   // Prompt for file name and/or extension?
   bool bPromptingRequired =
      (project.mBatchMode == 0) || project.GetFileName().empty() ||
      Format.empty();
   wxString filename;

   if (!bPromptingRequired) {

      // We're in batch mode, and we have an mFileName and Format.
      wxString extension = ".";
      extension += Format;
      extension.MakeLower();

      filename =
         MacroCommands::BuildCleanFileName(project.GetFileName(), extension);

      // Bug 1854, No warning of file overwrite
      // (when export is called from Macros).
      int counter = 0;
      bPromptingRequired = wxFileExists(filename);

      // We'll try alternative names to avoid overwriting.
      while ( bPromptingRequired && counter < 100 ) {
         counter++;
         wxString number;
         number.Printf("%03i", counter);
         // So now the name has a number in it too.
         filename = MacroCommands::BuildCleanFileName(
            project.GetFileName(), number + extension);
         bPromptingRequired = wxFileExists(filename);
      }
      // If we've run out of alternative names, we will fall back to prompting
      // - even if in a macro.
   }


   if (bPromptingRequired)
   {
      // Do export with prompting.
      e.SetDefaultFormat(Format);
      e.Process(&project, false, t0, t1);
   }
   else
   {
      wxGetApp().AddFileToHistory(filename);
      // We're in batch mode, the file does not exist already.
      // We really can proceed without prompting.
      int nChannels = MacroCommands::IsMono() ? 1 : 2;
      e.Process(
         &project,   // AudacityProject
         nChannels,  // numChannels,
         Format,     // type, 
         filename,   // filename,
         false,      // selectedOnly, 
         t0,         // t0
         t1          // t1
      );
   }

}
}

namespace FileActions {

// exported helper functions

#ifdef USE_MIDI
// return null on failure; if success, return the given project, or a NEW
// one, if the given was null; create no NEW project if failure
AudacityProject *DoImportMIDI(
   AudacityProject *pProject, const FilePath &fileName)
{
   auto tracks = pProject->GetTracks();

   AudacityProject *pNewProject {};
   if ( !pProject )
      pProject = pNewProject = CreateNewAudacityProject();
   auto cleanup = finally( [&]
      { if ( pNewProject ) pNewProject->Close(true); } );

   auto newTrack = pProject->GetTrackFactory()->NewNoteTrack();

   if (::ImportMIDI(fileName, newTrack.get())) {

      pProject->SelectNone();
      auto pTrack = tracks->Add( newTrack );
      pTrack->SetSelected(true);

      pProject->PushState(wxString::Format(_("Imported MIDI from '%s'"),
         fileName), _("Import MIDI"));

      pProject->ZoomAfterImport(pTrack);
      pNewProject = nullptr;

      wxGetApp().AddFileToHistory(fileName);

      return pProject;
   }
   else
      return nullptr;
}
#endif

#ifdef USE_MIDI

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnNew(const CommandContext & )
{
   CreateNewAudacityProject();
}

void OnOpen(const CommandContext &context )
{
   auto &project = context.project;
   AudacityProject::OpenFiles(&project);
}

// JKC: This is like OnClose, except it emptys the project in place,
// rather than createing a new empty project (with new toolbars etc).
// It does not test for unsaved changes.
// It is not in the menus by default.  Its main purpose is/was for 
// developers checking functionality of ResetProjectToEmpty().
void OnProjectReset(const CommandContext &context)
{
   auto &project = context.project;
   project.ResetProjectToEmpty();
}

void OnClose(const CommandContext &context )
{
   auto &project = context.project;
   project.SetMenuClose(true);
   project.Close();
}

void OnSave(const CommandContext &context )
{
   auto &project = context.project;
   project.Save();
}

void OnSaveAs(const CommandContext &context )
{
   auto &project = context.project;
   project.SaveAs();
}

void OnSaveCopy(const CommandContext &context )
{
   auto &project = context.project;
   project.SaveAs(true, true);
}

#ifdef USE_LIBVORBIS
void OnSaveCompressed(const CommandContext &context)
{
   auto &project = context.project;
   project.SaveAs(true);
}
#endif

void OnExportMp3(const CommandContext &context)
{
   auto &project = context.project;
   DoExport(project, "MP3");
}

void OnExportWav(const CommandContext &context)
{
   auto &project = context.project;
   DoExport(project, "WAV");
}

void OnExportOgg(const CommandContext &context)
{
   auto &project = context.project;
   DoExport(project, "OGG");
}

void OnExportAudio(const CommandContext &context)
{
   auto &project = context.project;
   DoExport(project, "");
}

void OnExportSelection(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   Exporter e;

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   e.SetFileDialogTitle( _("Export Selected Audio") );
   e.Process(&project, true, selectedRegion.t0(),
      selectedRegion.t1());
}

void OnExportLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();

   /* i18n-hint: filename containing exported text from label tracks */
   wxString fName = _("labels.txt");
   auto trackRange = tracks->Any<const LabelTrack>();
   auto numLabelTracks = trackRange.size();

   if (numLabelTracks == 0) {
      AudacityMessageBox(_("There are no label tracks to export."));
      return;
   }
   else
      fName = (*trackRange.rbegin())->GetName();

   fName = FileNames::SelectFile(FileNames::Operation::Export,
                        _("Export Labels As:"),
                        wxEmptyString,
                        fName,
                        wxT("txt"),
                        wxT("*.txt"),
                        wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                        &project);

   if (fName.empty())
      return;

   // Move existing files out of the way.  Otherwise wxTextFile will
   // append to (rather than replace) the current file.

   if (wxFileExists(fName)) {
#ifdef __WXGTK__
      wxString safetyFileName = fName + wxT("~");
#else
      wxString safetyFileName = fName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      wxRename(fName, safetyFileName);
   }

   wxTextFile f(fName);
   f.Create();
   f.Open();
   if (!f.IsOpened()) {
      AudacityMessageBox( wxString::Format(
         _("Couldn't write to file: %s"), fName ) );
      return;
   }

   for (auto lt : trackRange)
      lt->Export(f);

   f.Write();
   f.Close();
}

void OnExportMultiple(const CommandContext &context)
{
   auto &project = context.project;
   ExportMultiple em(&project);

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   em.ShowModal();
}

#ifdef USE_MIDI
void OnExportMIDI(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();

   // Make sure that there is
   // exactly one NoteTrack selected.
   const auto range = tracks->Selected< const NoteTrack >();
   const auto numNoteTracksSelected = range.size();

   if(numNoteTracksSelected > 1) {
      AudacityMessageBox(_(
         "Please select only one Note Track at a time."));
      return;
   }
   else if(numNoteTracksSelected < 1) {
      AudacityMessageBox(_(
         "Please select a Note Track."));
      return;
   }

   wxASSERT(numNoteTracksSelected);
   if (!numNoteTracksSelected)
      return;

   const auto nt = *range.begin();

   while(true) {

      wxString fName;

      fName = FileNames::SelectFile(FileNames::Operation::Export,
         _("Export MIDI As:"),
         wxEmptyString,
         fName,
         wxT(".mid|.gro"),
         _("MIDI file (*.mid)|*.mid|Allegro file (*.gro)|*.gro"),
         wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
         &project);

      if (fName.empty())
         return;

      if(!fName.Contains(wxT("."))) {
         fName = fName + wxT(".mid");
      }

      // Move existing files out of the way.  Otherwise wxTextFile will
      // append to (rather than replace) the current file.

      if (wxFileExists(fName)) {
#ifdef __WXGTK__
         wxString safetyFileName = fName + wxT("~");
#else
         wxString safetyFileName = fName + wxT(".bak");
#endif

         if (wxFileExists(safetyFileName))
            wxRemoveFile(safetyFileName);

         wxRename(fName, safetyFileName);
      }

      if(fName.EndsWith(wxT(".mid")) || fName.EndsWith(wxT(".midi"))) {
         nt->ExportMIDI(fName);
      } else if(fName.EndsWith(wxT(".gro"))) {
         nt->ExportAllegro(fName);
      } else {
         wxString msg = _("You have selected a filename with an unrecognized file extension.\nDo you want to continue?");
         wxString title = _("Export MIDI");
         int id = AudacityMessageBox(msg, title, wxYES_NO);
         if (id == wxNO) {
            continue;
         } else if (id == wxYES) {
            nt->ExportMIDI(fName);
         }
      }
      break;
   }
}
#endif // USE_MIDI

void OnImport(const CommandContext &context)
{
   auto &project = context.project;

   // An import trigger for the alias missing dialog might not be intuitive, but
   // this serves to track the file if the users zooms in and such.
   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   wxArrayString selectedFiles = project.ShowOpenDialog(wxT(""));
   if (selectedFiles.size() == 0) {
      gPrefs->Write(wxT("/LastOpenType"),wxT(""));
      gPrefs->Flush();
      return;
   }

   // PRL:  This affects FFmpegImportPlugin::Open which resets the preference
   // to false.  Should it also be set to true on other paths that reach
   // AudacityProject::Import ?
   gPrefs->Write(wxT("/NewImportingSession"), true);

   //sort selected files by OD status.  Load non OD first so user can edit asap.
   //first sort selectedFiles.
   selectedFiles.Sort(CompareNoCaseFileName);
   ODManager::Pauser pauser;

   auto cleanup = finally( [&] {
      gPrefs->Write(wxT("/LastOpenType"),wxT(""));

      gPrefs->Flush();

      project.HandleResize(); // Adjust scrollers for NEW track sizes.
   } );

   for (size_t ff = 0; ff < selectedFiles.size(); ff++) {
      wxString fileName = selectedFiles[ff];

      FileNames::UpdateDefaultPath(FileNames::Operation::Open, fileName);

      project.Import(fileName);
   }

   project.ZoomAfterImport(nullptr);
}

void OnImportLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto trackFactory = project.GetTrackFactory();
   auto tracks = project.GetTracks();

   wxString fileName =
       FileNames::SelectFile(FileNames::Operation::Open,
                    _("Select a text file containing labels"),
                    wxEmptyString,     // Path
                    wxT(""),       // Name
                    wxT(".txt"),   // Extension
                    _("Text files (*.txt)|*.txt|All files|*"),
                    wxRESIZE_BORDER,        // Flags
                    &project);    // Parent

   if (!fileName.empty()) {
      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         AudacityMessageBox(
            wxString::Format( _("Could not open file: %s"), fileName ) );
         return;
      }

      auto newTrack = trackFactory->NewLabelTrack();
      wxString sTrackName;
      wxFileName::SplitPath(fileName, NULL, NULL, &sTrackName, NULL);
      newTrack->SetName(sTrackName);

      newTrack->Import(f);

      project.SelectNone();
      newTrack->SetSelected(true);
      tracks->Add( newTrack );

      project.PushState(wxString::
                Format(_("Imported labels from '%s'"), fileName),
                _("Import Labels"));

      project.ZoomAfterImport(nullptr);
   }
}

void OnImportMIDI(const CommandContext &context)
{
   auto &project = context.project;

   wxString fileName = FileNames::SelectFile(FileNames::Operation::Open,
      _("Select a MIDI file"),
      wxEmptyString,     // Path
      wxT(""),       // Name
      wxT(""),       // Extension
      _("MIDI and Allegro files (*.mid;*.midi;*.gro)|*.mid;*.midi;*.gro|MIDI files (*.mid;*.midi)|*.mid;*.midi|Allegro files (*.gro)|*.gro|All files|*"),
      wxRESIZE_BORDER,        // Flags
      &project);    // Parent

   if (!fileName.empty())
      DoImportMIDI(&project, fileName);
}
#endif

void OnImportRaw(const CommandContext &context)
{
   auto &project = context.project;
   auto trackFactory = project.GetTrackFactory();

   wxString fileName =
       FileNames::SelectFile(FileNames::Operation::Open,
                    _("Select any uncompressed audio file"),
                    wxEmptyString,     // Path
                    wxT(""),       // Name
                    wxT(""),       // Extension
                    _("All files|*"),
                    wxRESIZE_BORDER,        // Flags
                    &project);    // Parent

   if (fileName.empty())
      return;

   TrackHolders newTracks;

   ::ImportRaw(&project, fileName, trackFactory, newTracks);

   if (newTracks.size() <= 0)
      return;

   project.AddImportedTracks(fileName, std::move(newTracks));
   project.HandleResize(); // Adjust scrollers for NEW track sizes.
}

void OnPageSetup(const CommandContext &context)
{
   auto &project = context.project;
   HandlePageSetup(&project);
}

void OnPrint(const CommandContext &context)
{
   auto &project = context.project;
   auto name = project.GetName();
   auto tracks = project.GetTracks();
   HandlePrint(&project, name, tracks, *project.GetTrackPanel());
}

void OnExit(const CommandContext &WXUNUSED(context) )
{
   QuitAudacity();
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static FileActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& FileActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr FileMenu( AudacityProject& )
{
   using namespace MenuTable;

   return Menu( _("&File"),
      /*i18n-hint: "New" is an action (verb) to create a NEW project*/
      Command( wxT("New"), XXO("&New"), FN(OnNew),
         AudioIONotBusyFlag, wxT("Ctrl+N") ),

      /*i18n-hint: (verb)*/
      Command( wxT("Open"), XXO("&Open..."), FN(OnOpen),
         AudioIONotBusyFlag, wxT("Ctrl+O") ),

#ifdef EXPERIMENTAL_RESET
      // Empty the current project and forget its name and path.  DANGEROUS
      // It's just for developers.
      // Do not translate this menu item (no XXO).  
      // It MUST not be shown to regular users.
      Command( wxT("Reset"), XXO("&Dangerous Reset..."), FN(OnProjectReset),
         AudioIONotBusyFlag ),
#endif

/////////////////////////////////////////////////////////////////////////////

      Menu(
#ifdef __WXMAC__
         /* i18n-hint: This is the name of the menu item on Mac OS X only */
         _("Open Recent")
#else
         /* i18n-hint: This is the name of the menu item on Windows and Linux */
         _("Recent &Files")
#endif
         ,
         Special( [](AudacityProject &, wxMenu &theMenu){
            // Recent Files and Recent Projects menus
            wxGetApp().GetRecentFiles()->UseMenu( &theMenu );
            wxGetApp().GetRecentFiles()->AddFilesToMenu( &theMenu );

            wxWeakRef<wxMenu> recentFilesMenu{ &theMenu };
            wxTheApp->CallAfter( [=] {
               // Bug 143 workaround.
               // The bug is in wxWidgets.  For a menu that has scrollers,
               // the scrollers have an ID of 0 (not wxID_NONE which is -3).
               // Therefore wxWidgets attempts to find a help string. See
               // wxFrameBase::ShowMenuHelp(int menuId)
               // It finds a bogus automatic help string of "Recent &Files"
               // from that submenu.
               // So we set the help string for command with Id 0 to empty.
               if ( recentFilesMenu )
                  recentFilesMenu->GetParent()->SetHelpString( 0, "" );
            } );
         } )
      ),

/////////////////////////////////////////////////////////////////////////////

      Command( wxT("Close"), XXO("&Close"), FN(OnClose),
         AudioIONotBusyFlag, wxT("Ctrl+W") ),

      Separator(),

      Menu( _("&Save Project"),
         Command( wxT("Save"), XXO("&Save Project"), FN(OnSave),
            AudioIONotBusyFlag | UnsavedChangesFlag, wxT("Ctrl+S") ),
         Command( wxT("SaveAs"), XXO("Save Project &As..."), FN(OnSaveAs),
            AudioIONotBusyFlag ),
         // TODO: The next two items should be disabled if project is empty
         Command( wxT("SaveCopy"), XXO("Save Lossless Copy of Project..."),
            FN(OnSaveCopy), AudioIONotBusyFlag )
#ifdef USE_LIBVORBIS
         ,
         Command( wxT("SaveCompressed"),
            XXO("&Save Compressed Copy of Project..."),
            FN(OnSaveCompressed), AudioIONotBusyFlag )
#endif
      ),

      Separator(),

      Menu( _("&Export"),
         // Enable Export audio commands only when there are audio tracks.
         Command( wxT("ExportMp3"), XXO("Export as MP&3"), FN(OnExportMp3),
            AudioIONotBusyFlag | WaveTracksExistFlag ),

         Command( wxT("ExportWav"), XXO("Export as &WAV"), FN(OnExportWav),
            AudioIONotBusyFlag | WaveTracksExistFlag ),

         Command( wxT("ExportOgg"), XXO("Export as &OGG"), FN(OnExportOgg),
            AudioIONotBusyFlag | WaveTracksExistFlag ),

         Command( wxT("Export"), XXO("&Export Audio..."), FN(OnExportAudio),
            AudioIONotBusyFlag | WaveTracksExistFlag, wxT("Ctrl+Shift+E") ),

         // Enable Export Selection commands only when there's a selection.
         Command( wxT("ExportSel"), XXO("Expo&rt Selected Audio..."),
            FN(OnExportSelection),
            AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag ),

         Command( wxT("ExportLabels"), XXO("Export &Labels..."),
            FN(OnExportLabels),
            AudioIONotBusyFlag | LabelTracksExistFlag ),
         // Enable Export audio commands only when there are audio tracks.
         Command( wxT("ExportMultiple"), XXO("Export &Multiple..."),
            FN(OnExportMultiple),
            AudioIONotBusyFlag | WaveTracksExistFlag, wxT("Ctrl+Shift+L") )
#if defined(USE_MIDI)
         ,
         Command( wxT("ExportMIDI"), XXO("Export MI&DI..."), FN(OnExportMIDI),
            AudioIONotBusyFlag | NoteTracksExistFlag )
#endif
      ),

      Menu( _("&Import"),
         Command( wxT("ImportAudio"), XXO("&Audio..."), FN(OnImport),
            AudioIONotBusyFlag, wxT("Ctrl+Shift+I") ),
         Command( wxT("ImportLabels"), XXO("&Labels..."), FN(OnImportLabels),
            AudioIONotBusyFlag ),
   #ifdef USE_MIDI
         Command( wxT("ImportMIDI"), XXO("&MIDI..."), FN(OnImportMIDI),
            AudioIONotBusyFlag ),
   #endif // USE_MIDI
         Command( wxT("ImportRaw"), XXO("&Raw Data..."), FN(OnImportRaw),
            AudioIONotBusyFlag )
      ),

      Separator(),

/////////////////////////////////////////////////////////////////////////////

      Command( wxT("PageSetup"), XXO("Pa&ge Setup..."), FN(OnPageSetup),
         AudioIONotBusyFlag | TracksExistFlag ),
      /* i18n-hint: (verb) It's item on a menu. */
      Command( wxT("Print"), XXO("&Print..."), FN(OnPrint),
         AudioIONotBusyFlag | TracksExistFlag ),

      Separator(),

      // On the Mac, the Exit item doesn't actually go here...wxMac will
      // pull it out
      // and put it in the Audacity menu for us based on its ID.
      /* i18n-hint: (verb) It's item on a menu. */
      Command( wxT("Exit"), XXO("E&xit"), FN(OnExit),
         AlwaysEnabledFlag, wxT("Ctrl+Q") )
   );
}

#undef XXO
#undef FN
