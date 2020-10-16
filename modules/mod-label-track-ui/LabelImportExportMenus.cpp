#include "CommonCommandFlags.h"
#include "FileNames.h"
#include "LabelImportExport.h"
#include "LabelTrack.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ProjectWindow.h"
#include "SelectFile.h"
#include "SelectUtilities.h"
#include "WaveTrack.h"
#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "widgets/AudacityMessageBox.h"
#include <wx/filedlg.h>
 
// Menu handler functions
namespace {

void OnExportLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &window = GetProjectFrame( project );

   /* i18n-hint: filename containing exported text from label tracks */
   wxString fName = _("labels.txt");
   auto trackRange = tracks.Any<const LabelTrack>();
   auto numLabelTracks = trackRange.size();

   if (numLabelTracks == 0) {
      AudacityMessageBox( XO("There are no label tracks to export.") );
      return;
   }
   else
      fName = (*trackRange.rbegin())->GetName();

   fName = SelectFile(FileNames::Operation::Export,
      XO("Export Labels As:"),
      wxEmptyString,
      fName,
      wxT("txt"),
      { FileNames::TextFiles },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      &window);

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
      AudacityMessageBox(
         XO( "Couldn't write to file: %s" ).Format( fName ) );
      return;
   }

   for (auto lt : trackRange)
      ExportLabelTrack(*lt, f);

   f.Write();
   f.Close();
}

void OnImportLabels(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );

   wxString fileName =
       SelectFile(FileNames::Operation::Open,
         XO("Select a text file containing labels"),
         wxEmptyString,     // Path
         wxT(""),       // Name
         wxT("txt"),   // Extension
         { FileNames::TextFiles, FileNames::AllFiles },
         wxRESIZE_BORDER,        // Flags
         &window);    // Parent

   if (!fileName.empty()) {
      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         AudacityMessageBox(
            XO("Could not open file: %s").Format( fileName ) );
         return;
      }

      auto newTrack = std::make_shared<LabelTrack>();
      wxString sTrackName;
      wxFileName::SplitPath(fileName, NULL, NULL, &sTrackName, NULL);
      newTrack->SetName(sTrackName);

      ImportLabelTrack(*newTrack, f);

      SelectUtilities::SelectNone( project );
      newTrack->SetSelected(true);
      tracks.Add( newTrack );

      ProjectHistory::Get( project ).PushState(
         XO("Imported labels from '%s'").Format( fileName ),
            XO("Import Labels"));

      window.ZoomAfterImport(nullptr);
   }
}

// Menu definitions

using namespace MenuTable;
AttachedItem sAttachment{
   { wxT("File/Import-Export/Export"),
      { OrderingHint::After, wxT("ExportSel") } },
   Command( wxT("ExportLabels"), XXO("Export &Labels..."),
      OnExportLabels, AudioIONotBusyFlag() | LabelTracksExistFlag() )
};

AttachedItem sAttachment2{
   { wxT("File/Import-Export/Import"),
      { OrderingHint::After, wxT("ImportAudio") } },
   Command( wxT("ImportLabels"), XXO("&Labels..."),
      OnImportLabels, AudioIONotBusyFlag() )
};

} // namespace
