/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/frame.h>
#include <wx/intl.h>

#if defined(USE_MIDI)


#include "../lib-src/header-substitutes/allegro.h"

//#include "strparse.h"
//#include "mfmidi.h"

#include "../import/Import.h"
#include "../import/ImportPlugin.h"
#include "../NoteTrack.h"
#include "Project.h"
#include "../ProjectFileIO.h"
#include "ProjectHistory.h"
#include "../ProjectWindow.h"
#include "ProjectWindows.h"
#include "SelectFile.h"
#include "../SelectUtilities.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/FileHistory.h"
#include "../widgets/ProgressDialog.h"

namespace {

bool ImportMIDI(const FilePath &fName, NoteTrack * dest);

// Given an existing project, try to import into it, return true on success
bool DoImportMIDI( AudacityProject &project, const FilePath &fileName )
{
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto newTrack =  std::make_shared<NoteTrack>();
   bool initiallyEmpty = tracks.empty();
   
   if (::ImportMIDI(fileName, newTrack.get())) {
      
      SelectUtilities::SelectNone( project );
      auto pTrack = tracks.Add( newTrack );
      pTrack->SetSelected(true);
      
      // Fix the bug 2109.
      // In case the project had soloed tracks before importing,
      // the newly imported track is muted.
      const bool projectHasSolo =
         !(tracks.Any<PlayableTrack>() + &PlayableTrack::GetSolo).empty();
#ifdef EXPERIMENTAL_MIDI_OUT
      if (projectHasSolo)
         pTrack->SetMute(true);
#endif

      ProjectHistory::Get( project )
         .PushState(
            XO("Imported MIDI from '%s'").Format( fileName ),
            XO("Import MIDI")
         );
      
      ProjectWindow::Get( project ).ZoomAfterImport(pTrack);
      FileHistory::Global().Append(fileName);

      // If the project was clean and temporary (not permanently saved), then set
      // the filename to the just imported path.
      if (initiallyEmpty && projectFileIO.IsTemporary()) {
         wxFileName fn(fileName);
         project.SetProjectName(fn.GetName());
         project.SetInitialImportPath(fn.GetPath());
         projectFileIO.SetProjectTitle();
      }
      return true;
   }
   else
      return false;
}

bool ImportMIDI(const FilePath &fName, NoteTrack * dest)
{
   if (fName.length() <= 4){
      AudacityMessageBox(
         XO("Could not open file %s: Filename too short.").Format( fName ) );
      return false;
   }

   bool is_midi = false;
   if (fName.Right(4).CmpNoCase(wxT(".mid")) == 0 || fName.Right(5).CmpNoCase(wxT(".midi")) == 0)
      is_midi = true;
   else if(fName.Right(4).CmpNoCase(wxT(".gro")) != 0) {
      AudacityMessageBox(
         XO("Could not open file %s: Incorrect filetype.").Format( fName ) );
      return false;
   }

   wxFFile mf(fName, wxT("rb"));
   if (!mf.IsOpened()) {
      AudacityMessageBox(
         XO("Could not open file %s.").Format( fName ) );
      return false;
   }

   double offset = 0.0;
   auto new_seq = std::make_unique<Alg_seq>(fName.mb_str(), is_midi, &offset);

   //Should we also check if(seq->tracks() == 0) ?
   if(new_seq->get_read_error() == alg_error_open){
      AudacityMessageBox(
         XO("Could not open file %s.").Format( fName ) );
      mf.Close();
      return false;
   }

   dest->SetSequence(std::move(new_seq));
   dest->SetOffset(offset);
   wxString trackNameBase = fName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
   dest->SetName(trackNameBase);
   mf.Close();

   dest->ZoomAllNotes();
   return true;
}

}

// Insert a menu item
#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "CommonCommandFlags.h"

namespace {
using namespace MenuTable;

void OnImportMIDI(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = GetProjectFrame( project );

   wxString fileName = SelectFile(FileNames::Operation::Open,
      XO("Select a MIDI file"),
      wxEmptyString,     // Path
      wxT(""),       // Name
      wxT(""),       // Extension
      {
         { XO("MIDI and Allegro files"),
           { wxT("mid"), wxT("midi"), wxT("gro"), }, true },
         { XO("MIDI files"),
           { wxT("mid"), wxT("midi"), }, true },
         { XO("Allegro files"),
           { wxT("gro"), }, true },
         FileNames::AllFiles
      },
      wxRESIZE_BORDER,        // Flags
      &window);    // Parent

   if (!fileName.empty())
      DoImportMIDI(project, fileName);
}

AttachedItem sAttachment{
   { wxT("File/Import-Export/Import"),
      { OrderingHint::Before, {"ImportRaw"} } },
   Command( wxT("ImportMIDI"), XXO("&MIDI..."), OnImportMIDI,
      AudioIONotBusyFlag() )
};

constexpr auto exts = {
   wxT("gro"),
   wxT("midi"),
   wxT("mid"),
};

const auto DESC = XO("MIDI files");

class MIDIImportFileHandle final : public ImportFileHandle
{
public:
   explicit MIDIImportFileHandle(const FilePath &fileName)
      : ImportFileHandle{ fileName }
   {}

   ~MIDIImportFileHandle() override {}

   TranslatableString GetFileDescription() override { return DESC; }

   ByteCount GetFileUncompressedBytes() override
   {
      // TODO: Get Uncompressed byte count.
      return 0;
   }

   ImportResult Import(WaveTrackFactory *trackFactory,
      TrackHolders &outTracks, Tags *tags) override;

   wxInt32 GetStreamCount() override { return 1; }

   const TranslatableStrings &GetStreamInfo() override
   {
      static TranslatableStrings empty;
      return empty;
   }

   void SetStreamUsage(wxInt32, bool) override {}
};

auto MIDIImportFileHandle::Import(
   WaveTrackFactory *, TrackHolders &tracks, Tags *) -> ImportResult
{
   auto newTrack = std::make_shared<NoteTrack>();
   if (::ImportMIDI(mFilename, newTrack.get())) {
      tracks.push_back({});
      tracks.back().push_back(newTrack);
      return ImportResult::Success;
   }
   return ImportResult::Failed;
}

class MIDIImportPlugin final : public ImportPlugin
{
public:
   MIDIImportPlugin()
      : ImportPlugin(FileExtensions(exts.begin(), exts.end()))
   {}
   ~MIDIImportPlugin() override {}

   wxString GetPluginStringID() override { return wxT("portsmf"); }
   
   TranslatableString GetPluginFormatDescription() override { return DESC; }

   std::unique_ptr<ImportFileHandle> Open(const FilePath &fileName,
                     AudacityProject *project) override
   {
      return std::make_unique<MIDIImportFileHandle>(fileName);
   }
};

Importer::RegisteredImportPlugin registered{ "portsmf",
   std::make_unique<MIDIImportPlugin>()
};

}
#endif
