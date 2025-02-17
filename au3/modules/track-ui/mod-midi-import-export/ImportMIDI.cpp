/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/frame.h>

#if defined(USE_MIDI)

#include "WrapAllegro.h"

#include "Import.h"
#include "ImportPlugin.h"
#include "ImportProgressListener.h"
#include "NoteTrack.h"
#include "tracks/playabletrack/notetrack/ui/NoteTrackDisplayData.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "Viewport.h"
#include "ProjectWindows.h"
#include "SelectFile.h"
#include "SelectUtilities.h"
#include "AudacityMessageBox.h"
#include "widgets/FileHistory.h"
#include "ProgressDialog.h"

namespace {
bool ImportMIDI(const FilePath& fName, NoteTrack* dest);

// Given an existing project, try to import into it, return true on success
bool DoImportMIDI(AudacityProject& project, const FilePath& fileName)
{
    auto& projectFileIO = ProjectFileIO::Get(project);
    auto& tracks = TrackList::Get(project);
    auto newTrack =  std::make_shared<NoteTrack>();
    bool initiallyEmpty = tracks.empty();

    if (::ImportMIDI(fileName, newTrack.get())) {
        SelectUtilities::SelectNone(project);
        auto pTrack = tracks.Add(newTrack);
        pTrack->SetSelected(true);

        // Fix the bug 2109.
        // In case the project had soloed tracks before importing,
        // the newly imported track is muted.
        const bool projectHasSolo
            =!(tracks.Any<PlayableTrack>() + &PlayableTrack::GetSolo).empty();
        if (projectHasSolo) {
            pTrack->SetMute(true);
        }

        ProjectHistory::Get(project)
        .PushState(
            XO("Imported MIDI from '%s'").Format(fileName),
            XO("Import MIDI")
            );

        Viewport::Get(project).ZoomFitHorizontallyAndShowTrack(pTrack);
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
    } else {
        return false;
    }
}

bool ImportMIDI(const FilePath& fName, NoteTrack* dest)
{
    if (fName.length() <= 4) {
        AudacityMessageBox(
            XO("Could not open file %s: Filename too short.").Format(fName));
        return false;
    }

    bool is_midi = false;
    if (fName.Right(4).CmpNoCase(wxT(".mid")) == 0 || fName.Right(5).CmpNoCase(wxT(".midi")) == 0) {
        is_midi = true;
    } else if (fName.Right(4).CmpNoCase(wxT(".gro")) != 0) {
        AudacityMessageBox(
            XO("Could not open file %s: Incorrect filetype.").Format(fName));
        return false;
    }

    wxFFile mf(fName, wxT("rb"));
    if (!mf.IsOpened()) {
        AudacityMessageBox(
            XO("Could not open file %s.").Format(fName));
        return false;
    }

    double offset = 0.0;
    auto new_seq = std::make_unique<Alg_seq>(fName.mb_str(), is_midi, &offset);

    //Should we also check if(seq->tracks() == 0) ?
    if (new_seq->get_read_error() == alg_error_open) {
        AudacityMessageBox(
            XO("Could not open file %s.").Format(fName));
        mf.Close();
        return false;
    }

    dest->SetSequence(std::move(new_seq));
    dest->MoveTo(offset);
    wxString trackNameBase = fName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
    dest->SetName(trackNameBase);
    mf.Close();

    NoteTrackRange::Get(*dest).ZoomAllNotes(&dest->GetSeq());
    return true;
}
}

// Insert a menu item
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "CommonCommandFlags.h"

namespace {
using namespace MenuRegistry;

void OnImportMIDI(const CommandContext& context)
{
    auto& project = context.project;
    auto& window = GetProjectFrame(project);

    wxString fileName = SelectFile(FileNames::Operation::Open,
                                   XO("Select a MIDI file"),
                                   wxEmptyString, // Path
                                   wxT(""), // Name
                                   wxT(""), // Extension
        {
            { XO("MIDI and Allegro files"),
              { wxT("mid"), wxT("midi"), wxT("gro"), }, true },
            { XO("MIDI files"),
              { wxT("mid"), wxT("midi"), }, true },
            { XO("Allegro files"),
              { wxT("gro"), }, true },
            FileNames::AllFiles
        },
                                   wxRESIZE_BORDER, // Flags
                                   &window); // Parent

    if (!fileName.empty()) {
        DoImportMIDI(project, fileName);
    }
}

AttachedItem sAttachment{
    Command(wxT("ImportMIDI"), XXO("&MIDI..."), OnImportMIDI,
            AudioIONotBusyFlag()),
    { wxT("File/Import-Export/Import"),
      { OrderingHint::After, { "ImportAudio" } } }
};

constexpr auto exts = {
    wxT("gro"),
    wxT("midi"),
    wxT("mid"),
};

const auto DESC = XO("MIDI files");

struct MIDIImportFileHandle final : ImportFileHandle
{
public:
    explicit MIDIImportFileHandle(const FilePath& fileName)
        : mFileName{fileName}
    {}

    ~MIDIImportFileHandle() override {}

    FilePath GetFilename() const override { return mFileName; }

    TranslatableString GetFileDescription() override { return DESC; }

    ByteCount GetFileUncompressedBytes() override
    {
        // TODO: Get Uncompressed byte count.
        return 0;
    }

    wxInt32 GetStreamCount() override { return 1; }

    const TranslatableStrings& GetStreamInfo() override
    {
        static TranslatableStrings empty;
        return empty;
    }

    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    void Cancel() override {}
    void Stop() override {}

    void SetStreamUsage(wxInt32, bool) override {}

    FilePath mFileName;
};

void MIDIImportFileHandle::Import(
    ImportProgressListener& progressListener, WaveTrackFactory*,
    TrackHolders& outTracks, Tags*, std::optional<LibFileFormats::AcidizerTags>&)
{
    auto newTrack = std::make_shared<NoteTrack>();
    if (::ImportMIDI(mFileName, newTrack.get())) {
        outTracks.push_back(newTrack);
        progressListener.OnImportResult(
            ImportProgressListener::ImportResult::Success);
    } else {
        progressListener.OnImportResult(
            ImportProgressListener::ImportResult::Error);
    }
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

    std::unique_ptr<ImportFileHandle> Open(const FilePath& fileName,
                                           AudacityProject* project) override
    {
        return std::make_unique<MIDIImportFileHandle>(fileName);
    }
};

Importer::RegisteredImportPlugin registered{ "portsmf",
                                             std::make_unique<MIDIImportPlugin>()
};
}

#include "ModuleConstants.h"
DEFINE_MODULE_ENTRIES

#endif
