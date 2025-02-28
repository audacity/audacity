/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportLOF.h

  David I. Murray
  Leland Lucius

*//****************************************************************//**

\class LOFImportFileHandle
\brief An ImportFileHandle for LOF data

  Supports the opening of ".lof" files which are text files that contain
  a list of individual files to open in audacity in specific formats. Files may
  be file names (in the same directory as the LOF file), absolute paths or
  relative paths relative to the directory of the LOF file.

  (In BNF) The syntax for an LOF file, denoted by <lof>:

\verbatim
  <lof> ::= [<window> | <file> | <#>]*
  <window> ::= window [<window-parameter>]* <newline>
  <window-parameter> ::= offset <time> | duration <time>
  <time> ::= [<digit>]+ [ . [<digit>]* ]
  <file> ::= file [<file-parameter>]* <newline>
  <file-parameter> ::= offset <time>
  <#> ::= <comment> <newline>
\endverbatim

  EXAMPLE LOF file:

\verbatim
  # everything following the hash character is ignored
  window # an initial window command is implicit and optional
  file "C:\folder1\sample1.wav"    # sample1.wav is displayed
  file "C:\sample2.wav" offset 5   # sample2 is displayed with a 5s offset
  File "C:\sample3.wav"            # sample3 is displayed with no offset
  File "foo.aiff" # foo is loaded from the same directory as the LOF file
  window offset 5 duration 10      # open a NEW window, zoom to display
  # 10 seconds total starting at 5 (ending at 15) seconds
  file "C:\sample3.wav" offset 2.5
\endverbatim

  SEMANTICS:

  There are two commands: "window" creates a NEW window, and "file"
  appends a track to the current window and displays the file there. The
  first file is always placed in a NEW window, whether or not an initial
  "window" command is given.

  Commands have optional keyword parameters that may be listed in any
  order. A parameter should only occur once per command. The "offset"
  parameter specifies a time offset. For windows, this is the leftmost
  time displayed in the window. For files, the offset is an amount by
  which the file is shifted in time before display (only enabled for audio;
  not midi). The offset is specified as an integer or decimal number of
  seconds, and the default value is zero.

  Windows may also have a "duration" parameter, which specifies how much
  time should be displayed in the window. The default duration is equal
  to the duration of the longest track currently displayed.

*//****************************************************************//**

\class LOFImportPlugin
\brief An ImportPlugin for LOF data

*//*******************************************************************/

#include <wx/frame.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>

#include "FileNames.h"
#include "WaveTrack.h"
#include "ImportPlugin.h"
#include "ImportProgressListener.h"
#include "Import.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectManager.h"
#include "Viewport.h"
#include "ProjectWindows.h"
#include "ImportUtils.h"

#define BINARY_FILE_CHECK_BUFFER_SIZE 1024

#define DESC XO("List of Files in basic text format")

static const auto exts = {
    wxT("lof")
};

class LOFImportPlugin final : public ImportPlugin
{
public:
    LOFImportPlugin()
        :  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
    {
    }

    ~LOFImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("lof"); }
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject* pProject) override;
};

class LOFImportFileHandle final : public ImportFileHandle
{
public:
    LOFImportFileHandle(AudacityProject* pProject, const FilePath& name, std::unique_ptr<wxTextFile>&& file);
    ~LOFImportFileHandle();

    TranslatableString GetFileDescription() override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    FilePath GetFilename() const override;

    void Cancel() override;

    void Stop() override;

    wxInt32 GetStreamCount() override { return 1; }

    const TranslatableStrings& GetStreamInfo() override
    {
        static TranslatableStrings empty;
        return empty;
    }

    void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)) override
    {}

private:
    // Takes a line of text in lof file and interprets it and opens files
    void lofOpenFiles(wxString* ln);
    void doDurationAndScrollOffset();

    std::unique_ptr<wxTextFile> mTextFile;
    const wxFileName mLOFFileName; /**< The name of the LOF file, which is used to
                                interpret relative paths in it */
    AudacityProject* mProject{};

    // In order to know whether or not to create a NEW window
    int nFilesInGroup{ 0 };

    // In order to zoom in, it must be done after files are opened
    bool callDurationFactor{ false };
    double durationFactor{ 1 };

    // In order to offset scrollbar, it must be done after files are opened
    bool callScrollOffset{ false };
    double scrollOffset{ 0 };
};

LOFImportFileHandle::LOFImportFileHandle(AudacityProject* pProject,
                                         const FilePath& name, std::unique_ptr<wxTextFile>&& file)
    :  mTextFile(std::move(file))
    , mLOFFileName{name}
    , mProject{pProject}
{
}

TranslatableString LOFImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> LOFImportPlugin::Open(
    const FilePath& filename, AudacityProject* pProject)
{
    // Check if it is a text file.
    {
        wxFile binaryFile;
        if (!binaryFile.Open(filename)) {
            return nullptr; // File not found
        }
        char buf[BINARY_FILE_CHECK_BUFFER_SIZE];
        int count = binaryFile.Read(buf, BINARY_FILE_CHECK_BUFFER_SIZE);

        bool isTextFile = false;
        const std::string lofToken("file");

        // At least we should get a size more than: <token> + <space> + <filename>.
        if (count > (lofToken.length() + sizeof(' ') + 1)) {
            // Audacity can import list from LOF only with BOM or ASCII,
            // each other text (like unicode without BOM, bin, etc) can't be recognized.

            // UTF-16 BOM checker.
            auto IsUtf16_BE = [](const char* str) -> bool
            {
                return str[0] == static_cast<char>(0xFE) && str[1] == static_cast<char>(0xFF);
            };
            auto IsUtf16_LE = [](const char* str) -> bool
            {
                return str[0] == static_cast<char>(0xFF) && str[1] == static_cast<char>(0xFE);
            };

            // UTF-32 BOM checker.
            auto IsUtf32_BE = [](const char* str) -> bool
            {
                return str[0] == static_cast<char>(0x00)
                       && str[1] == static_cast<char>(0x00)
                       && str[2] == static_cast<char>(0xFE)
                       && str[3] == static_cast<char>(0xFF);
            };
            auto IsUtf32_LE = [](const char* str) -> bool
            {
                return str[0] == static_cast<char>(0xFF)
                       && str[1] == static_cast<char>(0xFE)
                       && str[2] == static_cast<char>(0x00)
                       && str[3] == static_cast<char>(0x00);
            };

            // Is unicode text file.
            if (IsUtf16_BE(buf) || IsUtf16_LE(buf) || IsUtf32_BE(buf) || IsUtf32_LE(buf)) {
                isTextFile = true;
            }
            // Try parse as ASCII and UTF-8 text as ASCII too.
            else {
                buf[sizeof(buf) - 1] = '\0';

                std::string importedText(buf);

                if (importedText.find(lofToken) != std::string::npos) {
                    isTextFile = true;
                }
            }
        }

        if (!isTextFile) {
            binaryFile.Close();
            return nullptr;
        }
    }

    // Now open the file again as text file
    auto file = std::make_unique<wxTextFile>(filename);
    file->Open();

    if (!file->IsOpened()) {
        return nullptr;
    }

    return std::make_unique<LOFImportFileHandle>(
        pProject, filename, std::move(file));
}

TranslatableString LOFImportFileHandle::GetFileDescription()
{
    return DESC;
}

auto LOFImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
    return 0;
}

void LOFImportFileHandle::Import(
    ImportProgressListener& progressListener, WaveTrackFactory*,
    TrackHolders& outTracks, Tags*, std::optional<LibFileFormats::AcidizerTags>&)
{
    // Unlike other ImportFileHandle subclasses, this one never gives any tracks
    // back to the caller.
    // Instead, it recursively calls AudacityProject::Import for each file listed
    // in the .lof file.
    // Each importation creates a NEW undo state.
    // If there is an error or exception during one of them, only that one's
    // side effects are rolled back, and the rest of the import list is skipped.
    // The file may have "window" directives that cause NEW AudacityProjects
    // to be created, and the undo states are pushed onto the latest project.
    // If a project is created but the first file import into it fails, destroy
    // the project.

    outTracks.clear();

    wxASSERT(mTextFile->IsOpened());

    if (mTextFile->Eof()) {
        mTextFile->Close();
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Error);
        return;
    }

    wxString line = mTextFile->GetFirstLine();

    while (!mTextFile->Eof())
    {
        lofOpenFiles(&line);
        line = mTextFile->GetNextLine();
    }

    // for last line
    lofOpenFiles(&line);

    if (!mTextFile->Close()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Error);
        return;
    }
    // set any duration/offset factors for last window, as all files were called
    doDurationAndScrollOffset();
    progressListener.OnImportResult(ImportProgressListener::ImportResult::Success);
}

FilePath LOFImportFileHandle::GetFilename() const
{
    return mLOFFileName.GetFullPath();
}

void LOFImportFileHandle::Cancel()
{
    //LOFImport delegates import to other plugins
}

void LOFImportFileHandle::Stop()
{
    //LOFImport delegates import to other plugins
}

static Importer::RegisteredImportPlugin registered{ "LOF",
                                                    std::make_unique< LOFImportPlugin >()
};

/** @brief Processes a single line from a LOF text file, doing whatever is
 * indicated on the line.
 *
 * This function should just return for lines it cannot deal with, and the
 * caller will continue to the next line of the input file
 */
void LOFImportFileHandle::lofOpenFiles(wxString* ln)
{
    wxStringTokenizer tok(*ln, wxT(" "));
    wxStringTokenizer temptok1(*ln, wxT("\""));
    wxStringTokenizer temptok2(*ln, wxT(" "));
    int tokenplace = 0;

    wxString targetfile;
    wxString tokenholder = tok.GetNextToken();

    if (tokenholder.IsSameAs(wxT("window"), false)) {
        // set any duration/offset factors for last window, as all files were called
        doDurationAndScrollOffset();

        if (nFilesInGroup > 0) {
            // Cause a project to be created with the next import
            mProject = nullptr;
        }

        nFilesInGroup = 0;

        while (tok.HasMoreTokens())
        {
            tokenholder = tok.GetNextToken();

            if (tokenholder.IsSameAs(wxT("offset"), false)) {
                if (tok.HasMoreTokens()) {
                    tokenholder = tok.GetNextToken();
                }

                if (Internat::CompatibleToDouble(tokenholder, &scrollOffset)) {
                    callScrollOffset = true;
                } else {
                    ImportUtils::ShowMessageBox(
                        /* i18n-hint: You do not need to translate "LOF" */
                        XO("Invalid window offset in LOF file."));
                }

                if (tok.HasMoreTokens()) {
                    tokenholder = tok.GetNextToken();
                }
            }

            if (tokenholder.IsSameAs(wxT("duration"), false)) {
                if (tok.HasMoreTokens()) {
                    tokenholder = tok.GetNextToken();
                }

                if (Internat::CompatibleToDouble(tokenholder, &durationFactor)) {
                    callDurationFactor = true;
                } else {
                    ImportUtils::ShowMessageBox(
                        /* i18n-hint: You do not need to translate "LOF" */
                        XO("Invalid duration in LOF file."));
                }
            }  // End if statement

            if (tokenholder == wxT("#")) {
                // # indicates comments; ignore line
                tok = wxStringTokenizer(wxT(""), wxT(" "));
            }
        }   // End while loop
    }       // End if statement handling "window" lines
    else if (tokenholder.IsSameAs(wxT("file"), false)) {
        nFilesInGroup++;
        // To identify filename and open it
        tokenholder = temptok1.GetNextToken();
        wxString targettoken = temptok1.GetNextToken();
        targetfile = targettoken;

        // If path is relative, make absolute path from LOF path
        if (!wxIsAbsolutePath(targetfile)) {
            wxFileName fName(targetfile);
            fName.Normalize(wxPATH_NORM_ALL, mLOFFileName.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR));
            if (fName.FileExists()) {
                targetfile = fName.GetFullPath();
            }
        }

        // Do recursive call to import
        mProject = ProjectManager::OpenProject(mProject, targetfile,
                                               true /* addtohistory */, true /* reuseNonemptyProject */);

        // Set tok to right after filename
        temptok2.SetString(targettoken);
        tokenplace = temptok2.CountTokens();

        for (int i = 0; i < tokenplace; i++) {
            tokenholder = tok.GetNextToken();
        }

        if (tok.HasMoreTokens()) {
            tokenholder = tok.GetNextToken();

            if (tokenholder == wxT("#")) {
                // # indicates comments; ignore line
                tok = wxStringTokenizer(wxT(""), wxT(" "));
            }

            if (tokenholder.IsSameAs(wxT("offset"), false)) {
                if (tok.HasMoreTokens()) {
                    tokenholder = tok.GetNextToken();
                }
                double offset;

                // handle an "offset" specifier
                if (!mProject) {
                    // there was an import error,
                    // presumably with its own error message
                } else if (Internat::CompatibleToDouble(tokenholder, &offset)) {
                    auto& tracks = TrackList::Get(*mProject);
                    auto t = *tracks.rbegin();

                    // t is now the last track in the project, unless the import of
                    // all tracks failed, in which case it will be null. In that
                    // case we return because we cannot offset a non-existent track.
                    if (t == NULL) {
                        return;
                    }
#ifdef USE_MIDI
                    if (targetfile.AfterLast(wxT('.')).IsSameAs(wxT("mid"), false)
                        || targetfile.AfterLast(wxT('.')).IsSameAs(wxT("midi"), false)) {
                        ImportUtils::ShowMessageBox(XO("MIDI tracks cannot be offset individually, only audio files can be."));
                    } else
#endif
                    t->MoveTo(offset);

                    // Amend the undo transaction made by import
                    ProjectHistory::Get(*mProject).ModifyState(false);
                } // end of converting "offset" argument
                else {
                    ImportUtils::ShowMessageBox(
                        /* i18n-hint: You do not need to translate "LOF" */
                        XO("Invalid track offset in LOF file."));
                }
            }  // End if statement for "offset" parameters
        }   // End if statement (more tokens after file name)
    }    // End if statement "file" lines
    else if (tokenholder == wxT("#")) {
        // # indicates comments; ignore line
        tok = wxStringTokenizer(wxT(""), wxT(" "));
    } else {
        // Couldn't parse a line
    }
}

void LOFImportFileHandle::doDurationAndScrollOffset()
{
    if (!mProject) {
        return;
    }

    callScrollOffset = callScrollOffset && (scrollOffset != 0);
    bool doSomething = callDurationFactor || callScrollOffset;

    if (callDurationFactor) {
        double longestDuration = TrackList::Get(*mProject).GetEndTime();
        Viewport::Get(*mProject).ZoomBy(longestDuration / durationFactor);
        callDurationFactor = false;
    }

    if (callScrollOffset) {
        Viewport::Get(*mProject).SetHorizontalThumb(scrollOffset);
        callScrollOffset = false;
    }

    if (doSomething) {
        // Amend last undo state
        ProjectHistory::Get(*mProject).ModifyState(false);
    }
}

LOFImportFileHandle::~LOFImportFileHandle()
{
}
