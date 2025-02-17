/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.cpp

  Joshua Haberman

  This code allows Audacity to export data by piping it to an external
  program.

**********************************************************************/

#include "ProjectRate.h"

#include <thread>

#include <wx/app.h>
#include <wx/cmdline.h>
#include <wx/combobox.h>
#include <wx/button.h>
#include <wx/log.h>
#include <wx/process.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#if defined(__WXMSW__)
#include <wx/msw/registry.h> // for wxRegKey
#endif

#include "FileNames.h"
#include "Export.h"

#include "Mix.h"
#include "Prefs.h"
#include "SelectFile.h"
#include "ShuttleGui.h"
#include "Tags.h"
#include "Track.h"
#include "float_cast.h"
#include "widgets/FileHistory.h"
#include "wxPanelWrapper.h"
#include "widgets/Warning.h"
#include "wxFileNameWrapper.h"
#include "BasicUI.h"

#include "ExportOptionsEditor.h"
#include "ExportOptionsUIServices.h"
#include "ExportPluginHelpers.h"
#include "ExportPluginRegistry.h"

#ifdef USE_LIBID3TAG
   #include <id3tag.h>
extern "C" {
struct id3_frame* id3_frame_new(char const*);
}
#endif

namespace {
void Drain(wxInputStream* s, wxString* o)
{
    while (s->CanRead()) {
        char buffer[4096];

        s->Read(buffer, WXSIZEOF(buffer) - 1);
        buffer[s->LastRead()] = wxT('\0');
        *o += LAT1CTOWX(buffer);
    }
}

struct ExtendPath
{
#if defined(__WXMSW__)
    wxString opath;

    ExtendPath()
    {
        // Give Windows a chance at finding lame command in the default location.
        wxString paths[] = { wxT("HKEY_LOCAL_MACHINE\\Software\\Lame for Audacity"),
                             wxT("HKEY_LOCAL_MACHINE\\Software\\FFmpeg for Audacity") };
        wxString npath;
        wxRegKey reg;

        wxGetEnv(wxT("PATH"), &opath);
        npath = opath;

        for (int i = 0; i < WXSIZEOF(paths); i++) {
            reg.SetName(paths[i]);

            if (reg.Exists()) {
                wxString ipath;
                reg.QueryValue(wxT("InstallPath"), ipath);
                if (!ipath.empty()) {
                    npath += wxPATH_SEP + ipath;
                }
            }
        }

        wxSetEnv(wxT("PATH"), npath);
    }

    ~ExtendPath()
    {
        if (!opath.empty()) {
            wxSetEnv(wxT("PATH"), opath);
        }
    }

#endif
};

//----------------------------------------------------------------------------
// ExportCLProcess
//----------------------------------------------------------------------------

class ExportCLProcess final : public wxProcess
{
public:
    ExportCLProcess(wxString* output)
    {
#if defined(__WXMAC__)
        // Don't want to crash on broken pipe
        signal(SIGPIPE, SIG_IGN);
#endif

        mOutput = output;
        mActive = true;
        mStatus = -555;
        Redirect();
    }

    bool IsActive() const
    {
        return mActive;
    }

    void OnTerminate(int WXUNUSED(pid), int status) override
    {
        Drain(GetInputStream(), mOutput);
        Drain(GetErrorStream(), mOutput);

        mStatus = status;
        mActive = false;
    }

    int GetStatus() const
    {
        return mStatus;
    }

private:
    wxString* mOutput;
    bool mActive;
    int mStatus;
};
}

enum : int {
    CLOptionIDCommand = 0,
    CLOptionIDShowOutput
};

const std::vector<ExportOption> CLOptions {
    { CLOptionIDCommand, {}, std::string() },
    { CLOptionIDShowOutput, {}, false }
};

class ExportOptionsCLEditor final : public ExportOptionsEditor, public ExportOptionsUIServices
{
    wxString mCommand { wxT("lame - \"%f\"") };
    bool mShowOutput { false };
    bool mInitialized { false };
public:

    ExportOptionsCLEditor() = default;

    void PopulateUI(ShuttleGui& S) override
    {
        if (!mInitialized) {
            mHistory.Load(*gPrefs, wxT("/FileFormats/ExternalProgramHistory"));

            if (mHistory.empty()) {
                mHistory.Append(wxT("ffmpeg -i - \"%f.opus\""));
                mHistory.Append(wxT("ffmpeg -i - \"%f.wav\""));
                mHistory.Append(wxT("ffmpeg -i - \"%f\""));
                mHistory.Append(wxT("lame - \"%f\""));
            }

            if (!mCommand.empty()) {
                mHistory.Append(mCommand);
            }

            mInitialized = true;
        }

        mParent = wxGetTopLevelParent(S.GetParent());

        wxArrayStringEx cmds(mHistory.begin(), mHistory.end());
        auto cmd = cmds[0];

        S.StartVerticalLay();
        {
            S.StartHorizontalLay(wxEXPAND);
            {
                S.SetSizerProportion(1);
                S.StartMultiColumn(3, wxEXPAND);
                {
                    S.SetStretchyCol(1);
                    mCommandBox = S.AddCombo(XXO("Command:"),
                                             cmd,
                                             cmds);
                    mCommandBox->Bind(wxEVT_TEXT, [this](wxCommandEvent& event) {
                        mLastCommand = event.GetString();
                    });
                    mLastCommand = mCommandBox->GetValue();
                    mCommandBox->SetMaxSize(wxSize(50, 400));

                    S.AddButton(XXO("Browse..."), wxALIGN_CENTER_VERTICAL)
                    ->Bind(wxEVT_BUTTON, &ExportOptionsCLEditor::OnBrowse, this);

                    S.AddFixedText({});
                    S.TieCheckBox(XXO("Show output"), mShowOutput);
                }
                S.EndMultiColumn();
            }
            S.EndHorizontalLay();

            S.AddTitle(XO(
                           /* i18n-hint: Some programmer-oriented terminology here:
                              "Data" refers to the sound to be exported, "piped" means sent,
                              and "standard in" means the default input stream that the external program,
                              named by %f, will read.  And yes, it's %f, not %s -- this isn't actually used
                              in the program as a format string.  Keep %f unchanged. */
                           "Data will be piped to standard in. \"%f\" uses the file name in the export window."), 250);
        }
        S.EndVerticalLay();
    }

    static bool IsValidCommand(const wxString& command)
    {
        wxArrayString argv = wxCmdLineParser::ConvertStringToArgs(command,
#if defined(__WXMSW__)
                                                                  wxCMD_LINE_SPLIT_DOS
#else
                                                                  wxCMD_LINE_SPLIT_UNIX
#endif
                                                                  );

        if (argv.empty()) {
            ShowExportErrorDialog(
                XO("Warning"),
                XO("Program name appears to be missing."),//":745"
                true);
            return false;
        }

        // Normalize the path (makes absolute and resolves variables)
        wxFileName cmd(argv[0]);
        cmd.Normalize(wxPATH_NORM_ALL & ~wxPATH_NORM_ABSOLUTE);

        // Just verify the given path exists if it is absolute.
        if (cmd.IsAbsolute()) {
            if (!cmd.Exists()) {
                BasicUI::ShowMessageBox(XO("\"%s\" couldn't be found.").Format(cmd.GetFullPath()),
                                        BasicUI::MessageBoxOptions()
                                        .IconStyle(BasicUI::Icon::Warning)
                                        .Caption(XO("Warning")));
                return false;
            }

            return true;
        }

        // Search for the command in the PATH list
        wxPathList pathlist;
        pathlist.AddEnvList(wxT("PATH"));
        wxString path = pathlist.FindAbsoluteValidPath(argv[0]);

   #if defined(__WXMSW__)
        if (path.empty()) {
            path = pathlist.FindAbsoluteValidPath(argv[0] + wxT(".exe"));
        }
   #endif

        if (path.empty()) {
            BasicUI::ShowMessageBox(XO("Unable to locate \"%s\" in your path.").Format(cmd.GetFullPath()),
                                    BasicUI::MessageBoxOptions()
                                    .IconStyle(BasicUI::Icon::Warning)
                                    .Caption(XO("Warning")));
            return false;
        }

        return true;
    }

    bool TransferDataFromWindow() override
    {
        if (IsValidCommand(mLastCommand)) {
            mCommand = mLastCommand;
            mHistory.Append(mCommand);
            mHistory.Save(*gPrefs);
            return true;
        }
        return false;
    }

    SampleRateList GetSampleRateList() const override
    {
        return {};
    }

    int GetOptionsCount() const override
    {
        return static_cast<int>(CLOptions.size());
    }

    bool GetOption(int index, ExportOption& option) const override
    {
        if (index >= 0 && index < static_cast<int>(CLOptions.size())) {
            option = CLOptions[index];
            return true;
        }
        return false;
    }

    bool GetValue(int id, ExportValue& value) const override
    {
        if (id == CLOptionIDCommand) {
            value = std::string(mCommand.ToUTF8());
            return true;
        }
        if (id == CLOptionIDShowOutput) {
            value = mShowOutput;
            return true;
        }
        return false;
    }

    bool SetValue(int id, const ExportValue& value) override
    {
        if (id == CLOptionIDCommand && std::holds_alternative<std::string>(value)) {
            mCommand = wxString::FromUTF8(*std::get_if<std::string>(&value));
            return true;
        }
        if (id == CLOptionIDShowOutput && std::holds_alternative<bool>(value)) {
            mShowOutput = *std::get_if<bool>(&value);
            return true;
        }
        return false;
    }

    void Load(const audacity::BasicSettings& config) override
    {
        mCommand = config.Read(wxT("/FileFormats/ExternalProgramExportCommand"), mCommand);
        mShowOutput = config.Read(wxT("/FileFormats/ExternalProgramShowOutput"), mShowOutput);
    }

    void Store(audacity::BasicSettings& config) const override
    {
        config.Write(wxT("/FileFormats/ExternalProgramExportCommand"), mCommand);
        config.Write(wxT("/FileFormats/ExternalProgramShowOutput"), mShowOutput);
    }

private:

    void OnBrowse(const wxCommandEvent&)
    {
        wxString path;
        FileExtension ext;
        FileNames::FileType type = FileNames::AllFiles;

   #if defined(__WXMSW__)
        ext = wxT("exe");
        /* i18n-hint files that can be run as programs */
        type = { XO("Executables"), { ext } };
   #endif

        path = SelectFile(FileNames::Operation::Open,
                          XO("Find path to command"),
                          wxEmptyString,
                          wxEmptyString,
                          ext,
                          { type },
                          wxFD_OPEN | wxRESIZE_BORDER,
                          mParent);
        if (path.empty()) {
            return;
        }

        if (path.Find(wxT(' ')) != wxNOT_FOUND) {
            path = wxT('"') + path + wxT('"');
        }

        mCommandBox->SetValue(path);
        mCommandBox->SetInsertionPointEnd();
    }

    wxWindow* mParent{ nullptr };
    wxComboBox* mCommandBox{ nullptr };

    //Caches latest value in mCommandBox.
    //Currently mCommandBox isn't available from
    //`TransferDataFromWindow` since parent window is destroyed.
    wxString mLastCommand;

    FileHistory mHistory;
};

class CLExportProcessor : public ExportProcessor
{
    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        unsigned channels;
        wxString cmd;
        bool showOutput;
        std::unique_ptr<Mixer> mixer;
        wxString output;
        std::unique_ptr<ExportCLProcess> process;
    } context;
public:

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double rate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:

    static std::vector<char> GetMetaChunk(const Tags* metadata);
};

class ExportCL final : public ExportPlugin
{
public:

    ExportCL() = default;

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

int ExportCL::GetFormatCount() const
{
    return 1;
}

FormatInfo ExportCL::GetFormatInfo(int) const
{
    return {
        wxT("CL"), XO("(external program)"), { "" }, 255, false
    };
}

std::unique_ptr<ExportOptionsEditor>
ExportCL::CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const
{
    return std::make_unique<ExportOptionsCLEditor>();
}

std::unique_ptr<ExportProcessor> ExportCL::CreateProcessor(int format) const
{
    return std::make_unique<CLExportProcessor>();
}

bool CLExportProcessor::Initialize(AudacityProject& project,
                                   const Parameters& parameters,
                                   const wxFileNameWrapper& fName,
                                   double t0, double t1, bool selectionOnly,
                                   double sampleRate, unsigned channels,
                                   MixerOptions::Downmix* mixerSpec,
                                   const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;
    context.channels = channels;

    ExtendPath ep;
    long rc;

    const auto path = fName.GetFullPath();

    context.cmd = wxString::FromUTF8(ExportPluginHelpers::GetParameterValue<std::string>(parameters, CLOptionIDCommand));
    context.showOutput = ExportPluginHelpers::GetParameterValue(parameters, CLOptionIDShowOutput, false);

    // Bug 2178 - users who don't know what they are doing will
    // now get a file extension of .wav appended to their ffmpeg filename
    // and therefore ffmpeg will be able to choose a file type.
    if (context.cmd == wxT("ffmpeg -i - \"%f\"") && !fName.HasExt()) {
        context.cmd.Replace("%f", "%f.wav");
    }
    context.cmd.Replace(wxT("%f"), path);

    // Kick off the command
    context.process = std::make_unique<ExportCLProcess>(&context.output);
    auto& process = *context.process;

    rc = wxExecute(context.cmd, wxEXEC_ASYNC, &process);
    if (!rc) {
        process.Detach();
        process.CloseOutput();
        throw ExportException(XO("Cannot export audio to %s")
                              .Format(path)
                              .Translation());
    }

    // Turn off logging to prevent broken pipe messages
    wxLogNull nolog;

    // establish parameters
    int rate = lrint(sampleRate);
    const size_t maxBlockLen = 44100 * 5;
    unsigned long totalSamples = lrint((t1 - t0) * rate);
    unsigned long sampleBytes = totalSamples * channels * SAMPLE_SIZE(floatSample);

    wxOutputStream* os = process.GetOutputStream();

    // RIFF header
    struct {
        char riffID[4];          // "RIFF"
        wxUint32 riffLen;        // basically the file len - 8
        char riffType[4];        // "WAVE"
    } riff;

    // format chunk */
    struct {
        char fmtID[4];           // "fmt " */
        wxUint32 formatChunkLen; // (format chunk len - first two fields) 16 in our case
        wxUint16 formatTag;      // 1 for PCM
        wxUint16 channels;
        wxUint32 sampleRate;
        wxUint32 avgBytesPerSec; // sampleRate * blockAlign
        wxUint16 blockAlign;     // bitsPerSample * channels (assume bps % 8 = 0)
        wxUint16 bitsPerSample;
    } fmt;

    // id3 chunk header
    struct {
        char id3ID[4];           // "id3 "
        wxUint32 id3Len;         // length of metadata in bytes
    } id3;

    // data chunk header
    struct {
        char dataID[4];          // "data"
        wxUint32 dataLen;        // length of all samples in bytes
    } data;

    riff.riffID[0] = 'R';
    riff.riffID[1] = 'I';
    riff.riffID[2] = 'F';
    riff.riffID[3] = 'F';
    riff.riffLen   = wxUINT32_SWAP_ON_BE(sizeof(riff)
                                         + sizeof(fmt)
                                         + sizeof(data)
                                         + sampleBytes
                                         - 8);
    riff.riffType[0]  = 'W';
    riff.riffType[1]  = 'A';
    riff.riffType[2]  = 'V';
    riff.riffType[3]  = 'E';

    fmt.fmtID[0]        = 'f';
    fmt.fmtID[1]        = 'm';
    fmt.fmtID[2]        = 't';
    fmt.fmtID[3]        = ' ';
    fmt.formatChunkLen  = wxUINT32_SWAP_ON_BE(16);
    fmt.formatTag       = wxUINT16_SWAP_ON_BE(3);
    fmt.channels        = wxUINT16_SWAP_ON_BE(channels);
    fmt.sampleRate      = wxUINT32_SWAP_ON_BE(rate);
    fmt.bitsPerSample   = wxUINT16_SWAP_ON_BE(SAMPLE_SIZE(floatSample) * 8);
    fmt.blockAlign      = wxUINT16_SWAP_ON_BE(fmt.bitsPerSample * fmt.channels / 8);
    fmt.avgBytesPerSec  = wxUINT32_SWAP_ON_BE(fmt.sampleRate * fmt.blockAlign);

    // Retrieve tags if not given a set
    if (metadata == nullptr) {
        metadata = &Tags::Get(project);
    }
    const auto metachunk = GetMetaChunk(metadata);

    if (!metachunk.empty()) {
        id3.id3ID[0] = 'i';
        id3.id3ID[1] = 'd';
        id3.id3ID[2] = '3';
        id3.id3ID[3] = ' ';
        id3.id3Len   = wxUINT32_SWAP_ON_BE(metachunk.size());
        riff.riffLen += sizeof(id3) + metachunk.size();
    }

    data.dataID[0] = 'd';
    data.dataID[1] = 'a';
    data.dataID[2] = 't';
    data.dataID[3] = 'a';
    data.dataLen   = wxUINT32_SWAP_ON_BE(sampleBytes);

    // write the headers and metadata
    os->Write(&riff, sizeof(riff));
    os->Write(&fmt, sizeof(fmt));
    if (!metachunk.empty()) {
        os->Write(&id3, sizeof(id3));
        os->Write(metachunk.data(), metachunk.size());
    }
    os->Write(&data, sizeof(data));

    // Mix 'em up
    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, channels, maxBlockLen, true, rate,
        floatSample, mixerSpec);

    context.status = selectionOnly
                     ? XO("Exporting the selected audio using command-line encoder")
                     : XO("Exporting the audio using command-line encoder");

    return true;
}

ExportResult CLExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);
    auto& process = *context.process;
    auto exportResult = ExportResult::Success;
    {
        size_t numBytes = 0;
        constSamplePtr mixed = nullptr;

        wxOutputStream* os = process.GetOutputStream();
        auto closeIt = finally([&] {
            // Should make the process die, before propagating any exception
            process.CloseOutput();
        });

        // Start piping the mixed data to the command
        while (exportResult == ExportResult::Success && process.IsActive() && os->IsOk()) {
            // Capture any stdout and stderr from the command
            Drain(process.GetInputStream(), &context.output);
            Drain(process.GetErrorStream(), &context.output);

            // Need to mix another block
            if (numBytes == 0) {
                auto numSamples = context.mixer->Process();
                if (numSamples == 0) {
                    break;
                }

                mixed = context.mixer->GetBuffer();
                numBytes = numSamples * context.channels;

                // Byte-swapping is necessary on big-endian machines, since
                // WAV files are little-endian
#if wxBYTE_ORDER == wxBIG_ENDIAN
                auto buffer = (const float*)mixed;
                for (int i = 0; i < numBytes; i++) {
                    buffer[i] = wxUINT32_SWAP_ON_BE(buffer[i]);
                }
#endif
                numBytes *= SAMPLE_SIZE(floatSample);
            }

            // Don't write too much at once...pipes may not be able to handle it
            size_t bytes = wxMin(numBytes, 4096);
            numBytes -= bytes;

            while (bytes > 0) {
                os->Write(mixed, bytes);
                if (!os->IsOk()) {
                    exportResult = ExportResult::Error;
                    break;
                }
                bytes -= os->LastWrite();
                mixed += os->LastWrite();
            }

            if (exportResult == ExportResult::Success) {
                exportResult = ExportPluginHelpers::UpdateProgress(
                    delegate, *context.mixer, context.t0, context.t1);
            }
        }
        // Done with the progress display
    }

    // Wait for process to terminate
    while (process.IsActive()) {
        using namespace std::chrono;
        std::this_thread::sleep_for(10ms);
        BasicUI::Yield();
    }

    // Display output on error or if the user wants to see it
    if (process.GetStatus() != 0 || context.showOutput) {
        // TODO use ShowInfoDialog() instead.
        BasicUI::CallAfter([cmd = context.cmd, output = std::move(context.output)]
        {
            wxDialogWrapper dlg(nullptr,
                                wxID_ANY,
                                XO("Command Output"),
                                wxDefaultPosition,
                                wxSize(600, 400),
                                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
            dlg.SetName();

            ShuttleGui S(&dlg, eIsCreating);
            S
            .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
            .AddTextWindow(cmd + wxT("\n\n") + output);
            S.StartHorizontalLay(wxALIGN_CENTER, false);
            {
                S.Id(wxID_OK).AddButton(XXO("&OK"), wxALIGN_CENTER, true);
            }
            dlg.GetSizer()->AddSpacer(5);
            dlg.Layout();
            dlg.SetMinSize(dlg.GetSize());
            dlg.Center();

            dlg.ShowModal();
        });

        if (process.GetStatus() != 0) {
            exportResult = ExportResult::Error;
        }
    }

    return exportResult;
}

std::vector<char> CLExportProcessor::GetMetaChunk(const Tags* tags)
{
    std::vector<char> buffer;

#ifdef USE_LIBID3TAG
    struct id3_tag_deleter {
        void operator ()(id3_tag* p) const
        {
            if (p) {
                id3_tag_delete(p);
            }
        }
    };

    std::unique_ptr<id3_tag, id3_tag_deleter> tp { id3_tag_new() };

    for (const auto& pair : tags->GetRange()) {
        const auto& n = pair.first;
        const auto& v = pair.second;
        const char* name = "TXXX";

        if (n.CmpNoCase(TAG_TITLE) == 0) {
            name = ID3_FRAME_TITLE;
        } else if (n.CmpNoCase(TAG_ARTIST) == 0) {
            name = ID3_FRAME_ARTIST;
        } else if (n.CmpNoCase(TAG_ALBUM) == 0) {
            name = ID3_FRAME_ALBUM;
        } else if (n.CmpNoCase(TAG_YEAR) == 0) {
            name = ID3_FRAME_YEAR;
        } else if (n.CmpNoCase(TAG_GENRE) == 0) {
            name = ID3_FRAME_GENRE;
        } else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
            name = ID3_FRAME_COMMENT;
        } else if (n.CmpNoCase(TAG_TRACK) == 0) {
            name = ID3_FRAME_TRACK;
        } else if (n.CmpNoCase(wxT("composer")) == 0) {
            name = "TCOM";
        }

        struct id3_frame* frame = id3_frame_new(name);

        if (!n.IsAscii() || !v.IsAscii()) {
            id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
        } else {
            id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
        }

        MallocString<id3_ucs4_t> ucs4{
            id3_utf8_ucs4duplicate((id3_utf8_t*)(const char*)v.mb_str(wxConvUTF8)) };

        if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
            // A hack to get around iTunes not recognizing the comment.  The
            // language defaults to XXX and, since it's not a valid language,
            // iTunes just ignores the tag.  So, either set it to a valid language
            // (which one???) or just clear it.  Unfortunately, there's no supported
            // way of clearing the field, so do it directly.
            id3_field* f = id3_frame_field(frame, 1);
            memset(f->immediate.value, 0, sizeof(f->immediate.value));
            id3_field_setfullstring(id3_frame_field(frame, 3), ucs4.get());
        } else if (strcmp(name, "TXXX") == 0) {
            id3_field_setstring(id3_frame_field(frame, 2), ucs4.get());

            ucs4.reset(id3_utf8_ucs4duplicate((id3_utf8_t*)(const char*)n.mb_str(wxConvUTF8)));

            id3_field_setstring(id3_frame_field(frame, 1), ucs4.get());
        } else {
            auto addr = ucs4.get();
            id3_field_setstrings(id3_frame_field(frame, 1), 1, &addr);
        }

        id3_tag_attachframe(tp.get(), frame);
    }

    tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

    // If this version of libid3tag supports it, use v2.3 ID3
    // tags instead of the newer, but less well supported, v2.4
    // that libid3tag uses by default.
#ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
    tp->options |= ID3_TAG_OPTION_ID3V2_3;
#endif

    id3_length_t len;

    len = id3_tag_render(tp.get(), 0);
    if ((len % 2) != 0) {
        len++; // Length must be even.
    }

    if (len > 0) {
        buffer.resize(len);
        id3_tag_render(tp.get(), (id3_byte_t*)buffer.data());
    }
#endif

    return buffer;
}

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "CommandLine",
                                                                 []{ return std::make_unique< ExportCL >(); }
};
