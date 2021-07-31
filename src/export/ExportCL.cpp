/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.cpp

  Joshua Haberman

  This code allows Audacity to export data by piping it to an external
  program.

**********************************************************************/



#include "../ProjectSettings.h"

#include <wx/app.h>
#include <wx/button.h>
#include <wx/cmdline.h>
#include <wx/combobox.h>
#include <wx/log.h>
#include <wx/process.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#if defined(__WXMSW__)
#include <wx/msw/registry.h> // for wxRegKey
#endif

#include "FileNames.h"
#include "Export.h"

#include "../Mix.h"
#include "Prefs.h"
#include "../SelectFile.h"
#include "../ShuttleGui.h"
#include "../Tags.h"
#include "../Track.h"
#include "float_cast.h"
#include "../widgets/FileHistory.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/Warning.h"
#include "wxFileNameWrapper.h"

#ifdef USE_LIBID3TAG
   #include <id3tag.h>
   extern "C" {
      struct id3_frame *id3_frame_new(char const *);
   }
#endif

//----------------------------------------------------------------------------
// ExportCLOptions
//----------------------------------------------------------------------------

class ExportCLOptions final : public wxPanelWrapper
{
public:
   ExportCLOptions(wxWindow *parent, int format);
   virtual ~ExportCLOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   void OnBrowse(wxCommandEvent & event);

private:
   wxComboBox *mCmd;
   FileHistory mHistory;

   DECLARE_EVENT_TABLE()
};

#define ID_BROWSE 5000

BEGIN_EVENT_TABLE(ExportCLOptions, wxPanelWrapper)
   EVT_BUTTON(ID_BROWSE, ExportCLOptions::OnBrowse)
END_EVENT_TABLE()

///
///
ExportCLOptions::ExportCLOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   mHistory.Load(*gPrefs, wxT("/FileFormats/ExternalProgramHistory"));

   if (mHistory.empty()) {
      mHistory.Append(wxT("ffmpeg -i - \"%f.opus\""));
      mHistory.Append(wxT("ffmpeg -i - \"%f.wav\""));
      mHistory.Append(wxT("ffmpeg -i - \"%f\""));
      mHistory.Append(wxT("lame - \"%f\""));
   }

   mHistory.Append(gPrefs->Read(wxT("/FileFormats/ExternalProgramExportCommand"),
                                          mHistory[ 0 ]));

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();

   parent->Layout();
}

ExportCLOptions::~ExportCLOptions()
{
   TransferDataFromWindow();
}

///
///
void ExportCLOptions::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayStringEx cmds( mHistory.begin(), mHistory.end() );
   auto cmd = cmds[0];

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetSizerProportion(1);
         S.StartMultiColumn(3, wxEXPAND);
         {
            S.SetStretchyCol(1);
            mCmd = S.AddCombo(XXO("Command:"),
                              cmd,
                              cmds);
            S.Id(ID_BROWSE).AddButton(XXO("Browse..."),
                                      wxALIGN_CENTER_VERTICAL);
            S.AddFixedText( {} );
            S.TieCheckBox(XXO("Show output"),
                          {wxT("/FileFormats/ExternalProgramShowOutput"),
                           false});
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

///
///
bool ExportCLOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportCLOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   wxString cmd = mCmd->GetValue();

   mHistory.Append(cmd);
   mHistory.Save(*gPrefs);

   gPrefs->Write(wxT("/FileFormats/ExternalProgramExportCommand"), cmd);
   gPrefs->Flush();

   return true;
}

///
///
void ExportCLOptions::OnBrowse(wxCommandEvent& WXUNUSED(event))
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
      this);
   if (path.empty()) {
      return;
   }

   if (path.Find(wxT(' ')) == wxNOT_FOUND) {
      mCmd->SetValue(path);
   }
   else {
      mCmd->SetValue(wxT('"') + path + wxT('"'));
   }

   mCmd->SetInsertionPointEnd();

   return;
}

//----------------------------------------------------------------------------
// ExportCLProcess
//----------------------------------------------------------------------------

static void Drain(wxInputStream *s, wxString *o)
{
   while (s->CanRead()) {
      char buffer[4096];

      s->Read(buffer, WXSIZEOF(buffer) - 1);
      buffer[s->LastRead()] = wxT('\0');
      *o += LAT1CTOWX(buffer);
   }
}

class ExportCLProcess final : public wxProcess
{
public:
   ExportCLProcess(wxString *output)
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

   bool IsActive()
   {
      return mActive;
   }

   void OnTerminate(int WXUNUSED( pid ), int status)
   {
      Drain(GetInputStream(), mOutput);
      Drain(GetErrorStream(), mOutput);

      mStatus = status;
      mActive = false;
   }

   int GetStatus()
   {
      return mStatus;
   }

private:
   wxString *mOutput;
   bool mActive;
   int mStatus;
};

//----------------------------------------------------------------------------
// ExportCL
//----------------------------------------------------------------------------

class ExportCL final : public ExportPlugin
{
public:

   ExportCL();

   // Required
   void OptionsCreate(ShuttleGui &S, int format) override;

   ProgressResult Export(AudacityProject *project,
                         std::unique_ptr<ProgressDialog> &pDialog,
                         unsigned channels,
                         const wxFileNameWrapper &fName,
                         bool selectedOnly,
                         double t0,
                         double t1,
                         MixerSpec *mixerSpec = NULL,
                         const Tags *metadata = NULL,
                         int subformat = 0) override;

   // Optional   
   bool CheckFileName(wxFileName &filename, int format = 0) override;

private:
   void GetSettings();

   std::vector<char> GetMetaChunk(const Tags *metadata);
   wxString mCmd;
   bool mShow;

   struct ExtendPath
   {
#if defined(__WXMSW__)
      wxString opath;

      ExtendPath()
      {
         // Give Windows a chance at finding lame command in the default location.
         wxString paths[] = {wxT("HKEY_LOCAL_MACHINE\\Software\\Lame for Audacity"),
                             wxT("HKEY_LOCAL_MACHINE\\Software\\FFmpeg for Audacity")};
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

         wxSetEnv(wxT("PATH"),npath);
      };

      ~ExtendPath()
      {
         if (!opath.empty())
         {
            wxSetEnv(wxT("PATH"),opath);
         }
      }
#endif
   };
};

ExportCL::ExportCL()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("CL"),0);
   AddExtension(wxT(""),0);
   SetMaxChannels(255,0);
   SetCanMetaData(false,0);
   SetDescription(XO("(external program)"),0);
}

ProgressResult ExportCL::Export(AudacityProject *project,
                                std::unique_ptr<ProgressDialog> &pDialog,
                                unsigned channels,
                                const wxFileNameWrapper &fName,
                                bool selectionOnly,
                                double t0,
                                double t1,
                                MixerSpec *mixerSpec,
                                const Tags *metadata,
                                int WXUNUSED(subformat))
{
   ExtendPath ep;
   wxString output;
   long rc;

   const auto path = fName.GetFullPath();

   GetSettings();

   // Bug 2178 - users who don't know what they are doing will 
   // now get a file extension of .wav appended to their ffmpeg filename
   // and therefore ffmpeg will be able to choose a file type.
   if( mCmd == wxT("ffmpeg -i - \"%f\"") && !fName.HasExt())
      mCmd.Replace( "%f", "%f.wav" );
   mCmd.Replace(wxT("%f"), path);

   // Kick off the command
   ExportCLProcess process(&output);

   rc = wxExecute(mCmd, wxEXEC_ASYNC, &process);
   if (!rc) {
      AudacityMessageBox( XO("Cannot export audio to %s").Format( path ) );
      process.Detach();
      process.CloseOutput();

      return ProgressResult::Cancelled;
   }

   // Turn off logging to prevent broken pipe messages
   wxLogNull nolog;

   // establish parameters
   int rate = lrint( ProjectSettings::Get( *project ).GetRate());
   const size_t maxBlockLen = 44100 * 5;
   unsigned long totalSamples = lrint((t1 - t0) * rate);
   unsigned long sampleBytes = totalSamples * channels * SAMPLE_SIZE(floatSample);

   wxOutputStream *os = process.GetOutputStream();

   // RIFF header
   struct {
      char riffID[4];            // "RIFF"
      wxUint32 riffLen;          // basically the file len - 8
      char riffType[4];          // "WAVE"
   } riff;

   // format chunk */
   struct {
      char fmtID[4];             // "fmt " */
      wxUint32 formatChunkLen;   // (format chunk len - first two fields) 16 in our case
      wxUint16 formatTag;        // 1 for PCM
      wxUint16 channels;
      wxUint32 sampleRate;
      wxUint32 avgBytesPerSec;   // sampleRate * blockAlign
      wxUint16 blockAlign;       // bitsPerSample * channels (assume bps % 8 = 0)
      wxUint16 bitsPerSample;
   } fmt;

   // id3 chunk header
   struct {
      char id3ID[4];             // "id3 "
      wxUint32 id3Len;           // length of metadata in bytes
   } id3;

   // data chunk header
   struct {
      char dataID[4];            // "data"
      wxUint32 dataLen;          // length of all samples in bytes
   } data;

   riff.riffID[0] = 'R';
   riff.riffID[1] = 'I';
   riff.riffID[2] = 'F';
   riff.riffID[3] = 'F';
   riff.riffLen   = wxUINT32_SWAP_ON_BE(sizeof(riff) +
                                        sizeof(fmt) +
                                        sizeof(data) +
                                        sampleBytes -
                                        8);
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
   if (metadata == NULL) {
      metadata = &Tags::Get(*project);
   }
   auto metachunk = GetMetaChunk(metadata);

   if (metachunk.size()) {

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
   if (metachunk.size()) {
      os->Write(&id3, sizeof(id3));
      os->Write(metachunk.data(), metachunk.size());
   }
   os->Write(&data, sizeof(data));

   // Mix 'em up
   const auto &tracks = TrackList::Get( *project );
   auto mixer = CreateMixer(
                            tracks,
                            selectionOnly,
                            t0,
                            t1,
                            channels,
                            maxBlockLen,
                            true,
                            rate,
                            floatSample,
                            mixerSpec);

   size_t numBytes = 0;
   samplePtr mixed = NULL;
   auto updateResult = ProgressResult::Success;

   {
      auto closeIt = finally ( [&] {
         // Should make the process die, before propagating any exception
         process.CloseOutput();
      } );

      // Prepare the progress display
      InitProgress( pDialog, XO("Export"),
         selectionOnly
            ? XO("Exporting the selected audio using command-line encoder")
            : XO("Exporting the audio using command-line encoder") );
      auto &progress = *pDialog;

      // Start piping the mixed data to the command
      while (updateResult == ProgressResult::Success && process.IsActive() && os->IsOk()) {
         // Capture any stdout and stderr from the command
         Drain(process.GetInputStream(), &output);
         Drain(process.GetErrorStream(), &output);

         // Need to mix another block
         if (numBytes == 0) {
            auto numSamples = mixer->Process(maxBlockLen);
            if (numSamples == 0) {
               break;
            }

            mixed = mixer->GetBuffer();
            numBytes = numSamples * channels;

            // Byte-swapping is necessary on big-endian machines, since
            // WAV files are little-endian
#if wxBYTE_ORDER == wxBIG_ENDIAN
            float *buffer = (float *) mixed;
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
               updateResult = ProgressResult::Cancelled;
               break;
            }
            bytes -= os->LastWrite();
            mixed += os->LastWrite();
         }

         // Update the progress display
         updateResult = progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
      // Done with the progress display
   }

   // Wait for process to terminate
   while (process.IsActive()) {
      wxMilliSleep(10);
      wxTheApp->Yield();
   }

   // Display output on error or if the user wants to see it
   if (process.GetStatus() != 0 || mShow) {
      // TODO use ShowInfoDialog() instead.
      wxDialogWrapper dlg(nullptr,
                   wxID_ANY,
                   XO("Command Output"),
                   wxDefaultPosition,
                   wxSize(600, 400),
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
      dlg.SetName();

      ShuttleGui S(&dlg, eIsCreating);
      S
         .Style( wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH )
         .AddTextWindow(mCmd + wxT("\n\n") + output);
      S.StartHorizontalLay(wxALIGN_CENTER, false);
      {
         S.Id(wxID_OK).AddButton(XXO("&OK"), wxALIGN_CENTER, true);
      }
      dlg.GetSizer()->AddSpacer(5);
      dlg.Layout();
      dlg.SetMinSize(dlg.GetSize());
      dlg.Center();

      dlg.ShowModal();

      if (process.GetStatus() != 0)
         updateResult = ProgressResult::Failed;
   }

   return updateResult;
}

std::vector<char> ExportCL::GetMetaChunk(const Tags *tags)
{
   std::vector<char> buffer;

#ifdef USE_LIBID3TAG
   struct id3_tag_deleter {
      void operator () (id3_tag *p) const { if (p) id3_tag_delete(p); }
   };

   std::unique_ptr<id3_tag, id3_tag_deleter> tp { id3_tag_new() };

   for (const auto &pair : tags->GetRange()) {
      const auto &n = pair.first;
      const auto &v = pair.second;
      const char *name = "TXXX";

      if (n.CmpNoCase(TAG_TITLE) == 0) {
         name = ID3_FRAME_TITLE;
      }
      else if (n.CmpNoCase(TAG_ARTIST) == 0) {
         name = ID3_FRAME_ARTIST;
      }
      else if (n.CmpNoCase(TAG_ALBUM) == 0) {
         name = ID3_FRAME_ALBUM;
      }
      else if (n.CmpNoCase(TAG_YEAR) == 0) {
         name = ID3_FRAME_YEAR;
      }
      else if (n.CmpNoCase(TAG_GENRE) == 0) {
         name = ID3_FRAME_GENRE;
      }
      else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
         name = ID3_FRAME_COMMENT;
      }
      else if (n.CmpNoCase(TAG_TRACK) == 0) {
         name = ID3_FRAME_TRACK;
      }
      else if (n.CmpNoCase(wxT("composer")) == 0) {
         name = "TCOM";
      }

      struct id3_frame *frame = id3_frame_new(name);

      if (!n.IsAscii() || !v.IsAscii()) {
         id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
      }
      else {
         id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
      }

      MallocString<id3_ucs4_t> ucs4{
         id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) v.mb_str(wxConvUTF8)) };

      if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
         // A hack to get around iTunes not recognizing the comment.  The
         // language defaults to XXX and, since it's not a valid language,
         // iTunes just ignores the tag.  So, either set it to a valid language
         // (which one???) or just clear it.  Unfortunately, there's no supported
         // way of clearing the field, so do it directly.
         id3_field *f = id3_frame_field(frame, 1);
         memset(f->immediate.value, 0, sizeof(f->immediate.value));
         id3_field_setfullstring(id3_frame_field(frame, 3), ucs4.get());
      }
      else if (strcmp(name, "TXXX") == 0) {
         id3_field_setstring(id3_frame_field(frame, 2), ucs4.get());

         ucs4.reset(id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) n.mb_str(wxConvUTF8)));

         id3_field_setstring(id3_frame_field(frame, 1), ucs4.get());
      }
      else {
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
      len++;   // Length must be even.
   }

   if (len > 0) {
      buffer.resize(len);
      id3_tag_render(tp.get(), (id3_byte_t *) buffer.data());
   }
#endif

   return buffer;
}

void ExportCL::OptionsCreate(ShuttleGui &S, int format)
{
   S.AddWindow( safenew ExportCLOptions{ S.GetParent(), format } );
}

bool ExportCL::CheckFileName(wxFileName &filename, int WXUNUSED(format))
{
   ExtendPath ep;

   if (filename.GetExt().empty()) {
      if (ShowWarningDialog(NULL,
                            wxT("MissingExtension"),
                            XO("You've specified a file name without an extension. Are you sure?"),
                            true) == wxID_CANCEL) {
         return false;
      }
   }

   GetSettings();

   wxArrayString argv = wxCmdLineParser::ConvertStringToArgs(mCmd,
#if defined(__WXMSW__)
      wxCMD_LINE_SPLIT_DOS
#else
      wxCMD_LINE_SPLIT_UNIX
#endif
   );

   if (argv.size() == 0) {
      ShowExportErrorDialog(
         ":745",
         XO("Program name appears to be missing."));
      return false;
   }
      
   // Normalize the path (makes absolute and resolves variables)   
   wxFileName cmd(argv[0]);
   cmd.Normalize(wxPATH_NORM_ALL & ~wxPATH_NORM_ABSOLUTE);

   // Just verify the given path exists if it is absolute.
   if (cmd.IsAbsolute()) {
      if (!cmd.Exists()) {
         AudacityMessageBox(
            XO("\"%s\" couldn't be found.").Format(cmd.GetFullPath()),
            XO("Warning"),
            wxOK | wxICON_EXCLAMATION);

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
      int action = AudacityMessageBox(
         XO("Unable to locate \"%s\" in your path.").Format(cmd.GetFullPath()),
         XO("Warning"),
         wxOK | wxICON_EXCLAMATION);

      return false;
   }

   return true;
}

void ExportCL::GetSettings()
{
   // Retrieve settings
   gPrefs->Read(wxT("/FileFormats/ExternalProgramShowOutput"), &mShow, false);
   mCmd = gPrefs->Read(wxT("/FileFormats/ExternalProgramExportCommand"), wxT("lame - \"%f.mp3\""));
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "CommandLine",
   []{ return std::make_unique< ExportCL >(); }
};
