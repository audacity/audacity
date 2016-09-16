/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.cpp

  Joshua Haberman

  This code allows Audacity to export data by piping it to an external
  program.

**********************************************************************/

#include "../Audacity.h"
#include "ExportCL.h"
#include "../Project.h"

#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/process.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <FileDialog.h>
#include "Export.h"

#include "../Mix.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../Internat.h"
#include "../float_cast.h"
#include "../widgets/FileHistory.h"

#include "../Track.h"


//----------------------------------------------------------------------------
// ExportCLOptions
//----------------------------------------------------------------------------

class ExportCLOptions final : public wxPanelWrapper
{
public:
   ExportCLOptions(wxWindow *parent, int format);
   virtual ~ExportCLOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

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

   if (mHistory.GetCount() == 0) {
      mHistory.AddFileToHistory(wxT("ffmpeg -i - \"%f\""), false);
      mHistory.AddFileToHistory(wxT("lame - \"%f\""), false);
   }

   mHistory.AddFileToHistory(gPrefs->Read(wxT("/FileFormats/ExternalProgramExportCommand"),
                                          mHistory.GetHistoryFile(0)),
                             false);

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
   wxArrayString cmds;
   wxString cmd;

   for (size_t i = 0; i < mHistory.GetCount(); i++) {
      cmds.Add(mHistory.GetHistoryFile(i));
   }
   cmd = cmds[0];

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetSizerProportion(1);
         S.StartMultiColumn(3, wxEXPAND);
         {
            S.SetStretchyCol(1);
            mCmd = S.AddCombo(_("Command:"),
                              cmd,
                              &cmds);
            S.Id(ID_BROWSE).AddButton(_("Browse..."),
                                      wxALIGN_CENTER_VERTICAL);
            S.AddFixedText(wxT(""));
            S.TieCheckBox(_("Show output"),
                          wxT("/FileFormats/ExternalProgramShowOutput"),
                          false);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();

      S.AddTitle(_("Data will be piped to standard in. \"%f\" uses the file name in the export window."));
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

   mHistory.AddFileToHistory(cmd, false);
   mHistory.Save(*gPrefs, wxT("/FileFormats/ExternalProgramHistory"));

   gPrefs->Write(wxT("/FileFormats/ExternalProgramExportCommand"), cmd);
   gPrefs->Flush();

   return true;
}

///
///
void ExportCLOptions::OnBrowse(wxCommandEvent& WXUNUSED(event))
{
   wxString path;
   wxString ext;

#if defined(__WXMSW__)
   ext = wxT(".exe");
#endif

   path = FileSelector(_("Find path to command"),
                       wxEmptyString,
                       wxEmptyString,
                       ext,
                       wxT("*") + ext,
                       wxFD_OPEN | wxRESIZE_BORDER,
                       this);
   if (path.IsEmpty()) {
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

/* this structure combines the RIFF header, the format chunk, and the data
 * chunk header */
struct wav_header {
   /* RIFF header */
   char riffID[4];            /* "RIFF" */
   wxUint32 lenAfterRiff;     /* basically the file len - 8, or samples len + 36 */
   char riffType[4];          /* "WAVE" */

   /* format chunk */
   char fmtID[4];             /* "fmt " */
   wxUint32 formatChunkLen;   /* (format chunk len - first two fields) 16 in our case */
   wxUint16 formatTag;        /* 1 for PCM */
   wxUint16 channels;
   wxUint32 sampleRate;
   wxUint32 avgBytesPerSec;   /* sampleRate * blockAlign */
   wxUint16 blockAlign;       /* bitsPerSample * channels (assume bps % 8 = 0) */
   wxUint16 bitsPerSample;

   /* data chunk header */
   char dataID[4];            /* "data" */
   wxUint32 dataLen;          /* length of all samples in bytes */
};

class ExportCL final : public ExportPlugin
{
public:

   ExportCL();

   // Required
   wxWindow *OptionsCreate(wxWindow *parent, int format);

   int Export(AudacityProject *project,
               unsigned channels,
               const wxString &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;
};

ExportCL::ExportCL()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("CL"),0);
   AddExtension(wxT(""),0);
   SetMaxChannels(255,0);
   SetCanMetaData(false,0);
   SetDescription(_("(external program)"),0);
}

int ExportCL::Export(AudacityProject *project,
                      unsigned channels,
                      const wxString &fName,
                      bool selectionOnly,
                      double t0,
                      double t1,
                      MixerSpec *mixerSpec,
                      const Tags *WXUNUSED(metadata),
                      int WXUNUSED(subformat))
{
   wxString output;
   wxString cmd;
   bool show;
   long rc;

   // Retrieve settings
   gPrefs->Read(wxT("/FileFormats/ExternalProgramShowOutput"), &show, false);
   cmd = gPrefs->Read(wxT("/FileFormats/ExternalProgramExportCommand"), wxT("lame - \"%f.mp3\""));
   cmd.Replace(wxT("%f"), fName);

#if defined(__WXMSW__)
   // Give Windows a chance at finding lame command in the default location.
   wxString paths[] = {wxT("HKEY_LOCAL_MACHINE\\Software\\Lame for Audacity"),
                       wxT("HKEY_LOCAL_MACHINE\\Software\\FFmpeg for Audacity")};
   wxString opath;
   wxString npath;
   wxRegKey reg;

   wxGetEnv(wxT("PATH"), &opath);
   npath = opath;

   for (int i = 0; i < WXSIZEOF(paths); i++) {
      reg.SetName(paths[i]);

      if (reg.Exists()) {
         wxString ipath;
         reg.QueryValue(wxT("InstallPath"), ipath);
         if (!ipath.IsEmpty()) {
            npath += wxPATH_SEP + ipath;
         }
      }
   }

   wxSetEnv(wxT("PATH"),npath.c_str());
#endif

   // Kick off the command
   ExportCLProcess process(&output);
   rc = wxExecute(cmd, wxEXEC_ASYNC, &process);

#if defined(__WXMSW__)
   if (!opath.IsEmpty()) {
      wxSetEnv(wxT("PATH"),opath.c_str());
   }
#endif

   if (!rc) {
      wxMessageBox(wxString::Format(_("Cannot export audio to %s"),
                                    fName.c_str()));
      process.Detach();
      process.CloseOutput();

      return false;
   }

   // Turn off logging to prevent broken pipe messages
   wxLogNull nolog;

   // establish parameters
   int rate = lrint(project->GetRate());
   const size_t maxBlockLen = 44100 * 5;
   unsigned long totalSamples = lrint((t1 - t0) * rate);
   unsigned long sampleBytes = totalSamples * channels * SAMPLE_SIZE(int16Sample);

   // fill up the wav header
   wav_header header;
   header.riffID[0]        = 'R';
   header.riffID[1]        = 'I';
   header.riffID[2]        = 'F';
   header.riffID[3]        = 'F';
   header.lenAfterRiff     = wxUINT32_SWAP_ON_BE(sampleBytes + 36);
   header.riffType[0]      = 'W';
   header.riffType[1]      = 'A';
   header.riffType[2]      = 'V';
   header.riffType[3]      = 'E';

   header.fmtID[0]         = 'f';
   header.fmtID[1]         = 'm';
   header.fmtID[2]         = 't';
   header.fmtID[3]         = ' ';
   header.formatChunkLen   = wxUINT32_SWAP_ON_BE(16);
   header.formatTag        = wxUINT16_SWAP_ON_BE(1);
   header.channels         = wxUINT16_SWAP_ON_BE(channels);
   header.sampleRate       = wxUINT32_SWAP_ON_BE(rate);
   header.bitsPerSample    = wxUINT16_SWAP_ON_BE(SAMPLE_SIZE(int16Sample) * 8);
   header.blockAlign       = wxUINT16_SWAP_ON_BE(header.bitsPerSample * header.channels / 8);
   header.avgBytesPerSec   = wxUINT32_SWAP_ON_BE(header.sampleRate * header.blockAlign);
   header.dataID[0]        = 'd';
   header.dataID[1]        = 'a';
   header.dataID[2]        = 't';
   header.dataID[3]        = 'a';
   header.dataLen          = wxUINT32_SWAP_ON_BE(sampleBytes);

   // write the header
   wxOutputStream *os = process.GetOutputStream();
   os->Write(&header, sizeof(wav_header));

   // Mix 'em up
   const TrackList *tracks = project->GetTracks();
   const WaveTrackConstArray waveTracks =
      tracks->GetWaveTrackConstArray(selectionOnly, false);
   auto mixer = CreateMixer(
                            waveTracks,
                            tracks->GetTimeTrack(),
                            t0,
                            t1,
                            channels,
                            maxBlockLen,
                            true,
                            rate,
                            int16Sample,
                            true,
                            mixerSpec);

   size_t numBytes = 0;
   samplePtr mixed = NULL;
   int updateResult = eProgressSuccess;

   {
      // Prepare the progress display
      ProgressDialog progress(_("Export"),
         selectionOnly ?
         _("Exporting the selected audio using command-line encoder") :
         _("Exporting the entire project using command-line encoder"));

      // Start piping the mixed data to the command
      while (updateResult == eProgressSuccess && process.IsActive() && os->IsOk()) {
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

            // Byte-swapping is neccesary on big-endian machines, since
            // WAV files are little-endian
#if wxBYTE_ORDER == wxBIG_ENDIAN
            wxUint16 *buffer = (wxUint16 *) mixed;
            for (int i = 0; i < numBytes; i++) {
               buffer[i] = wxUINT16_SWAP_ON_BE(buffer[i]);
            }
#endif
            numBytes *= SAMPLE_SIZE(int16Sample);
         }

         // Don't write too much at once...pipes may not be able to handle it
         size_t bytes = wxMin(numBytes, 4096);
         numBytes -= bytes;

         while (bytes > 0) {
            os->Write(mixed, bytes);
            if (!os->IsOk()) {
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

   // Should make the process die
   process.CloseOutput();

   // Wait for process to terminate
   while (process.IsActive()) {
      wxMilliSleep(10);
      wxTheApp->Yield();
   }

   // Display output on error or if the user wants to see it
   if (process.GetStatus() != 0 || show) {
      // TODO use ShowInfoDialog() instead.
      wxDialogWrapper dlg(nullptr,
                   wxID_ANY,
                   wxString(_("Command Output")),
                   wxDefaultPosition,
                   wxSize(600, 400),
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
      dlg.SetName(dlg.GetTitle());

      ShuttleGui S(&dlg, eIsCreating);
      S.AddTextWindow(cmd + wxT("\n\n") + output);
      S.StartHorizontalLay(wxALIGN_CENTER, false);
      {
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
      }
      dlg.GetSizer()->AddSpacer(5);
      dlg.Layout();
      dlg.SetMinSize(dlg.GetSize());
      dlg.Center();

      dlg.ShowModal();
   }

   return updateResult;
}

wxWindow *ExportCL::OptionsCreate(wxWindow *parent, int format)
{
   wxASSERT(parent); // to justify safenew
   return safenew ExportCLOptions(parent, format);
}

movable_ptr<ExportPlugin> New_ExportCL()
{
   return make_movable<ExportCL>();
}

