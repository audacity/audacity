/**********************************************************************

  Audacity: A Digital Audio Editor

  NyquistDialog.cpp

  Roger B. Dannenberg

*******************************************************************//*!

\class NyquistDialog
\brief Get nyquist command and execute

*//*******************************************************************/

#include "Audacity.h"
#include "NyquistDialog.h"
#include "../../../lib-src/libnyquist/nyquist/xlisp/xlisp.h"
#include "nyx.h"

#include <wx/setup.h> // for wxUSE_* macros

#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/imaglist.h>
#include <wx/settings.h>

#include "Clipboard.h"
#include "Shuttle.h"
#include "ShuttleGui.h"
#include "Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectFileManager.h"
#include "ProjectHistory.h"
#include "ProjectManager.h"
#include "ProjectWindow.h"
#include "SelectUtilities.h"
#include "commands/CommandManager.h"
#include "PluginManager.h"
#include "effects/Effect.h"
#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"
#include "UndoManager.h"

#include "AllThemeResources.h"

#include "widgets/FileDialog/FileDialog.h"
#include "FileNames.h"
#include "import/Import.h"
#include "widgets/ErrorDialog.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/HelpSystem.h"
#include "NyquistAPI.h"


#if wxUSE_ACCESSIBILITY
#include "widgets/WindowAccessible.h"
#endif


#define NyquistTitle XO("Nyquist Command")
//#define ManageMacrosTitle XO("Manage Macros")

static const wxChar* KEY_File = wxT("NyquistFile");


// Separate numerical range from the additional buttons
// in the expanded view (which start at 10,000).
#define MacrosListID       7001
#define CommandsListID     7002
#define ApplyToProjectID   7003
#define NyquistCmdID       7004
#define NyquistBrowseID    7005
#define NyquistRunID       7006

BEGIN_EVENT_TABLE(NyquistDialog, wxDialogWrapper)
   EVT_BUTTON(NyquistBrowseID, NyquistDialog::OnBrowse)
   EVT_BUTTON(wxID_CANCEL, NyquistDialog::OnCancel)
   EVT_BUTTON(NyquistRunID, NyquistDialog::OnRunCommand)
END_EVENT_TABLE()

NyquistDialog::NyquistDialog(
   wxWindow * parent, AudacityProject &project, bool bInherited):
   wxDialogWrapper(parent, wxID_ANY, NyquistTitle,
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
   , mProject{ project }
{
    mStop = false;
    mBreak = false;
    mCont = false;

   if( bInherited )
      return;
   SetLabel(NyquistTitle);          // Provide visual label
   SetName(NyquistTitle);           // Provide audible label
   Populate();
}

NyquistDialog::~NyquistDialog()
{
}

void NyquistDialog::Populate()
{
   //------------------------- Main section --------------------
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
 
   Layout();
   Fit();
   wxSize sz = GetSize();
   SetSizeHints( sz );

   // Size and place window
   SetSize(std::min(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * 3 / 4, sz.GetWidth()),
           std::min(wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * 4 / 5, 400));

   Center();

}

/// Defines the dialog and does data exchange with it.
void NyquistDialog::PopulateOrExchange(ShuttleGui &S)
{
    PluginID id(wxT("Nyquist_Command"));
    RegistryPath path(wxT("CurrentSettings"));
    wxString filePath;
    PluginManager::Get().GetPrivateConfig(id, path, KEY_File, filePath);
    S.SetBorder(2);

    S.StartStatic(XO("Nyquist Command File"));
    {
        S.AddSpace(1);

        S.StartMultiColumn(3, wxEXPAND);
        {
            S.SetStretchyCol(1);
            // I tried to use a validator here to transfer command file name
            // to the text box, but I could not find any examples of file
            // names with validators and there is no documentation on
            // shuttleGui to speak of, so we'll try to recover saved parameter
            // values manually.
            mNyquistFile = S.AddTextBox(XXO("Location"), wxT(""), 30);
            mNyquistFile->WriteText(filePath);
            S.Id(NyquistBrowseID).AddButton(XXO("&Browse..."));
        }
        S.EndMultiColumn();
        S.SetBorder(10);
        S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, false);
        {
            S.StartHorizontalLay(wxALIGN_LEFT, false);
            {
                S.Id(NyquistRunID).AddButton(XXO("Run"), wxALIGN_CENTRE, true);
                /* i18n-hint verb */
                S.Id(wxID_CANCEL).AddButton(XXO("Close"));
            }
            S.EndHorizontalLay();
        }
        S.EndHorizontalLay();

        mNyquistOut = S.Prop(1)
            .Position(wxEXPAND | wxALL)
            .MinSize({ 480, 250 })
            .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
            .AddTextWindow("");

    }
    S.EndStatic();
}


void NyquistDialog::ApplyNyquist()
{
    ShowModal();
}

/*
void NyquistDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   wxString page = GetHelpPageName();
   HelpSystem::ShowHelp(this, page, true);
}
*/

void NyquistDialog::StaticOSCallback(void* This)
{
    ((NyquistDialog*)This)->OSCallback();
}

void NyquistDialog::OSCallback()
{
    if (mStop) {
        mStop = false;
        nyx_stop();
    } else if (mBreak) {
        mBreak = false;
        nyx_break();
    } else if (mCont) {
        mCont = false;
        nyx_continue();
    }
}

void NyquistDialog::StaticOutputCallback(int c, void* This)
{
    ((NyquistDialog*)This)->OutputCallback(c);
}

void NyquistDialog::OutputCallback(int c)
{
    // Always collect Nyquist error messages for normal plug-ins
    mNyquistOut->WriteText(wxString((char) c));
    std::cout << (char)c;
}

// DEBUG
const Track* getTrack0(AudacityProject &project)
{
    const TrackList& trackList = TrackList::Get(project);
    auto range = trackList.Leaders();

    if (trackList.size() == 0) {
        return NULL;
    }
    TrackList::const_iterator iter = range.begin();
    return *iter;
}


void NyquistDialog::OnRunCommand(wxCommandEvent & WXUNUSED(event))
{
    PluginID id(wxT("Nyquist_Command"));
    RegistryPath path(wxT("CurrentSettings"));
    wxString fileName = mNyquistFile->GetValue();
    PluginManager::Get().SetPrivateConfig(id, path, KEY_File, fileName);

#ifdef WRITE_ONE_CHANNEL
    // simple test: let's generate a 1kHz sine tone and write it at time 5 to 6 in mono audio Track 0
    const Track* track = getTrack0(mProject);
    if (track) {
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack);
            int trkchans = channels.size();  // channels->size();
            double trkrate = waveTrack->GetRate();
            assert(trkchans == 1);
            assert(trkrate == 44100);

            WaveTrack* curTrack = nullptr;
            WaveTrack::Holder outputTrack;
            double outputTime = 0;

            int i = 0;
            for (auto track : channels) {
                // this must be wrong. Also, I'm not sure how to write to a track.
                // I generally copied code from Nyquist.cpp (nyquist effects), but
                // I don't understand why it makes copies or how deep the copies 
                // go (are these virtual copies of whole tracks, or just empty
                // containers for new audio?)
                curTrack = (WaveTrack*)(const WaveTrack*)track;
                outputTrack = track->EmptyCopy();
                outputTrack->SetRate(trkrate);
                i++;
            }
            float tone[44100];
            for (int i = 0; i < trkrate; i++) {
                tone[i] = sin(3.14159 * 2 * 1000 * i / trkrate) * 0.1;
            }
            // write the audio
            outputTrack->Append((samplePtr)tone, floatSample, 44100);
            outputTrack->Flush();
            double end_time = outputTrack->GetEndTime();

            curTrack->ClearAndPaste(5, 6, outputTrack.get(),
                                    true, true); // mRestoreSplits, bMergeClips
        });

/*            for (size_t i = 0; i < trkchans; i++) {
                WaveTrack* out;

                if (chans == trkchans) {
                    out = outputTrack[i].get();
                } else {
                    out = outputTrack[0].get();
                }

                // See Nyquist.cpp for some code to compute mRestoreSplits and 
                // bMergeClips options.  For now, we ignore options. Let's see 
                // if the behavior is reasonable, and if not we'll try something
                // else.
                mCurTrack[i]->ClearAndPaste(start, start + maxdur, out,
                    true, true); // mRestoreSplits, bMergeClips
// See Nyquist.cpp for code dealing with SyncLockGroups here
            }
            */
            // Well, we wrote to the project, so probably something should be 
            // notified. Nyquist.cpp sets mProjectChanged = true; and we're in
            // an effect, but how can we find it? Should this go through globals
            // in nyx.c? Which already uses some globals to negotiate between
            // Audacity and Nyquist?
            // mProjectChanged = true;
    }
#endif
    // test to see if we can just execute some Nyquist code and display text output
    nyx_init();
    nyx_set_os_callback(StaticOSCallback, (void*)this);
    nyx_capture_output(StaticOutputCallback, (void*)this);

    auto cleanup = finally([&] {
        nyx_capture_output(NULL, (void*)NULL);
        nyx_set_os_callback(NULL, (void*)NULL);
        nyx_cleanup();
        });

    // reference needed when Nyquist calls into Audacity:
    setNyquistProject(&mProject);
    // ProjectHistory::Get(project).PushState(longDesc, shortDesc);
    writeText(XO("loading "));
    writeText(fileName);
    writeText(" ...\n");
    int status = nyx_load(fileName);
    writeText(status ? XO("... load successful.") : XO("... load failed."));
    //auto &window = ProjectWindow::Get( project );
    //   window.RedrawProject();
    return;
}

void NyquistDialog::OnBrowse(wxCommandEvent& WXUNUSED(event))
{
    FileNames::FileTypes fileTypes = FileNames::FileTypes({ { XO("XLisp file"), { "*.lsp"}},
                                                            { XO("SAL file"), { "*.sal" }} });
    FileDialogWrapper filePicker(
        this,
        XO("Choose a Nyquist program to run"), FileNames::DataDir(), wxT(""),
        fileTypes);
    if (filePicker.ShowModal() == wxID_CANCEL) {
        return;
    } else {
        mNyquistFile->WriteText(filePicker.GetPath());
    }
}


void NyquistDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   Hide();
}

/////////////////////////////////////////////////////////////////////
#include <wx/textdlg.h>
#include "BatchCommandDialog.h"

enum {
   AddButtonID = 10000,
   RemoveButtonID,
   RenameButtonID,
   RestoreButtonID,
   ImportButtonID,
   ExportButtonID,
   SaveButtonID,

   DefaultsButtonID,

   InsertButtonID,
   EditButtonID,
   DeleteButtonID,
   UpButtonID,
   DownButtonID,

// MacrosListID             7005
// CommandsListID,       7002
// Re-Use IDs from NyquistDialog.
   ApplyToProjectButtonID = ApplyToProjectID,
};
