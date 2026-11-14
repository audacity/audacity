#include <wx/app.h>
#include <wx/bmpbuttn.h>
#include <wx/textctrl.h>
#include <wx/frame.h>

#include "../AboutDialog.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "../CommonCommandFlags.h"
#include "../CrashReport.h" // for HAS_CRASH_REPORT
#include "FileNames.h"
#include "HelpText.h"
#include "../HelpUtilities.h"
#include "LogWindow.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectSnap.h"
#include "../ProjectWindows.h"
#include "SelectFile.h"
#include "ShuttleGui.h"
#include "SyncLock.h"
#include "Theme.h"
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "../prefs/PrefsDialog.h"
#include "AudacityMessageBox.h"
#include "HelpSystem.h"

#include "FrameStatisticsDialog.h"

#if defined(HAVE_UPDATES_CHECK)
#include "update/UpdateManager.h"
#endif

// private helper classes and functions
namespace {
/** @brief Class which makes a dialog for displaying quick fixes to common issues.
 *
 * This class originated with the 'Stuck in a mode' problem, where far too many
 * users get into a mode without realising, and don't know how to get out.
 * It is a band-aid, and we should do more towards a full and proper solution
 * where there are fewer special modes, and they don't persist.
 */
class QuickFixDialog : public wxDialogWrapper
{
public:
    using PrefSetter = std::function< void () >;

    QuickFixDialog(wxWindow* pParent, AudacityProject& project);
    void Populate();
    void PopulateOrExchange(ShuttleGui& S);
    void AddStuck(ShuttleGui& S, bool& bBool, const PrefSetter& prefSetter, const TranslatableString& Prompt, const ManualPageID& Help);

    void OnOk(wxCommandEvent& event);
    void OnCancel(wxCommandEvent& event);
    void OnHelp(const ManualPageID& Str);
    void OnFix(const PrefSetter& setter, wxWindowID id);

    AudacityProject& mProject;

    int mItem;
    bool mbSyncLocked;
    bool mbInSnapTo;
    bool mbSoundActivated;
    DECLARE_EVENT_TABLE()
};

#define FixButtonID           7001
#define HelpButtonID          7011

BEGIN_EVENT_TABLE(QuickFixDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK,                                            QuickFixDialog::OnOk)
EVT_BUTTON(wxID_CANCEL,                                        QuickFixDialog::OnCancel)
END_EVENT_TABLE();

QuickFixDialog::QuickFixDialog(wxWindow* pParent, AudacityProject& project)
    : wxDialogWrapper(pParent, wxID_ANY, XO("Do you have these problems?"),
                      wxDefaultPosition, wxDefaultSize,
                      wxDEFAULT_DIALOG_STYLE)
    , mProject{project}
{
    mbSyncLocked = SyncLockTracks.Read();
    mbInSnapTo = ProjectSnap(project).GetSnapMode() != SnapMode::SNAP_OFF;
    gPrefs->Read(wxT("/SnapTo"));
    mbSoundActivated = SoundActivatedRecord.Read();

    ShuttleGui S(this, eIsCreating);
    PopulateOrExchange(S);

    Fit();
    auto sz = GetSize();
    SetMinSize(sz);
    SetMaxSize(sz);

    // The close button has the cancel id and acts exactly the same as cancel.
    wxButton* pWin = (wxButton*)FindWindowById(wxID_CANCEL);
    if (pWin) {
        pWin->SetFocus();
    }
    Center();
}

void QuickFixDialog::AddStuck(ShuttleGui& S, bool& bBool,
                              const PrefSetter& prefSetter,
                              const TranslatableString& Prompt, const ManualPageID& Help)
{
    mItem++;
    wxWindowID id = FixButtonID + mItem;
    if (!bBool) {
        return;
    }

    S
    .AddFixedText(Prompt);

    S
    .Id(id)
    .AddButton(XXO("Fix"))
    ->Bind(wxEVT_BUTTON, [this, prefSetter, id](wxCommandEvent&){
        OnFix(prefSetter, id);
    });

    {
        // Replace standard Help button with smaller icon button.
        // bs->AddButton(safenew wxButton(parent, wxID_HELP));
        auto b = safenew wxBitmapButton(S.GetParent(), HelpButtonID + mItem, theTheme.Bitmap(bmpHelpIcon));
        b->SetToolTip(_("Help"));
        b->SetLabel(_("Help"));     // for screen readers
        b->Bind(wxEVT_BUTTON, [this, Help](const wxCommandEvent&){
            OnHelp(Help);
        });
        S.AddWindow(b);
    }
}

void QuickFixDialog::PopulateOrExchange(ShuttleGui& S)
{
    S.StartVerticalLay(1);
    S.StartStatic(XO("Quick Fixes"));

    // These aren't all possible modes one can be stuck in, but they are some of them.
    bool bStuckInMode = mbSyncLocked || mbInSnapTo || mbSoundActivated;

    if (!bStuckInMode) {
        SetLabel(XO("Nothing to do"));
        S.AddFixedText(XO("No quick, easily fixed problems were found"));
    } else {
        S.StartMultiColumn(3, wxALIGN_CENTER);
        {
            mItem = -1;

            auto defaultAction
                =[](AudacityProject* pProject, BoolSetting& setting){
                return
                    [pProject, &setting]{
                    setting.Reset();
                    gPrefs->Flush();
                    // This is overkill (aka slow), as all preferences are
                    // reloaded and all
                    // toolbars recreated.
                    // Overkill probably doesn't matter, as this command is
                    // infrequently used.
                    DoReloadPreferences(*pProject);
                };
            };

            // Use # in the URLs to ensure we go to the online version of help.
            // Local help may well not be installed.
            auto pProject = &mProject;
            AddStuck(S, mbSyncLocked,
                     defaultAction(pProject, SyncLockTracks),
                     XO("Clocks on the Tracks"), "Quick_Fix#sync_lock");
            AddStuck(S, mbInSnapTo,
                     [pProject] {
                // Sadly SnapTo has to be handled specially,
                // as it is not part of the standard
                // preference dialogs.
                ProjectSnap::Get(*pProject).SetSnapMode(SnapMode::SNAP_OFF);
            },
                     XO("Can't select precisely"), "Quick_Fix#snap_to");
            AddStuck(S, mbSoundActivated,
                     defaultAction(pProject, SoundActivatedRecord),
                     XO("Recording stops and starts"),
                     "Quick_Fix#sound_activated_recording");
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartHorizontalLay(wxALIGN_CENTER_HORIZONTAL, 0);
    S.AddStandardButtons(eCloseButton + (bStuckInMode ? 0 : eHelpButton));
    S.EndHorizontalLay();

    S.EndVerticalLay();

    wxButton* pBtn = (wxButton*)FindWindowById(wxID_HELP);
    if (pBtn) {
        pBtn->Bind(wxEVT_BUTTON, [this]( const wxCommandEvent& ){
            OnHelp("Quick_Fix#");
        });
    }
}

void QuickFixDialog::OnOk(wxCommandEvent& event)
{
    (void)event;// Compiler food
    EndModal(wxID_OK);
}

void QuickFixDialog::OnCancel(wxCommandEvent& event)
{
    (void)event;// Compiler food
    EndModal(wxID_CANCEL);
}

void QuickFixDialog::OnHelp(const ManualPageID& Str)
{
    HelpSystem::ShowHelp(this, Str, true);
}

void QuickFixDialog::OnFix(const PrefSetter& setter, wxWindowID id)
{
    if (setter) {
        setter();
    }

    // Change the label after doing the fix, as the fix may take a second or two.
    auto pBtn = FindWindow(id);
    if (pBtn) {
        pBtn->SetLabel(_("Fixed"));
    }

    // The close button has the cancel id and acts exactly the same as cancel.
    wxButton* pWin = (wxButton*)FindWindowById(wxID_CANCEL);
    if (pWin) {
        pWin->SetFocus();
    }
}
}

namespace {
// Menu handler functions

void OnQuickFix(const CommandContext& context)
{
    auto& project = context.project;
    QuickFixDialog dlg(&GetProjectFrame(project), project);
    dlg.ShowModal();
}

void OnQuickHelp(const CommandContext& context)
{
    auto& project = context.project;
    HelpSystem::ShowHelp(
        &GetProjectFrame(project),
        L"Quick_Help");
}

void OnManual(const CommandContext& context)
{
    auto& project = context.project;
    HelpSystem::ShowHelp(
        &GetProjectFrame(project),
        L"Main_Page");
}

void OnAudioDeviceInfo(const CommandContext& context)
{
    auto& project = context.project;
    auto gAudioIO = AudioIOBase::Get();
    wxString info = gAudioIO->GetDeviceInfo();
    ShowDiagnostics(project, info,
                    XO("Audio Device Info"), wxT("deviceinfo.txt"));
}

void OnShowLog(const CommandContext& context)
{
    LogWindow::Show();
}

#if defined(HAS_CRASH_REPORT)
void OnCrashReport(const CommandContext& WXUNUSED(context))
{
// Change to "1" to test a real crash
#if 0
    char* p = 0;
    *p = 1234;
#endif
    CrashReport::Generate(wxDebugReport::Context_Current);
}

#endif

#ifdef IS_ALPHA
void OnSegfault(const CommandContext&)
{
    unsigned* p = nullptr;
    *p = 0xDEADBEEF;
}

void OnException(const CommandContext&)
{
    // Throw an exception that can be caught only as (...)
    // The intent is to exercise detection of unhandled exceptions by the
    // crash reporter
    struct Unique {};
    throw Unique{};
}

void OnAssertion(const CommandContext&)
{
    // We don't use assert() much directly, but Breakpad does detect it
    // This may crash the program only in debug builds
    // See also wxSetAssertHandler, and wxApp::OnAssertFailure()
    assert(false);
}

#endif

void OnMenuTree(const CommandContext& context)
{
    auto& project = context.project;
    enum : unsigned {
        TAB = 3
    };

    unsigned level{};
    wxString indentation;
    wxString info;
    auto Indent = [&](){ info += indentation; };
    auto Return = [&](){ info += '\n'; };

    using namespace MenuRegistry;
    auto visitor = Visitor<Traits> {
        std::tuple{
            [&](const MenuItem& item, const auto&) {
                Indent();
                // using GET for alpha only diagnostic tool
                info += item.name.GET();
                Return();
                indentation = wxString{ ' ', TAB* ++level };
            },

            [&](const SingleItem& item, const auto&) {
                // using GET for alpha only diagnostic tool
                Indent();
                info += item.name.GET();
                Return();
            },

            [&](const MenuItem& item, const auto&) {
                indentation = wxString{ ' ', TAB* --level };
            }
        },
        [&]() {
            static const wxString separatorName{ '=', 20 };
            Indent();
            info += separatorName;
            Return();
        }
    };
    MenuRegistry::Visit(visitor, project);

    ShowDiagnostics(project, info,
                    Verbatim("Menu Tree"), wxT("menutree.txt"), true);
}

void OnFrameStatistics(const CommandContext&)
{
    FrameStatisticsDialog::Show(true);
}

#if defined(HAVE_UPDATES_CHECK)
void OnCheckForUpdates(const CommandContext& WXUNUSED(context))
{
    UpdateManager::GetInstance().GetUpdates(false, false);
}

#endif

void OnAbout(const CommandContext& context)
{
#ifdef __WXMAC__
    // Modeless dialog, consistent with other Mac applications
    // Simulate the application Exit menu item
    wxCommandEvent evt{ wxEVT_MENU, wxID_ABOUT };
    wxTheApp->AddPendingEvent(evt);
#else
    auto& project = context.project;
    auto& window = GetProjectFrame(project);

    // Windows and Linux still modal.
    AboutDialog dlog(&window);
    dlog.ShowModal();
#endif
}

#if 0
// Legacy handlers, not used as of version 2.3.0

// Only does the update checks if it's an ALPHA build and not disabled by
// preferences.
void MayCheckForUpdates(AudacityProject& project)
{
#ifdef IS_ALPHA
    OnCheckForUpdates(project);
#endif
}

#endif

// Menu definitions

using namespace MenuRegistry;
auto HelpMenu()
{
    static auto menu = std::shared_ptr{
        Menu(wxT("Help"), XXO("&Help"),
             Section("Basic",
                     Command(wxT("QuickHelp"), XXO("&Quick Help..."), OnQuickHelp,
                             AlwaysEnabledFlag),
                     Command(wxT("Manual"), XXO("&Manual..."), OnManual,
                             AlwaysEnabledFlag)
                     ),

   #ifdef __WXMAC__
             Items
   #else
             Section
   #endif
                 ("Other",
                 Menu(wxT("Diagnostics"), XXO("&Diagnostics"),
                      Command(wxT("DeviceInfo"), XXO("Au&dio Device Info..."),
                              OnAudioDeviceInfo,
                              AudioIONotBusyFlag()),
                      Command(wxT("Log"), XXO("Show &Log..."), OnShowLog,
                              AlwaysEnabledFlag),
      #if defined(HAS_CRASH_REPORT)
                      Command(wxT("CrashReport"), XXO("&Generate Support Data..."),
                              OnCrashReport, AlwaysEnabledFlag)
      #endif

      #ifdef IS_ALPHA
                      ,
                      // alpha-only items don't need to internationalize, so use
                      // Verbatim for labels

                      Command(wxT("RaiseSegfault"), Verbatim("Test segfault report"),
                              OnSegfault, AlwaysEnabledFlag),

                      Command(wxT("ThrowException"), Verbatim("Test exception report"),
                              OnException, AlwaysEnabledFlag),

                      Command(wxT("ViolateAssertion"), Verbatim("Test assertion report"),
                              OnAssertion, AlwaysEnabledFlag),

                      // Menu explorer.  Perhaps this should become a macro command
                      Command(wxT("MenuTree"), Verbatim("Menu Tree..."),
                              OnMenuTree,
                              AlwaysEnabledFlag),

                      Command(
                          wxT("FrameStatistics"), Verbatim("Frame Statistics..."),
                          OnFrameStatistics,
                          AlwaysEnabledFlag)
      #endif
                      )
                 ),

             Section("Extra",
   #if defined(HAVE_UPDATES_CHECK)
                     Command(wxT("Updates"), XXO("&Check for Updates..."),
                             OnCheckForUpdates,
                             AlwaysEnabledFlag),
   #endif
                     Command(wxT("About"), XXO("&About Audacity"), OnAbout,
                             AlwaysEnabledFlag)
                     )
             ) };
    return menu;
}

AttachedItem sAttachment1{ Indirect(HelpMenu()) };
}
