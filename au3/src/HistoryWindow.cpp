/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman
  Leland Lucius

*******************************************************************//*!

\class HistoryDialog
\brief Works with UndoManager to allow user to see descriptions of
and undo previous commands.  Also allows you to selectively clear the
undo memory so as to free up space.

*//*******************************************************************/

#include "HistoryWindow.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/imaglist.h>
#include <wx/listctrl.h>
#include <wx/settings.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "AudioIO.h"
#include "Clipboard.h"
#include "CommonCommandFlags.h"
#include "Diags.h"
#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"
#include "UndoManager.h"
#include "UndoTracks.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "HelpSystem.h"

#include <unordered_set>
#include "SampleBlock.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"

namespace {
using namespace WaveTrackUtilities;
struct SpaceUsageCalculator {
    using Type = unsigned long long;
    using SpaceArray = std::vector<Type>;

    Type CalculateUsage(const TrackList& tracks, SampleBlockIDSet& seen)
    {
        Type result = 0;
        //TIMER_START( "CalculateSpaceUsage", space_calc );
        InspectBlocks(
            tracks,
            BlockSpaceUsageAccumulator(result),
            &seen
            );
        return result;
    }

    SpaceArray space;
    Type clipboardSpaceUsage;

    void Calculate(UndoManager& manager)
    {
        SampleBlockIDSet seen;

        // After copies and pastes, a block file may be used in more than
        // one place in one undo history state, and it may be used in more than
        // one undo history state.  It might even be used in two states, but not
        // in another state that is between them -- as when you have state A,
        // then make a cut to get state B, but then paste it back into state C.

        // So be sure to count each block file once only, in the last undo item that
        // contains it.

        // Why the last and not the first? Because the user of the History dialog
        // may DELETE undo states, oldest first.  To reclaim disk space you must
        // DELETE all states containing the block file.  So the block file's
        // contribution to space usage should be counted only in that latest
        // state.

        manager.VisitStates(
            [this, &seen](const UndoStackElem& elem) {
            // Scan all tracks at current level
            if (auto pTracks = UndoTracks::Find(elem)) {
                space.push_back(CalculateUsage(*pTracks, seen));
            }
        },
            true // newest state first
            );

        // Count the usage of the clipboard separately, using another set.  Do not
        // multiple-count any block occurring multiple times within the clipboard.
        seen.clear();
        clipboardSpaceUsage = CalculateUsage(
            Clipboard::Get().GetTracks(), seen);

        //TIMER_STOP( space_calc );
    }
};
}

enum {
    ID_AVAIL = 1000,
    ID_FILESIZE,
    ID_TOTAL,
    ID_LEVELS,
    ID_DISCARD,
    ID_DISCARD_CLIPBOARD,
    ID_COMPACT
};

BEGIN_EVENT_TABLE(HistoryDialog, wxDialogWrapper)
EVT_SHOW(HistoryDialog::OnShow)
EVT_SIZE(HistoryDialog::OnSize)
EVT_CLOSE(HistoryDialog::OnCloseWindow)
EVT_LIST_ITEM_SELECTED(wxID_ANY, HistoryDialog::OnItemSelected)
EVT_BUTTON(ID_DISCARD, HistoryDialog::OnDiscard)
EVT_BUTTON(ID_DISCARD_CLIPBOARD, HistoryDialog::OnDiscardClipboard)
EVT_BUTTON(ID_COMPACT, HistoryDialog::OnCompact)
EVT_BUTTON(wxID_HELP, HistoryDialog::OnGetURL)
END_EVENT_TABLE()

#define HistoryTitle XO("History")

HistoryDialog::HistoryDialog(AudacityProject* parent, UndoManager* manager)
    : wxDialogWrapper(FindProjectFrame(parent), wxID_ANY, HistoryTitle,
                      wxDefaultPosition, wxDefaultSize,
                      wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
    SetName();

    mManager = manager;
    mProject = parent;
    mSelected = 0;
    mAudioIOBusy = false;

    //------------------------- Main section --------------------
    // Construct the GUI.
    ShuttleGui S(this, eIsCreating);
    Populate(S);

    mAudioIOSubscription = AudioIO::Get()->Subscribe(*this, &HistoryDialog::OnAudioIO);

    mClipboardSubscription = Clipboard::Get()
                             .Subscribe(*this, &HistoryDialog::UpdateDisplayForClipboard);

    if (parent) {
        mUndoSubscription = UndoManager::Get(*parent)
                            .Subscribe(*this, &HistoryDialog::UpdateDisplay);
    }
}

void HistoryDialog::Populate(ShuttleGui& S)
{
    auto imageList = std::make_unique<wxImageList>(9, 16);
    imageList->Add(wxIcon(empty9x16_xpm));
    imageList->Add(wxIcon(arrow_xpm));

    S.SetBorder(5);
    S.StartVerticalLay(true);
    {
        S.StartStatic(XO("&Manage History"), 1);
        {
            mList = S
                    .MinSize()
                    .ConnectRoot(wxEVT_KEY_DOWN, &HistoryDialog::OnListKeyDown)
                    .AddListControlReportMode(
                { { XO("Action"), wxLIST_FORMAT_LEFT, 260 },
                    { XO("Used Space"), wxLIST_FORMAT_LEFT, 125 } },
                wxLC_SINGLE_SEL
                );

            //Assign rather than set the image list, so that it is deleted later.
            // AssignImageList takes ownership
            mList->AssignImageList(imageList.release(), wxIMAGE_LIST_SMALL);

            S.StartMultiColumn(3, wxCENTRE);
            {
                S.AddPrompt(XXO("&Total space used"));
                mTotal = S.Id(ID_TOTAL).Style(wxTE_READONLY).AddTextBox({}, wxT(""), 10);
                S.AddVariableText({})->Hide();

#if defined(ALLOW_DISCARD)
                S.AddPrompt(XXO("&Undo levels available"));
                mAvail = S.Id(ID_AVAIL).Style(wxTE_READONLY).AddTextBox({}, wxT(""), 10);
                S.AddVariableText({})->Hide();

                S.AddPrompt(XXO("&Levels to discard"));
                mLevels = safenew wxSpinCtrl(S.GetParent(),
                                             ID_LEVELS,
                                             wxT("1"),
                                             wxDefaultPosition,
                                             wxDefaultSize,
                                             wxSP_ARROW_KEYS,
                                             0,
                                             mManager->GetCurrentState(),
                                             0);
                S.AddWindow(mLevels);
                /* i18n-hint: (verb)*/
                mDiscard = S.Id(ID_DISCARD).AddButton(XXO("&Discard"));
#endif
                S.AddPrompt(XXO("Clip&board space used"));
                mClipboard = S.Style(wxTE_READONLY).AddTextBox({}, wxT(""), 10);

#if defined(ALLOW_DISCARD)
                S.Id(ID_DISCARD_CLIPBOARD).AddButton(XXO("D&iscard"));
#endif
            }
            S.EndMultiColumn();
        }
        S.EndStatic();
#if defined(ALLOW_DISCARD)
        mCompact = safenew wxButton(this, ID_COMPACT, _("&Compact"));
        S.AddStandardButtons(eOkButton | eHelpButton, mCompact);
#else
        S.AddStandardButtons(eOkButton | eHelpButton);
#endif
    }
    S.EndVerticalLay();
    // ----------------------- End of main section --------------

    Layout();
    Fit();
    SetMinSize(GetSize());
    mList->SetColumnWidth(0, mList->GetClientSize().x - mList->GetColumnWidth(1));
    mList->SetTextColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
}

void HistoryDialog::OnAudioIO(AudioIOEvent evt)
{
    if (evt.type == AudioIOEvent::MONITOR || evt.type == AudioIOEvent::PAUSE) {
        return;
    }
    mAudioIOBusy = evt.on;

#if defined(ALLOW_DISCARD)
    mDiscard->Enable(!mAudioIOBusy);
    mCompact->Enable(!mAudioIOBusy);
#endif
}

void HistoryDialog::UpdateDisplayForClipboard(ClipboardChangeMessage)
{
    DoUpdateDisplay();
}

void HistoryDialog::UpdateDisplay(UndoRedoMessage message)
{
    switch (message.type) {
    case UndoRedoMessage::Pushed:
    case UndoRedoMessage::Modified:
    case UndoRedoMessage::UndoOrRedo:
    case UndoRedoMessage::Reset:
    case UndoRedoMessage::Purge:
        break;
    default:
        return;
    }
    DoUpdateDisplay();
}

void HistoryDialog::DoUpdateDisplay()
{
    if (IsShown()) {
        DoUpdate();
    }
}

bool HistoryDialog::Show(bool show)
{
    if (show && !IsShown()) {
        DoUpdate();
    }
    return wxDialogWrapper::Show(show);
}

void HistoryDialog::DoUpdate()
{
    int i = 0;

    SpaceUsageCalculator calculator;
    calculator.Calculate(*mManager);

    // point to size for oldest state
    auto iter = calculator.space.rbegin();

    mList->DeleteAllItems();

    wxLongLong_t total = 0;
    mSelected = mManager->GetCurrentState();
    mManager->VisitStates(
        [&]( const UndoStackElem& elem ){
        const auto space = *iter++;
        total += space;
        const auto size = Internat::FormatSize(space);
        const auto& desc = elem.description;
        mList->InsertItem(i, desc.Translation(), i == mSelected ? 1 : 0);
        mList->SetItem(i, 1, size.Translation());
        ++i;
    },
        false // oldest state first
        );

    mTotal->SetValue(Internat::FormatSize(total).Translation());

    auto clipboardUsage = calculator.clipboardSpaceUsage;
    mClipboard->SetValue(Internat::FormatSize(clipboardUsage).Translation());
#if defined(ALLOW_DISCARD)
    FindWindowById(ID_DISCARD_CLIPBOARD)->Enable(clipboardUsage > 0);
#endif

    mList->EnsureVisible(mSelected);

    mList->SetItemState(mSelected,
                        wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                        wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

    UpdateLevels();
}

void HistoryDialog::UpdateLevels()
{
#if defined(ALLOW_DISCARD)
    wxWindow* focus;
    int value = mLevels->GetValue();

    if (value > mSelected) {
        value = mSelected;
    }

    if (value == 0) {
        value = 1;
    }

    mLevels->SetValue(value);
    mLevels->SetRange(1, mSelected);

    mAvail->SetValue(wxString::Format(wxT("%d"), mSelected));

    focus = FindFocus();
    if ((focus == mDiscard || focus == mLevels) && mSelected == 0) {
        mList->SetFocus();
    }

    mLevels->Enable(mSelected > 0);
    mDiscard->Enable(!mAudioIOBusy && mSelected > 0);
#endif
}

void HistoryDialog::OnDiscard(wxCommandEvent& WXUNUSED(event))
{
    int i = mLevels->GetValue();

    mSelected -= i;
    mManager->RemoveStates(0, i);
    ProjectHistory::Get(*mProject).SetStateTo(mSelected);

    while (--i >= 0) {
        mList->DeleteItem(i);
    }

    DoUpdate();
}

void HistoryDialog::OnDiscardClipboard(wxCommandEvent& WXUNUSED(event))
{
    Clipboard::Get().Clear();
}

void HistoryDialog::OnCompact(wxCommandEvent& WXUNUSED(event))
{
    auto& projectFileIO = ProjectFileIO::Get(*mProject);

    projectFileIO.ReopenProject();

    auto baseFile = wxFileName(projectFileIO.GetFileName());
    auto walFile = wxFileName(projectFileIO.GetFileName() + wxT("-wal"));
    auto before = baseFile.GetSize() + walFile.GetSize();

    projectFileIO.Compact({}, true);

    auto after = baseFile.GetSize() + walFile.GetSize();

    AudacityMessageBox(
        XO("Compacting actually freed %s of disk space.")
        .Format(Internat::FormatSize((before - after).GetValue())),
        XO("History"));
}

void HistoryDialog::OnGetURL(wxCommandEvent& WXUNUSED(event))
{
    HelpSystem::ShowHelp(this, L"Undo,_Redo_and_History");
}

void HistoryDialog::OnItemSelected(wxListEvent& event)
{
    if (mAudioIOBusy) {
        mList->SetItemState(mSelected,
                            wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                            wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);
        return;
    }

    int selected = event.GetIndex();
    int i;

    for (i = 0; i < mList->GetItemCount(); i++) {
        mList->SetItemImage(i, 0);
        if (i > selected) {
            mList->SetItemTextColour(i, *wxLIGHT_GREY);
        } else {
            mList->SetItemTextColour(i, mList->GetTextColour());
        }
    }
    mList->SetItemImage(selected, 1);

    // Do not do a SetStateTo() if we're not actually changing the selected
    // entry.  Doing so can cause unnecessary delays upon initial load or while
    // clicking the same entry over and over.
    if (selected != mSelected) {
        ProjectHistory::Get(*mProject).SetStateTo(selected);
    }
    mSelected = selected;

    UpdateLevels();
}

void HistoryDialog::OnListKeyDown(wxKeyEvent& event)
{
    switch (event.GetKeyCode()) {
    case WXK_RETURN:
        // Don't know why wxListCtrls prevent default dialog action,
        // but they do, so handle it.
        EmulateButtonClickIfPresent(GetAffirmativeId());
        break;

    default:
        event.Skip();
        break;
    }
}

void HistoryDialog::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
    this->Show(false);
}

void HistoryDialog::OnShow(wxShowEvent& event)
{
    if (event.IsShown()) {
        mList->SetFocus();
    }
}

void HistoryDialog::OnSize(wxSizeEvent& WXUNUSED(event))
{
    Layout();
    mList->SetColumnWidth(0, mList->GetClientSize().x - mList->GetColumnWidth(1));
    if (mList->GetItemCount() > 0) {
        mList->EnsureVisible(mSelected);
    }
}

// PrefsListener implementation
void HistoryDialog::UpdatePrefs()
{
    bool shown = IsShown();
    if (shown) {
        Show(false);
    }

    SetSizer(nullptr);
    DestroyChildren();

    SetTitle(HistoryTitle);
    ShuttleGui S(this, eIsCreating);
    Populate(S);

    if (shown) {
        Show(true);
    }
}

// Remaining code hooks this add-on into the application
#include "CommandContext.h"
#include "MenuRegistry.h"

namespace {
// History window attached to each project is built on demand by:
AttachedWindows::RegisteredFactory sHistoryWindowKey{
    []( AudacityProject& parent ) -> wxWeakRef< wxWindow > {
        auto& undoManager = UndoManager::Get(parent);
        return safenew HistoryDialog(&parent, &undoManager);
    }
};

// Define our extra menu item that invokes that factory
void OnHistory(const CommandContext& context)
{
    auto& project = context.project;

    auto historyWindow = &GetAttachedWindows(project).Get(sHistoryWindowKey);
    historyWindow->Show();
    historyWindow->Raise();
}

// Register that menu item

using namespace MenuRegistry;
AttachedItem sAttachment{
    // History window should be available either for UndoAvailableFlag
    // or RedoAvailableFlag,
    // but we can't make the AddItem flags and mask have both,
    // because they'd both have to be true for the
    // command to be enabled.
    //    If user has Undone the entire stack, RedoAvailableFlag is on
    //    but UndoAvailableFlag is off.
    //    If user has done things but not Undone anything,
    //    RedoAvailableFlag is off but UndoAvailableFlag is on.
    // So in either of those cases,
    // (AudioIONotBusyFlag | UndoAvailableFlag | RedoAvailableFlag) mask
    // would fail.
    // The only way to fix this in the current architecture
    // is to hack in special cases for RedoAvailableFlag
    // in AudacityProject::UpdateMenus() (ugly)
    // and CommandManager::HandleCommandEntry() (*really* ugly --
    // shouldn't know about particular command names and flags).
    // Here's the hack that would be necessary in
    // AudacityProject::UpdateMenus(), if somebody decides to do it:
    //    // Because EnableUsingFlags requires all the flag bits match the
    //    // corresponding mask bits,
    //    // "UndoHistory" specifies only
    //    // AudioIONotBusyFlag | UndoAvailableFlag, because that
    //    // covers the majority of cases where it should be enabled.
    //    // If history is not empty but we've Undone the whole stack,
    //    // we also want to enable,
    //    // to show the Redo's on stack.
    //    // "UndoHistory" might already be enabled,
    //    // but add this check for RedoAvailableFlag.
    //    if (flags & RedoAvailableFlag)
    //       GetCommandManager()->Enable(wxT("UndoHistory"), true);
    // So for now, enable the command regardless of stack.
    // It will just show empty sometimes.
    // FOR REDESIGN,
    // clearly there are some limitations with the flags/mask bitmaps.

    /* i18n-hint: Clicking this menu item shows the various editing steps
       that have been taken.*/
    Command(wxT("UndoHistory"), XXO("&History"), OnHistory,
            AudioIONotBusyFlag()),
    wxT("View/Windows")
};
}
