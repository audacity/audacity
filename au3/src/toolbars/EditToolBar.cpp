/**********************************************************************

  Audacity: A Digital Audio Editor

  EditToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

  See EditToolBar.h for details

*******************************************************************//*!

\class EditToolBar
\brief A ToolBar that has the edit buttons on it.

  This class, which is a child of Toolbar, creates the
  window containing interfaces to commonly-used edit
  functions that are otherwise only available through
  menus. The window can be embedded within a normal project
  window, or within a ToolBarFrame.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/

#include "EditToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/sizer.h>
#include <wx/tooltip.h>
#endif

#include "AllThemeResources.h"
#include "ImageManipulation.h"
#include "Prefs.h"
#include "Project.h"
#include "UndoManager.h"
#include "../widgets/AButton.h"

#include "CommandContext.h"
#include "CommandManager.h"
#include "../commands/CommandDispatch.h"

enum {
    ETBZoomInID,
    ETBZoomOutID,
    ETBZoomToggleID,

    ETBZoomSelID,
    ETBZoomFitID,

    ETBTrimID,
    ETBSilenceID,

    // no sync-lock on/off button.
    // #define OPTION_SYNC_LOCK_BUTTON

#ifdef OPTION_SYNC_LOCK_BUTTON
    ETBSyncLockID,
#endif

    ETBUndoID,
    ETBRedoID,

    ETBNumButtons
};

#ifdef OPTION_SYNC_LOCK_BUTTON
#include "SyncLock.h"
#endif

constexpr int first_ETB_ID = 11300;

static const ToolBarButtons::ButtonList EditToolbarButtonList = {
    { ETBZoomInID,   wxT("ZoomIn"),      XO("Zoom In") },
    { ETBZoomOutID,  wxT("ZoomOut"),     XO("Zoom Out") },
    { ETBZoomToggleID,   wxT("ZoomToggle"),      XO("Zoom Toggle") },
    { ETBZoomSelID,  wxT("ZoomSel"),     XO("Fit selection to width") },
    { ETBZoomFitID,  wxT("FitInWindow"), XO("Fit project to width") },

    { ETBTrimID,     wxT("Trim"),        XO("Trim audio outside selection") },
    { ETBSilenceID,  wxT("Silence"),     XO("Silence audio selection") },
#ifdef OPTION_SYNC_LOCK_BUTTON
    {
        ETBSyncLockID, wxT("SyncLock"),    XO("Sync-Lock Tracks")
    },
#endif
    {
        ETBUndoID,     wxT("Undo"),        XO("Undo")
    },
    { ETBRedoID,     wxT("Redo"),        XO("Redo") },
};

IMPLEMENT_CLASS(EditToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for EditToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(EditToolBar, ToolBar)
EVT_COMMAND_RANGE(ETBZoomInID + first_ETB_ID,
                  ETBZoomInID + first_ETB_ID + ETBNumButtons - 1,
                  wxEVT_COMMAND_BUTTON_CLICKED,
                  EditToolBar::OnButton)
END_EVENT_TABLE()

Identifier EditToolBar::ID()
{
    return wxT("Edit");
}

//Standard constructor
EditToolBar::EditToolBar(AudacityProject& project)
    : ToolBar(project, XO("Edit"), ID())
    , mButtons{this, project, EditToolbarButtonList, ETBNumButtons, first_ETB_ID}
{
#ifdef OPTION_SYNC_LOCK_BUTTON
    auto action = [this]() {
        bool bSyncLockTracks = SyncLockTracks.Read();

        if (bSyncLockTracks) {
            mButtons.PushDown(ETBSyncLockID);
        } else {
            mButtons.PopUp(ETBSyncLockID);
        }
    };

    mButtons.SetCustomEnableDisableButtonsAction(action);
#endif
}

EditToolBar::~EditToolBar()
{
}

void EditToolBar::Create(wxWindow* parent)
{
    ToolBar::Create(parent);
    UpdatePrefs();
}

void EditToolBar::AddSeparator()
{
    mToolSizer->AddSpacer(0);
}

void EditToolBar::AddButton(
    teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
    int id, const TranslatableString& label, bool toggle)
{
    auto r = mButtons.CreateButton(eEnabledUp, eEnabledDown, eDisabled, id, label, toggle);
    mToolSizer->Add(r);
}

void EditToolBar::Populate()
{
    SetBackgroundColour(theTheme.Colour(clrMedium));
    MakeButtonBackgroundsSmall();

    Add(mToolSizer = safenew wxGridSizer(2, 5, toolbarSpacing, toolbarSpacing),
        0, wxALIGN_CENTRE | wxALL, toolbarSpacing);

    /* Buttons */
    // Tooltips match menu entries.
    // We previously had longer tooltips which were not more clear.
    AddButton(bmpZoomIn, bmpZoomIn, bmpZoomInDisabled, ETBZoomInID,
              XO("Zoom In"));
    AddButton(bmpZoomOut, bmpZoomOut, bmpZoomOutDisabled, ETBZoomOutID,
              XO("Zoom Out"));
    AddButton(bmpZoomSel, bmpZoomSel, bmpZoomSelDisabled, ETBZoomSelID,
              XO("Zoom to Selection"));
    AddButton(bmpZoomFit, bmpZoomFit, bmpZoomFitDisabled, ETBZoomFitID,
              XO("Fit to Width"));
    AddButton(bmpZoomToggle, bmpZoomToggle, bmpZoomToggleDisabled, ETBZoomToggleID,
              XO("Zoom Toggle"));

    // Tooltips slightly more verbose than the menu entries are.
    AddButton(bmpTrim, bmpTrim, bmpTrimDisabled, ETBTrimID,
              XO("Trim audio outside selection"));
    AddButton(bmpSilence, bmpSilence, bmpSilenceDisabled, ETBSilenceID,
              XO("Silence audio selection"));

#ifdef OPTION_SYNC_LOCK_BUTTON
    AddButton(bmpSyncLockTracksUp, bmpSyncLockTracksDown, bmpSyncLockTracksUp, ETBSyncLockID,
              XO("Sync-Lock Tracks"), true);
#else
    AddSeparator();
#endif

    AddButton(bmpUndo, bmpUndo, bmpUndoDisabled, ETBUndoID,
              XO("Undo"));
    AddButton(bmpRedo, bmpRedo, bmpRedoDisabled, ETBRedoID,
              XO("Redo"));

    mButtons.SetEnabled(ETBZoomInID, false);
    mButtons.SetEnabled(ETBZoomOutID, false);
    mButtons.SetEnabled(ETBZoomToggleID, false);

    mButtons.SetEnabled(ETBZoomSelID, false);
    mButtons.SetEnabled(ETBZoomFitID, false);

#ifdef OPTION_SYNC_LOCK_BUTTON
    mButtons.PushDown(ETBSyncLockID);
#endif

    RegenerateTooltips();
}

void EditToolBar::UpdatePrefs()
{
    RegenerateTooltips();

    // Set label to pull in language change
    SetLabel(XO("Edit"));

    // Give base class a chance
    ToolBar::UpdatePrefs();
}

void EditToolBar::RegenerateTooltips()
{
    mButtons.RegenerateTooltips();
}

void EditToolBar::EnableDisableButtons()
{
    mButtons.EnableDisableButtons();
}

void EditToolBar::OnButton(wxCommandEvent& event)
{
    mButtons.OnButton(event);
}

static RegisteredToolbarFactory factory{
    []( AudacityProject& project ){
        return ToolBar::Holder{ safenew EditToolBar{ project } };
    }
};

#include "ToolManager.h"

namespace {
AttachedToolBarMenuItem sAttachment{
    /* i18n-hint: Clicking this menu item shows the toolbar for editing */
    EditToolBar::ID(), wxT("ShowEditTB"), XXO("&Edit Toolbar")
};
}
