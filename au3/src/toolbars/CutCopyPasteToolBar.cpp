/**********************************************************************

  Audacity: A Digital Audio Editor

  CutCopyPasteToolBar.cpp

  ksoze95

  See CutCopyPasteToolBar.h for details

*******************************************************************//*!

\class CutCopyPasteToolBar
\brief A ToolBar that has the cut, copy and paste buttons on it.

  This class, which is a child of Toolbar, creates the
  window containing interfaces to cut/copy/paste
  functions that are otherwise only available through
  menus. The window can be embedded within a normal project
  window, or within a ToolBarFrame.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/

#include "CutCopyPasteToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
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

#include "ToolManager.h"

enum {
    TBCutID,
    TBCopyID,
    TBPasteID,
    TBDeleteID,
    TBNumButtons
};

constexpr int first_TB_ID = 21300;

static const ToolBarButtons::ButtonList CutCopyPasteToolbarButtonList = {
    { TBCutID,    wxT("Cut"),    XO("Cut") },
    { TBCopyID,   wxT("Copy"),   XO("Copy") },
    { TBPasteID,  wxT("Paste"),  XO("Paste") },
    { TBDeleteID, wxT("Delete"), XO("Delete") }
};

IMPLEMENT_CLASS(CutCopyPasteToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for CutCopyPasteToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(CutCopyPasteToolBar, ToolBar)
EVT_COMMAND_RANGE(TBCutID + first_TB_ID,
                  TBCutID + first_TB_ID + TBNumButtons - 1,
                  wxEVT_COMMAND_BUTTON_CLICKED,
                  CutCopyPasteToolBar::OnButton)
END_EVENT_TABLE()

Identifier CutCopyPasteToolBar::ID()
{
    return wxT("CutCopyPaste");
}

//Standard constructor
CutCopyPasteToolBar::CutCopyPasteToolBar(AudacityProject& project)
    : ToolBar(project, XO("Cut/Copy/Paste"), ID())
    , mButtons{this, project, CutCopyPasteToolbarButtonList, TBNumButtons, first_TB_ID}
{
}

CutCopyPasteToolBar::~CutCopyPasteToolBar()
{
}

bool CutCopyPasteToolBar::ShownByDefault() const
{
    return false;
}

bool CutCopyPasteToolBar::HideAfterReset() const
{
    return true;
}

void CutCopyPasteToolBar::Create(wxWindow* parent)
{
    ToolBar::Create(parent);
    UpdatePrefs();
}

void CutCopyPasteToolBar::AddButton(
    teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
    int id, const TranslatableString& label, bool toggle)
{
    auto r = mButtons.CreateButton(eEnabledUp, eEnabledDown, eDisabled, id, label, toggle);
    mToolSizer->Add(r);
}

void CutCopyPasteToolBar::Populate()
{
    SetBackgroundColour(theTheme.Colour(clrMedium));
    MakeButtonBackgroundsSmall();

    Add(mToolSizer = safenew wxGridSizer(2, 2, toolbarSpacing, toolbarSpacing),
        0, wxALIGN_CENTRE | wxALL, toolbarSpacing);

    /* Buttons */
    // Tooltips match menu entries.
    // We previously had longer tooltips which were not more clear.
    AddButton(bmpCut, bmpCut, bmpCutDisabled, TBCutID,
              XO("Cut"));
    AddButton(bmpCopy, bmpCopy, bmpCopyDisabled, TBCopyID,
              XO("Copy"));
    AddButton(bmpPaste, bmpPaste, bmpPasteDisabled, TBPasteID,
              XO("Paste"));
    AddButton(bmpDelete, bmpDelete, bmpDeleteDisabled, TBDeleteID,
              XO("Delete"));

    mButtons.SetEnabled(TBPasteID, false);

    RegenerateTooltips();
}

void CutCopyPasteToolBar::UpdatePrefs()
{
    RegenerateTooltips();

    // Set label to pull in language change
    SetLabel(XO("Cut/Copy/Paste"));

    // Give base class a chance
    ToolBar::UpdatePrefs();
}

void CutCopyPasteToolBar::RegenerateTooltips()
{
    mButtons.RegenerateTooltips();
}

void CutCopyPasteToolBar::EnableDisableButtons()
{
    mButtons.EnableDisableButtons();
}

void CutCopyPasteToolBar::OnButton(wxCommandEvent& event)
{
    mButtons.OnButton(event);
}

static RegisteredToolbarFactory factory{
    []( AudacityProject& project ){
        return ToolBar::Holder{ safenew CutCopyPasteToolBar{ project } };
    }
};

namespace {
AttachedToolBarMenuItem sAttachment{
    /* i18n-hint: Clicking this menu item shows the toolbar for editing */
    CutCopyPasteToolBar::ID(),
    wxT("ShowCutCopyPasteTB"),
    XXO("&Cut/Copy/Paste Toolbar"),
    { Registry::OrderingHint::After, "ShowEditTB" }
};
}
