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
#include "../Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "UndoManager.h"
#include "../widgets/AButton.h"

#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/CommandDispatch.h"

constexpr int first_TB_ID = 21300;


IMPLEMENT_CLASS(CutCopyPasteToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for CutCopyPasteToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( CutCopyPasteToolBar, ToolBar )
   EVT_COMMAND_RANGE(TBCutID+first_TB_ID,
                      TBCutID+first_TB_ID + TBNumButtons - 1,
                      wxEVT_COMMAND_BUTTON_CLICKED,
                      CutCopyPasteToolBar::OnButton )
END_EVENT_TABLE()

//Standard constructor
CutCopyPasteToolBar::CutCopyPasteToolBar( AudacityProject &project )
: ToolBar(project, CutCopyPasteBarID, XO("Cut/Copy/Paste"), wxT("Cut/Copy/Paste"))
{
}

CutCopyPasteToolBar::~CutCopyPasteToolBar()
{
}

void CutCopyPasteToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

void CutCopyPasteToolBar::AddButton(
   teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
   int firstToolBarId,
   int thisButtonId,
   const TranslatableString &label,
   bool toggle)
{
   AButton *&r = mButtons[thisButtonId];

   r = ToolBarButtons::AddButton(this,
      eEnabledUp, eEnabledDown, eDisabled,
      firstToolBarId, thisButtonId,
      label, toggle);

   mToolSizer->Add(r);
}

void CutCopyPasteToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   MakeButtonBackgroundsSmall();

   Add(mToolSizer = safenew wxGridSizer(2, 2, 1, 1));

   /* Buttons */
   // Tooltips match menu entries.
   // We previously had longer tooltips which were not more clear.
   AddButton(bmpCut, bmpCut, bmpCutDisabled, first_TB_ID, TBCutID,
      XO("Cut"));
   AddButton(bmpCopy, bmpCopy, bmpCopyDisabled, first_TB_ID, TBCopyID,
      XO("Copy"));
   AddButton(bmpPaste, bmpPaste, bmpPasteDisabled, first_TB_ID, TBPasteID,
      XO("Paste"));

   mButtons[TBPasteID]->SetEnabled(false);

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
   ForAllButtons( TBActTooltips );
}

void CutCopyPasteToolBar::EnableDisableButtons()
{
   ForAllButtons( TBActEnableDisable );
}

static const struct Entry {
   int tool;
   CommandID commandName;
   TranslatableString untranslatedLabel;
} CutCopyPasteToolbarButtonList[] = {
   { TBCutID,   wxT("Cut"),   XO("Cut")   },
   { TBCopyID,  wxT("Copy"),  XO("Copy")  },
   { TBPasteID, wxT("Paste"), XO("Paste") }
};

void CutCopyPasteToolBar::ForAllButtons(int Action)
{
   AudacityProject *p;
   CommandManager* cm = nullptr;

   if( Action & TBActEnableDisable ){
      p = &mProject;
      cm = &CommandManager::Get( *p );
   }

   for (const auto &entry : CutCopyPasteToolbarButtonList) {
#if wxUSE_TOOLTIPS
      if( Action & TBActTooltips ){
         ComponentInterfaceSymbol command{
            entry.commandName, entry.untranslatedLabel };
         ToolBar::SetButtonToolTip( mProject,
            *mButtons[entry.tool], &command, 1u );
      }
#endif
      if (cm) {
         mButtons[entry.tool]->SetEnabled(cm->GetEnabled(entry.commandName));
      }
   }
}

void CutCopyPasteToolBar::OnButton(wxCommandEvent &event)
{
   int id = event.GetId()-first_TB_ID;
   // Be sure the pop-up happens even if there are exceptions, except for buttons which toggle.
   auto cleanup = finally( [&] { mButtons[id]->InteractionOver();});

   AudacityProject *p = &mProject;
   auto &cm = CommandManager::Get( *p );

   auto flags = MenuManager::Get(*p).GetUpdateFlags();
   const CommandContext context( *p );
   ::HandleTextualCommand( cm,
      CutCopyPasteToolbarButtonList[id].commandName, context, flags, false);

#if defined(__WXMAC__)
   // Bug 2402
   // LLL: It seems that on the Mac the IDLE events are processed
   //      differently than on Windows/GTK and the AdornedRulerPanel's
   //      OnPaint() method gets called sooner that expected. This is
   //      evident when zooming from this toolbar only. When zooming from
   //      the Menu or from keyboard ommand, the zooming works correctly.
   wxTheApp->ProcessIdle();
#endif
}

static RegisteredToolbarFactory factory{ CutCopyPasteBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew CutCopyPasteToolBar{ project } }; }
};

#include "ToolManager.h"

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar for editing */
   CutCopyPasteBarID, wxT("ShowCutCopyPasteTB"), XXO("&Cut/Copy/Paste Toolbar")
};
}

