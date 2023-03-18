/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolsToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

  See ToolsToolBar.h for details

*******************************************************************//*!

\class ToolsToolBar
\brief A kind of ToolBar with Tools on it.

  This class, which is a child of Toolbar, creates the
  window containing the tool selection (ibeam, envelope,
  move, zoom). The window can be embedded within a
  normal project window, or within a ToolBarFrame.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

\see \ref Themability
*//*******************************************************************/



#include "ToolsToolBar.h"
#include "ToolManager.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/sizer.h>
#endif
#include <wx/tooltip.h>

#include "Prefs.h"
#include "AllThemeResources.h"
#include "ImageManipulation.h"
#include "Project.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../tracks/ui/Scrubbing.h"

#include "../widgets/AButton.h"


IMPLEMENT_CLASS(ToolsToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for ToolsToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ToolsToolBar, ToolBar)
   EVT_COMMAND_RANGE(ToolCodes::firstTool + FirstToolID,
                     ToolsToolBar::numTools - 1 + FirstToolID,
                     wxEVT_COMMAND_BUTTON_CLICKED,
                     ToolsToolBar::OnTool)
END_EVENT_TABLE()

Identifier ToolsToolBar::ID()
{
   return wxT("Tools");
}

//Standard constructor
ToolsToolBar::ToolsToolBar( AudacityProject &project )
: ToolBar(project, XO("Tools"), ID())
{
   using namespace ToolCodes;

   //Read the following wxASSERTs as documentating a design decision
   wxASSERT( selectTool   == selectTool   - firstTool );
   wxASSERT( envelopeTool == envelopeTool - firstTool );
   wxASSERT( drawTool     == drawTool     - firstTool );
   wxASSERT( multiTool    == multiTool    - firstTool );
   bool multiToolActive = false;
   gPrefs->Read(wxT("/GUI/ToolBars/Tools/MultiToolActive"), &multiToolActive);

   if (multiToolActive)
      mCurrentTool = multiTool;
   else
      mCurrentTool = selectTool;

   mSubscription = ProjectSettings::Get(project)
      .Subscribe(*this, &ToolsToolBar::OnToolChanged);
}

ToolsToolBar::~ToolsToolBar()
{
   static_assert(
      ToolsToolBar::numTools <= ToolCodes::numTools,
      "mismatch in number of tools" );
}

ToolsToolBar &ToolsToolBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<ToolsToolBar*>(toolManager.GetToolBar(ID()));
}

const ToolsToolBar &ToolsToolBar::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void ToolsToolBar::RegenerateTooltips()
{

// JKC:
//   Under Win98 Tooltips appear to be buggy, when you have a lot of
//   tooltip messages flying around.  I found that just creating a
//   twelfth tooltip caused Audacity to crash when it tried to show
//   any tooltip.
//
//   Win98 does NOT recover from this crash - for any application which is
//   using tooltips will also crash thereafter...  so you must reboot.
//   Rather weird.
//
//   Getting windows to process more of its stacked up messages seems
//   to workaround the problem.  The problem is not fully understood though
//   (as of April 2003).

   //	Vaughan, October 2003: Now we're crashing on Win2K if
   // "Quit when closing last window" is unchecked, when we come back
   // through here, on either of the wxSafeYield calls.
   // James confirms that commenting them out does not cause his original problem
   // to reappear, so they're commented out now.
   //		wxSafeYield(); //Deal with some queued up messages...

   #if wxUSE_TOOLTIPS

   using namespace ToolCodes;

   static const struct Entry {
      int tool;
      CommandID commandName;
      TranslatableString untranslatedLabel;
   } table[] = {
      { selectTool,   wxT("SelectTool"),    XO("Selection Tool")  },
      { envelopeTool, wxT("EnvelopeTool"),  XO("Envelope Tool")   },
      { drawTool,     wxT("DrawTool"),      XO("Draw Tool")       },
      { multiTool,    wxT("MultiTool"),     XO("Multi-Tool")      },
   };

   for (const auto &entry : table) {
      ComponentInterfaceSymbol command{
         entry.commandName, entry.untranslatedLabel };
      ToolBar::SetButtonToolTip( mProject,
         *mTool[entry.tool], &command, 1u );
   }
   #endif

   //		wxSafeYield();
   return;
}

void ToolsToolBar::UpdatePrefs()
{
   RegenerateTooltips();
   ToolBar::UpdatePrefs();
}

AButton * ToolsToolBar::MakeTool(
   ToolsToolBar *pBar, teBmps eTool,
   int id, const TranslatableString &label)
{
   AButton *button = ToolBar::MakeButton(pBar,
      bmpRecoloredUpSmall, 
      bmpRecoloredDownSmall, 
      bmpRecoloredUpHiliteSmall, 
      bmpRecoloredDownSmall, // Not bmpRecoloredHiliteSmall as down is inactive.
      eTool, eTool, eTool,
      wxWindowID(id + FirstToolID),
      wxDefaultPosition, true,
      theTheme.ImageSize( bmpRecoloredUpSmall ));
   button->SetLabel( label );
   pBar->mToolSizer->Add( button );
   return button;
}


void ToolsToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   MakeButtonBackgroundsSmall();

   Add(mToolSizer = safenew wxGridSizer(2, 2, 1, 1));

   /* Tools */
   using namespace ToolCodes;
   mTool[ selectTool   ] = MakeTool( this, bmpIBeam, selectTool, XO("Selection Tool") );
   mTool[ envelopeTool ] = MakeTool( this, bmpEnvelope, envelopeTool, XO("Envelope Tool") );
   mTool[ drawTool     ] = MakeTool( this, bmpDraw, drawTool, XO("Draw Tool") );
   mTool[ multiTool    ] = MakeTool( this, bmpMulti, multiTool, XO("Multi-Tool") );

   DoToolChanged();

   RegenerateTooltips();
}

void ToolsToolBar::OnTool(wxCommandEvent & evt)
{
   // This will cause callback to OnToolChanged
   auto iTool = evt.GetId() - ToolCodes::firstTool - FirstToolID;
   auto pButton = mTool[iTool];
   if (pButton->IsDown())
      ProjectSettings::Get( mProject ).SetTool( iTool );
   else
      // Don't stay up
      pButton->PushDown();
}

void ToolsToolBar::OnToolChanged(ProjectSettingsEvent evt)
{
   if (evt.type != ProjectSettingsEvent::ChangedTool)
      return;
   DoToolChanged();
   ProjectWindow::Get( mProject ).RedrawProject();
}

void ToolsToolBar::DoToolChanged()
{
   auto &projectSettings = ProjectSettings::Get( mProject );
   using namespace ToolCodes;
   mCurrentTool = projectSettings.GetTool() - firstTool;
   for (int i = 0; i < numTools; i++)
      if (i == mCurrentTool)
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   gPrefs->Write(wxT("/GUI/ToolBars/Tools/MultiToolActive"),
                 mTool[multiTool]->IsDown());
   gPrefs->Flush();
}

void ToolsToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

static RegisteredToolbarFactory factory{
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew ToolsToolBar{ project } }; }
};

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows a toolbar
      that has some tools in it */
   ToolsToolBar::ID(), wxT("ShowToolsTB"), XXO("T&ools Toolbar"),
};
}

// Following code injects menu items for changing the current tool

#include "../TrackPanel.h"

// private helper classes and functions
namespace {

/// Called by handlers that set tools.
void SetTool(AudacityProject &project, int tool)
{
   auto toolbar = &ToolsToolBar::Get( project );
   if (toolbar) {
      ProjectSettings::Get(project).SetTool(tool);
      TrackPanel::Get( project ).Refresh(false);
   }
}

}

/// Namespace for functions for View Toolbar menu
namespace {

// Menu handler functions

/// Handler to set the select tool active
void OnSelectTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::selectTool);
}

/// Handler to set the Envelope tool active
void OnEnvelopeTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::envelopeTool);
}

void OnDrawTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::drawTool);
}

void OnMultiTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::multiTool);
}

void OnPrevTool(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &settings = ProjectSettings::Get( project );

   settings.SetTool(
      (settings.GetTool() + (ToolCodes::numTools - 1 )) % ToolCodes::numTools);
   trackPanel.Refresh(false);
}

void OnNextTool(const CommandContext &context)
{
   auto &project = context.project;
   auto &trackPanel = TrackPanel::Get( project );
   auto &settings = ProjectSettings::Get( project );

   settings.SetTool( (settings.GetTool() + 1) % ToolCodes::numTools );
   trackPanel.Refresh(false);
}

using namespace MenuTable;
BaseItemSharedPtr ExtraToolsMenu()
{
   static BaseItemSharedPtr menu{
   Menu( wxT("Tools"), XXO("T&ools"),
      Command( wxT("SelectTool"), XXO("&Selection Tool"), OnSelectTool,
         AlwaysEnabledFlag, wxT("F1") ),
      Command( wxT("EnvelopeTool"), XXO("&Envelope Tool"),
         OnEnvelopeTool, AlwaysEnabledFlag, wxT("F2") ),
      Command( wxT("DrawTool"), XXO("&Draw Tool"), OnDrawTool,
         AlwaysEnabledFlag, wxT("F3") ),
      Command( wxT("MultiTool"), XXO("&Multi Tool"), OnMultiTool,
         AlwaysEnabledFlag, wxT("F6") ),
      Command( wxT("PrevTool"), XXO("&Previous Tool"), OnPrevTool,
         AlwaysEnabledFlag, wxT("A") ),
      Command( wxT("NextTool"), XXO("&Next Tool"), OnNextTool,
         AlwaysEnabledFlag, wxT("D") )
   ) };
   return menu;
}

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part1"),
   Shared( ExtraToolsMenu() )
};
}
