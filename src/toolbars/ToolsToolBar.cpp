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
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#endif
#include <wx/tooltip.h>

#include "Prefs.h"
#include "../AllThemeResources.h"
#include "../ImageManipulation.h"
#include "../Project.h"
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
                     ToolCodes::lastTool + FirstToolID,
                     wxEVT_COMMAND_BUTTON_CLICKED,
                     ToolsToolBar::OnTool)
END_EVENT_TABLE()

//Standard constructor
ToolsToolBar::ToolsToolBar( AudacityProject &project )
: ToolBar(project, ToolsBarID, XO("Tools"), wxT("Tools"))
{
   using namespace ToolCodes;

   //Read the following wxASSERTs as documentating a design decision
   wxASSERT( selectTool   == selectTool   - firstTool );
   wxASSERT( envelopeTool == envelopeTool - firstTool );
   wxASSERT( slideTool    == slideTool    - firstTool );
   wxASSERT( zoomTool     == zoomTool     - firstTool );
   wxASSERT( drawTool     == drawTool     - firstTool );
   wxASSERT( multiTool    == multiTool    - firstTool );

   bool multiToolActive = false;
   gPrefs->Read(wxT("/GUI/ToolBars/Tools/MultiToolActive"), &multiToolActive);

   if (multiToolActive)
      mCurrentTool = multiTool;
   else
      mCurrentTool = selectTool;
}

ToolsToolBar::~ToolsToolBar()
{
   static_assert( ToolsToolBar::numTools == ToolCodes::numTools,
      "mismatch in number of tools" );
}

ToolsToolBar &ToolsToolBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<ToolsToolBar*>( toolManager.GetToolBar(ToolsBarID) );
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
      { slideTool,    wxT("TimeShiftTool"), XO("Time Shift Tool") },
      { zoomTool,     wxT("ZoomTool"),      XO("Zoom Tool")       },
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
   Add(mToolSizer = safenew wxGridSizer(2, 3, 1, 1));

   /* Tools */
   using namespace ToolCodes;
   mTool[ selectTool   ] = MakeTool( this, bmpIBeam, selectTool, XO("Selection Tool") );
   mTool[ envelopeTool ] = MakeTool( this, bmpEnvelope, envelopeTool, XO("Envelope Tool") );
   mTool[ drawTool     ] = MakeTool( this, bmpDraw, drawTool, XO("Draw Tool") );
   mTool[ zoomTool     ] = MakeTool( this, bmpZoom, zoomTool, XO("Zoom Tool") );
   mTool[ slideTool    ] = MakeTool( this, bmpTimeShift, slideTool, XO("Slide Tool") );
   mTool[ multiTool    ] = MakeTool( this, bmpMulti, multiTool, XO("Multi-Tool") );

   // It's OK to reset the tool when regenerating this, e.g after visiting preferences.
   SetCurrentTool( selectTool );
   mTool[mCurrentTool]->PushDown();

   RegenerateTooltips();
}

/// Gets the currently active tool
/// In Multi-mode this might not return the multi-tool itself
/// since the active tool may be changed by what you hover over.
int ToolsToolBar::GetCurrentTool() const
{
   return mCurrentTool;
}

/// Sets the currently active tool
/// @param tool - The index of the tool to be used.
void ToolsToolBar::SetCurrentTool(int tool)
{
   //In multi-mode the current tool is shown by the
   //cursor icon.  The buttons are not updated.

   using namespace ToolCodes;
   bool leavingMulticlipMode =
      IsDown(multiTool) && tool != multiTool;

   if (leavingMulticlipMode)
      mTool[multiTool]->PopUp();

   if (tool != mCurrentTool || leavingMulticlipMode) {
      mTool[mCurrentTool]->PopUp();
      mCurrentTool=tool;
      mTool[mCurrentTool]->PushDown();
   }
   //JKC: ANSWER-ME: Why is this required?
   //msmeyer: I think it isn't, we leave it out for 1.3.1 (beta), and
   // we'll see if anyone complains.
   //for ( auto pProject : AllProjects{} )
   //   ProjectWindow::Get( *pProject ).RedrawProject();

   gPrefs->Write(wxT("/GUI/ToolBars/Tools/MultiToolActive"),
                 IsDown(multiTool));
   gPrefs->Flush();

   ProjectSettings::Get( mProject ).SetTool( mCurrentTool );
}

bool ToolsToolBar::IsDown(int tool) const
{
   return mTool[tool]->IsDown();
}

int ToolsToolBar::GetDownTool()
{
   int tool;

   using namespace ToolCodes;
   for (tool = firstTool; tool <= lastTool; tool++)
      if (IsDown(tool))
         return tool;

   return firstTool;  // Should never happen
}

void ToolsToolBar::OnTool(wxCommandEvent & evt)
{
   using namespace ToolCodes;
   mCurrentTool = evt.GetId() - firstTool - FirstToolID;
   for (int i = 0; i < numTools; i++)
      if (i == mCurrentTool)
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   for ( auto pProject : AllProjects{} )
      ProjectWindow::Get( *pProject ).RedrawProject();

   gPrefs->Write(wxT("/GUI/ToolBars/Tools/MultiToolActive"),
                 IsDown(multiTool));
   gPrefs->Flush();

   ProjectSettings::Get( mProject ).SetTool( mCurrentTool );
}

void ToolsToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

static RegisteredToolbarFactory factory{ ToolsBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew ToolsToolBar{ project } }; }
};

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows a toolbar
      that has some tools in it */
   ToolsBarID, wxT("ShowToolsTB"), XXO("T&ools Toolbar"),
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
      toolbar->SetCurrentTool(tool);
      TrackPanel::Get( project ).Refresh(false);
   }
}

}

/// Namespace for functions for View Toolbar menu
namespace ToolActions {

// exported helper functions
// none

// Menu handler functions

struct Handler : CommandHandlerObject {
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

/// Handler to set the Zoom tool active
void OnZoomTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::zoomTool);
}

/// Handler to set the Time shift tool active
void OnTimeShiftTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::slideTool);
}

void OnMultiTool(const CommandContext &context)
{
   SetTool(context.project, ToolCodes::multiTool);
}

void OnPrevTool(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolbar = ToolsToolBar::Get( project );
   auto &trackPanel = TrackPanel::Get( project );

   using namespace ToolCodes;
   // Use GetDownTool() here since GetCurrentTool() can return a value that
   // doesn't represent the real tool if the Multi-tool is being used.
   toolbar.SetCurrentTool((toolbar.GetDownTool()+(numTools-1))%numTools);
   trackPanel.Refresh(false);
}

void OnNextTool(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolbar = ToolsToolBar::Get( project );
   auto &trackPanel = TrackPanel::Get( project );

   using namespace ToolCodes;
   // Use GetDownTool() here since GetCurrentTool() can return a value that
   // doesn't represent the real tool if the Multi-tool is being used.
   toolbar.SetCurrentTool((toolbar.GetDownTool()+1)%numTools);
   trackPanel.Refresh(false);
}

}; // struct Handler

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static ToolActions::Handler instance;
   return instance;
};

#define FN(X) (& ToolActions::Handler :: X)

using namespace MenuTable;
BaseItemSharedPtr ExtraToolsMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Tools"), XXO("T&ools"),
      Command( wxT("SelectTool"), XXO("&Selection Tool"), FN(OnSelectTool),
         AlwaysEnabledFlag, wxT("F1") ),
      Command( wxT("EnvelopeTool"), XXO("&Envelope Tool"),
         FN(OnEnvelopeTool), AlwaysEnabledFlag, wxT("F2") ),
      Command( wxT("DrawTool"), XXO("&Draw Tool"), FN(OnDrawTool),
         AlwaysEnabledFlag, wxT("F3") ),
      Command( wxT("ZoomTool"), XXO("&Zoom Tool"), FN(OnZoomTool),
         AlwaysEnabledFlag, wxT("F4") ),
      Command( wxT("TimeShiftTool"), XXO("&Time Shift Tool"),
         FN(OnTimeShiftTool), AlwaysEnabledFlag, wxT("F5") ),
      Command( wxT("MultiTool"), XXO("&Multi Tool"), FN(OnMultiTool),
         AlwaysEnabledFlag, wxT("F6") ),
      Command( wxT("PrevTool"), XXO("&Previous Tool"), FN(OnPrevTool),
         AlwaysEnabledFlag, wxT("A") ),
      Command( wxT("NextTool"), XXO("&Next Tool"), FN(OnNextTool),
         AlwaysEnabledFlag, wxT("D") )
   ) ) };
   return menu;
}

#undef FN

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part1"),
   Shared( ExtraToolsMenu() )
};
}
