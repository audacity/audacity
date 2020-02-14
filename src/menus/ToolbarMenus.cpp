#include "../Audacity.h"

#include "../Menus.h"
#include "../TrackPanel.h"
#include "../ProjectSettings.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ToolManager.h"
#include "../toolbars/ToolsToolBar.h"

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
namespace ToolbarActions {

// exported helper functions
// none

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnResetToolBars(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.Reset();
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

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

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static ToolbarActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) (& ToolbarActions::Handler :: X)

namespace{
using namespace MenuTable;

auto ToolbarCheckFn( int toolbarId ) -> CommandListEntry::CheckFn
{
   return [toolbarId](AudacityProject &project){
      auto &toolManager = ToolManager::Get( project );
      return toolManager.IsVisible(toolbarId);
   };
}

BaseItemSharedPtr ToolbarsMenu()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Section( wxT("Toolbars"),
      Menu( wxT("Toolbars"), XO("&Toolbars"),
         Section( "Reset",
            /* i18n-hint: (verb)*/
            Command( wxT("ResetToolbars"), XXO("Reset Toolb&ars"),
               FN(OnResetToolBars), AlwaysEnabledFlag )
         ),

         Section( "Other" )
      )
   ) ) };
   return menu;
}

AttachedItem sAttachment1{
   Placement{ wxT("View/Other"), { OrderingHint::Begin } },
   Shared( ToolbarsMenu() )
};

BaseItemSharedPtr ExtraToolsMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Tools"), XO("T&ools"),
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

AttachedItem sAttachment2{
   wxT("Optional/Extra/Part1"),
   Shared( ExtraToolsMenu() )
};
}

#undef FN
