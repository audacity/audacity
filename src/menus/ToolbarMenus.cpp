#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ToolManager.h"

/// Namespace for functions for View Toolbar menu
namespace {

// Menu definitions

using namespace MenuTable;

auto ToolbarsMenu()
{
   using Options = CommandManager::Options;
   static auto menu = std::shared_ptr{
   Section( wxT("Toolbars"),
      Menu( wxT("Toolbars"), XXO("&Toolbars"),
         Section( "Reset",
            /* i18n-hint: (verb)*/
            Command( wxT("ResetToolbars"), XXO("Reset Toolb&ars"),
                    ToolManager::OnResetToolBars, AlwaysEnabledFlag )
         ),

         Section( "Other" )
      )
   ) };
   return menu;
}

AttachedItem sAttachment1{ Indirect(ToolbarsMenu()),
   Placement{ wxT("View/Other"), { OrderingHint::Begin } }
};
}
