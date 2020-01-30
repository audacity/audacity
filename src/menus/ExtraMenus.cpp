#include "../CommonCommandFlags.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/MixerToolBar.h"
#include "../toolbars/DeviceToolBar.h"

#include <wx/frame.h>

// helper functions and classes
namespace {
}

/// Namespace for helper functions for Extra menu
namespace ExtraActions {

// exported helper functions
// none

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnOutputGain(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &MixerToolBar::Get( project );

   if (tb) {
      tb->ShowOutputGainDialog();
   }
}

void OnOutputGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &MixerToolBar::Get( project );

   if (tb) {
      tb->AdjustOutputGain(1);
   }
}

void OnOutputGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &MixerToolBar::Get( project );

   if (tb) {
      tb->AdjustOutputGain(-1);
   }
}

void OnInputGain(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &MixerToolBar::Get( project );

   if (tb) {
      tb->ShowInputGainDialog();
   }
}

void OnInputGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &MixerToolBar::Get( project );

   if (tb) {
      tb->AdjustInputGain(1);
   }
}

void OnInputGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = &MixerToolBar::Get( project );

   if (tb) {
      tb->AdjustInputGain(-1);
   }
}

void OnInputDevice(const CommandContext &context)
{
   auto &project = context.project;
   auto &tb = DeviceToolBar::Get( project );
   tb.ShowInputDialog();
}

void OnOutputDevice(const CommandContext &context)
{
   auto &project = context.project;
   auto &tb = DeviceToolBar::Get( project );
   tb.ShowOutputDialog();
}

void OnInputChannels(const CommandContext &context)
{
   auto &project = context.project;
   auto &tb = DeviceToolBar::Get( project );
   tb.ShowChannelsDialog();
}

void OnAudioHost(const CommandContext &context)
{
   auto &project = context.project;
   auto &tb = DeviceToolBar::Get( project );
   tb.ShowHostDialog();
}

void OnFullScreen(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = GetProjectFrame( project );
   auto &commandManager = CommandManager::Get( project );

   bool bChecked = !window.wxTopLevelWindow::IsFullScreen();
   window.wxTopLevelWindow::ShowFullScreen(bChecked);
   commandManager.Check(wxT("FullScreenOnOff"), bChecked);
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static ExtraActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) (& ExtraActions::Handler :: X)

// Imported menu item definitions

MenuTable::BaseItemSharedPtr ExtraEditMenu();
MenuTable::BaseItemSharedPtr ExtraSelectionMenu();
MenuTable::BaseItemSharedPtr ExtraCursorMenu();
MenuTable::BaseItemSharedPtr ExtraSeekMenu();
MenuTable::BaseItemSharedPtr ExtraToolsMenu();
MenuTable::BaseItemSharedPtr ExtraTransportMenu();
MenuTable::BaseItemSharedPtr ExtraPlayAtSpeedMenu();
MenuTable::BaseItemSharedPtr ExtraTrackMenu();
MenuTable::BaseItemSharedPtr ExtraScriptablesIMenu();
MenuTable::BaseItemSharedPtr ExtraScriptablesIIMenu();
MenuTable::BaseItemSharedPtr ExtraWindowItems();
MenuTable::BaseItemSharedPtr ExtraGlobalCommands();
MenuTable::BaseItemSharedPtr ExtraFocusMenu();
MenuTable::BaseItemSharedPtr ExtraMenu();
MenuTable::BaseItemSharedPtr ExtraMixerMenu();
MenuTable::BaseItemSharedPtr ExtraDeviceMenu();
MenuTable::BaseItemPtr ExtraMiscItems( AudacityProject & );

MenuTable::BaseItemSharedPtr ExtraMenu()
{
   using namespace MenuTable;

   // Table of menu factories.
   // TODO:  devise a registration system instead.
   static BaseItemSharedPtr extraItems{ Items( wxEmptyString,
      Section( "",
           ExtraTransportMenu()
         , ExtraToolsMenu()
         , ExtraMixerMenu()
         , ExtraEditMenu()
         , ExtraPlayAtSpeedMenu()
         , ExtraSeekMenu()
         , ExtraDeviceMenu()
         , ExtraSelectionMenu()
      ),

      Section( "",
           ExtraGlobalCommands()
         , ExtraFocusMenu()
         , ExtraCursorMenu()
         , ExtraTrackMenu()
         , ExtraScriptablesIMenu()
         , ExtraScriptablesIIMenu()

         // Delayed evaluation:
         , ExtraMiscItems
      )
   ) };

   static const auto pred =
      []{ return gPrefs->ReadBool(wxT("/GUI/ShowExtraMenus"), false); };
   static BaseItemSharedPtr menu{
      ConditionalItems( wxT("Optional"),
         pred, Menu( wxT("Extra"), XO("Ext&ra"), extraItems ) )
   };
   return menu;
}

// Under /MenuBar/Optional/Extra
MenuTable::BaseItemSharedPtr ExtraMixerMenu()
{
   using namespace MenuTable;
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Mixer"), XO("Mi&xer"),
      Command( wxT("OutputGain"), XXO("Ad&just Playback Volume..."),
         FN(OnOutputGain), AlwaysEnabledFlag ),
      Command( wxT("OutputGainInc"), XXO("&Increase Playback Volume"),
         FN(OnOutputGainInc), AlwaysEnabledFlag ),
      Command( wxT("OutputGainDec"), XXO("&Decrease Playback Volume"),
         FN(OnOutputGainDec), AlwaysEnabledFlag ),
      Command( wxT("InputGain"), XXO("Adj&ust Recording Volume..."),
         FN(OnInputGain), AlwaysEnabledFlag ),
      Command( wxT("InputGainInc"), XXO("I&ncrease Recording Volume"),
         FN(OnInputGainInc), AlwaysEnabledFlag ),
      Command( wxT("InputGainDec"), XXO("D&ecrease Recording Volume"),
         FN(OnInputGainDec), AlwaysEnabledFlag )
   ) ) };
   return menu;
}

// Under /MenuBar/Optional/Extra
MenuTable::BaseItemSharedPtr ExtraDeviceMenu()
{
   using namespace MenuTable;
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Device"), XO("De&vice"),
      Command( wxT("InputDevice"), XXO("Change &Recording Device..."),
         FN(OnInputDevice),
         AudioIONotBusyFlag(), wxT("Shift+I") ),
      Command( wxT("OutputDevice"), XXO("Change &Playback Device..."),
         FN(OnOutputDevice),
         AudioIONotBusyFlag(), wxT("Shift+O") ),
      Command( wxT("AudioHost"), XXO("Change Audio &Host..."), FN(OnAudioHost),
         AudioIONotBusyFlag(), wxT("Shift+H") ),
      Command( wxT("InputChannels"), XXO("Change Recording Cha&nnels..."),
         FN(OnInputChannels),
         AudioIONotBusyFlag(), wxT("Shift+N") )
   ) ) };
   return menu;
}

// Under /MenuBar/Optional/Extra
MenuTable::BaseItemPtr ExtraMiscItems( AudacityProject &project )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   constexpr auto key =
#ifdef __WXMAC__
      wxT("Ctrl+/")
#else
      wxT("F11")
#endif
   ;

   // Not a menu.
   return ( FinderScope{ findCommandHandler },
   Items( wxT("Misc"),
      // Accel key is not bindable.
      Command( wxT("FullScreenOnOff"), XXO("&Full Screen (on/off)"),
         FN(OnFullScreen),
         AlwaysEnabledFlag,
         Options{ key }.CheckState(
            GetProjectFrame( project ).wxTopLevelWindow::IsFullScreen() ) ),

      ExtraWindowItems()
   ) );
}

#undef FN
