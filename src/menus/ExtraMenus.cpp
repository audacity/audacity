#include "../Prefs.h"
#include "../Project.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/MixerToolBar.h"
#include "../toolbars/DeviceToolBar.h"

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
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->ShowOutputGainDialog();
   }
}

void OnOutputGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustOutputGain(1);
   }
}

void OnOutputGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustOutputGain(-1);
   }
}

void OnInputGain(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->ShowInputGainDialog();
   }
}

void OnInputGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustInputGain(1);
   }
}

void OnInputGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustInputGain(-1);
   }
}

void OnInputDevice(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowInputDialog();
   }
}

void OnOutputDevice(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowOutputDialog();
   }
}

void OnInputChannels(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowChannelsDialog();
   }
}

void OnAudioHost(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowHostDialog();
   }
}

void OnFullScreen(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   bool bChecked = !project.wxTopLevelWindow::IsFullScreen();
   project.wxTopLevelWindow::ShowFullScreen(bChecked);
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

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& ExtraActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

// Imported menu item definitions

MenuTable::BaseItemPtr ExtraEditMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraSelectionMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraCursorMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraSeekMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraToolsMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraTransportMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraPlayAtSpeedMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraTrackMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraScriptablesIMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraScriptablesIIMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraWindowItems( AudacityProject & );
MenuTable::BaseItemPtr ExtraGlobalCommands( AudacityProject & );
MenuTable::BaseItemPtr ExtraFocusMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraMenu( AudacityProject& );
MenuTable::BaseItemPtr ExtraMixerMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraDeviceMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraMiscItems( AudacityProject & );

// Table of menu factories.
// TODO:  devise a registration system instead.
static const std::shared_ptr<MenuTable::BaseItem> extraItems = MenuTable::Items(
   ExtraTransportMenu
   , ExtraToolsMenu
   , ExtraMixerMenu
   , ExtraEditMenu
   , ExtraPlayAtSpeedMenu
   , ExtraSeekMenu
   , ExtraDeviceMenu
   , ExtraSelectionMenu

   , MenuTable::Separator()

   , ExtraGlobalCommands
   , ExtraFocusMenu
   , ExtraCursorMenu
   , ExtraTrackMenu
   , ExtraScriptablesIMenu
   , ExtraScriptablesIIMenu
   , ExtraMiscItems
);

MenuTable::BaseItemPtr ExtraMenu( AudacityProject & )
{
   using namespace MenuTable;
   static const auto pred =
      []{ return gPrefs->ReadBool(wxT("/GUI/ShowExtraMenus"), false); };
   static const auto factory =
      [](AudacityProject &){ return extraItems; };
   return ConditionalItems( pred, Menu( _("Ext&ra"), factory ) );
}

MenuTable::BaseItemPtr ExtraMixerMenu( AudacityProject & )
{
   using namespace MenuTable;
   return Menu( _("Mi&xer"),
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
   );
}

MenuTable::BaseItemPtr ExtraDeviceMenu( AudacityProject & )
{
   using namespace MenuTable;
   return Menu( _("De&vice"),
      Command( wxT("InputDevice"), XXO("Change &Recording Device..."),
         FN(OnInputDevice),
         AudioIONotBusyFlag, wxT("Shift+I") ),
      Command( wxT("OutputDevice"), XXO("Change &Playback Device..."),
         FN(OnOutputDevice),
         AudioIONotBusyFlag, wxT("Shift+O") ),
      Command( wxT("AudioHost"), XXO("Change Audio &Host..."), FN(OnAudioHost),
         AudioIONotBusyFlag, wxT("Shift+H") ),
      Command( wxT("InputChannels"), XXO("Change Recording Cha&nnels..."),
         FN(OnInputChannels),
         AudioIONotBusyFlag, wxT("Shift+N") )
   );
}

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
   return Items(
      // Accel key is not bindable.
      Command( wxT("FullScreenOnOff"), XXO("&Full Screen (on/off)"),
         FN(OnFullScreen),
         AlwaysEnabledFlag,
         Options{ key }
            .CheckState( project.wxTopLevelWindow::IsFullScreen() ) ),

      ExtraWindowItems
   );
}

#undef XXO
#undef FN
