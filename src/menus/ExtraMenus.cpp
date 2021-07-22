#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "Prefs.h"
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

   bool bChecked = !window.wxTopLevelWindow::IsFullScreen();
   window.wxTopLevelWindow::ShowFullScreen(bChecked);

   MenuManager::Get(project).ModifyToolbarMenus(project);
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

namespace {
using namespace MenuTable;

BaseItemSharedPtr ExtraMixerMenu();
BaseItemSharedPtr ExtraDeviceMenu();

BaseItemSharedPtr ExtraMenu()
{
   // Table of menu factories.
   // TODO:  devise a registration system instead.
   static BaseItemSharedPtr extraItems{ Items( wxEmptyString,
      Section( "Part1",
           ExtraMixerMenu()
         , ExtraDeviceMenu()
      ),

      Section( "Part2" )
   ) };

   static const auto pred =
      []{ return gPrefs->ReadBool(wxT("/GUI/ShowExtraMenus"), false); };
   static BaseItemSharedPtr menu{
      ConditionalItems( wxT("Optional"),
         pred, Menu( wxT("Extra"), XXO("Ext&ra"), extraItems ) )
   };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( ExtraMenu() )
};

// Under /MenuBar/Optional/Extra/Part1
BaseItemSharedPtr ExtraMixerMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Mixer"), XXO("Mi&xer"),
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

// Under /MenuBar/Optional/Extra/Part1
BaseItemSharedPtr ExtraDeviceMenu()
{
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Device"), XXO("De&vice"),
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

// Under /MenuBar/Optional/Extra/Part2
BaseItemSharedPtr ExtraMiscItems()
{
   using Options = CommandManager::Options;

   // Not a menu.
   static BaseItemSharedPtr items{
   Items( wxT("Misc"),
      // Delayed evaluation
      []( AudacityProject &project ) {

   static const auto key =
#ifdef __WXMAC__
      wxT("Ctrl+/")
#else
      wxT("F11")
#endif
   ;

         return (
         FinderScope{ findCommandHandler },
         // Accel key is not bindable.
         Command( wxT("FullScreenOnOff"), XXO("&Full Screen (on/off)"),
            FN(OnFullScreen),
            AlwaysEnabledFlag,
            Options{ key }.CheckTest( []( const AudacityProject &project ) {
               return GetProjectFrame( project )
                  .wxTopLevelWindow::IsFullScreen(); } ) )
        );
      }
   ) };
   return items;
}

AttachedItem sAttachment2{
   Placement{ wxT("Optional/Extra/Part2"), { OrderingHint::End } },
   Shared( ExtraMiscItems() )
};

}

#undef FN
