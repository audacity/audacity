#include "../Audacity.h"
#include "../Experimental.h"

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

void OnShowTransportToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide(TransportBarID);
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowTimerToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( TimerBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowToolsToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( ToolsBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowRecordMeterToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   if( !toolManager.IsVisible( RecordMeterBarID ) )
   {
      toolManager.Expose( MeterBarID, false );
   }
   toolManager.ShowHide( RecordMeterBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowPlayMeterToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   if( !toolManager.IsVisible( PlayMeterBarID ) )
   {
      toolManager.Expose( MeterBarID, false );
   }

   toolManager.ShowHide( PlayMeterBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

#if 0
void OnShowMeterToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   if( !toolManager.IsVisible( MeterBarID ) )
   {
      toolManager.Expose( PlayMeterBarID, false );
      toolManager.Expose( RecordMeterBarID, false );
   }
   toolManager.ShowHide( MeterBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}
#endif

void OnShowMixerToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( MixerBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowEditToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( EditBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowTranscriptionToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( TranscriptionBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowScrubbingToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( ScrubbingBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowDeviceToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( DeviceBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

void OnShowSelectionToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( SelectionBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void OnShowSpectralSelectionToolBar(const CommandContext &context)
{
   auto &project = context.project;
   auto &toolManager = ToolManager::Get( project );

   toolManager.ShowHide( SpectralSelectionBarID );
   MenuManager::Get(project).ModifyToolbarMenus(project);
}
#endif

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

// Under /MenuBar/View
MenuTable::BaseItemSharedPtr ToolbarsMenu()
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   static const auto checkOff = Options{}.CheckState( false );

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Toolbars"), XO("&Toolbars"),
      /* i18n-hint: (verb)*/
      Command( wxT("ResetToolbars"), XXO("Reset Toolb&ars"),
         FN(OnResetToolBars), AlwaysEnabledFlag ),

      Separator(),

      /* i18n-hint: Clicking this menu item shows the toolbar
         with the big buttons on it (play record etc)*/
      Command( wxT("ShowTransportTB"), XXO("&Transport Toolbar"),
         FN(OnShowTransportToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows a toolbar
         that has some tools in it*/
      Command( wxT("ShowToolsTB"), XXO("T&ools Toolbar"),
         FN(OnShowToolsToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar
         with the recording level meters*/
      Command( wxT("ShowRecordMeterTB"), XXO("&Recording Meter Toolbar"),
         FN(OnShowRecordMeterToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar
         with the playback level meter*/
      Command( wxT("ShowPlayMeterTB"), XXO("&Playback Meter Toolbar"),
         FN(OnShowPlayMeterToolBar), AlwaysEnabledFlag, checkOff )

      /* --i18nhint: Clicking this menu item shows the toolbar
         which has sound level meters*/
      //Command( wxT("ShowMeterTB"), XXO("Co&mbined Meter Toolbar"),
      //   FN(OnShowMeterToolBar), AlwaysEnabledFlag, checkOff ),

         ,

      /* i18n-hint: Clicking this menu item shows the toolbar
         with the mixer*/
      Command( wxT("ShowMixerTB"), XXO("Mi&xer Toolbar"),
         FN(OnShowMixerToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar for editing*/
      Command( wxT("ShowEditTB"), XXO("&Edit Toolbar"),
         FN(OnShowEditToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar
         for transcription (currently just vary play speed)*/
      Command( wxT("ShowTranscriptionTB"), XXO("Pla&y-at-Speed Toolbar"),
         FN(OnShowTranscriptionToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar
         that enables Scrub or Seek playback and Scrub Ruler*/
      Command( wxT("ShowScrubbingTB"), XXO("Scru&b Toolbar"),
         FN(OnShowScrubbingToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar
         that manages devices*/
      Command( wxT("ShowDeviceTB"), XXO("&Device Toolbar"),
         FN(OnShowDeviceToolBar), AlwaysEnabledFlag, checkOff ),
      /* i18n-hint: Clicking this menu item shows the toolbar
         for selecting a time range of audio*/
      Command( wxT("ShowSelectionTB"), XXO("&Selection Toolbar"),
         FN(OnShowSelectionToolBar), AlwaysEnabledFlag, checkOff )
#ifdef EXPERIMENTAL_TIMER_TOOLBAR
      ,
      /* i18n-hint: Clicking this menu item shows the toolbar
         for viewing actual time of the cursor*/
      Command( wxT("ShowTimerToolBarTB"), XXO("&Timer Toolbar"),
         FN(OnShowTimerToolBar), AlwaysEnabledFlag, checkOff )
#endif
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      ,
      Command( wxT("ShowSpectralSelectionTB"),
      /* i18n-hint: Clicking this menu item shows the toolbar
         for selecting a frequency range of audio*/
         XXO("Spe&ctral Selection Toolbar"),
         FN(OnShowSpectralSelectionToolBar), AlwaysEnabledFlag, checkOff )
#endif
   ) ) };
   return menu;
}

// Under /MenuBar/Optional/Extra
MenuTable::BaseItemSharedPtr ExtraToolsMenu()
{
   using namespace MenuTable;
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

#undef FN
