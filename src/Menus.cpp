/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et al.

*******************************************************************//**

\file Menus.cpp
\brief Functions that provide most of the menu actions.

  This file implements the method that creates the menu bar, plus
  most of the methods that get called when you select an item
  from a menu.

*//****************************************************************//**

\class MenuCommandHandler
\brief MenuCommandHandler contains many command handlers for individual 
menu items.

*//****************************************************************//**

\class MenuCreator
\brief MenuCreator is responsible for creating the main menu bar.

*//****************************************************************//**

\class MenuManager
\brief MenuManager handles updates to menu state.

*//*******************************************************************/

#include "Audacity.h"
#include "Menus.h"
#include "commands/CommandManager.h"
#include "commands/CommandContext.h"

#include <cfloat>
#include <iterator>
#include <algorithm>
#include <limits>
#include <math.h>


#include <wx/defs.h>
#include <wx/docview.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>
#include <wx/textdlg.h>
#include <wx/progdlg.h>
#include <wx/scrolbar.h>
#include <wx/ffile.h>
#include <wx/statusbr.h>
#include <wx/utils.h>

#include "FreqWindow.h"
#include "effects/Contrast.h"
#include "TrackPanel.h"

#include "effects/EffectManager.h"

#include "AudacityApp.h"
#include "AudacityLogger.h"
#include "AudioIO.h"
#include "Dependencies.h"
#include "float_cast.h"
#include "LabelTrack.h"
#include "import/ImportRaw.h"
#include "prefs/PrefsDialog.h"
#include "prefs/PlaybackPrefs.h"
#include "ShuttleGui.h"
#include "LyricsWindow.h"
#include "MixerBoard.h"
#include "Project.h"
#include "Internat.h"
#include "FileFormats.h"
#include "ModuleManager.h"
#include "PluginManager.h"
#include "Prefs.h"
#ifdef USE_MIDI
#include "NoteTrack.h"
#endif // USE_MIDI
#include "TimeTrack.h"
#include "Mix.h"
#include "AboutDialog.h"
#include "Benchmark.h"
#include "Screenshot.h"
#include "ondemand/ODManager.h"

#include "BatchProcessDialog.h"
#include "prefs/BatchPrefs.h"

#include "toolbars/ToolManager.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/EditToolBar.h"
#include "toolbars/DeviceToolBar.h"
#include "toolbars/MixerToolBar.h"

#include "tracks/ui/SelectHandle.h"

#include "widgets/LinkingHtmlWindow.h"

#include "Experimental.h"
#include "PlatformCompatibility.h"
#include "FileNames.h"

#include "SplashDialog.h"
#include "widgets/HelpSystem.h"

#include "UndoManager.h"
#include "WaveTrack.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)
#include <wx/debugrpt.h>
#endif

#ifdef EXPERIMENTAL_SCOREALIGN
#include "effects/ScoreAlignDialog.h"
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#endif /* EXPERIMENTAL_SCOREALIGN */

#include "prefs/TracksPrefs.h"

#include "widgets/Meter.h"
#include "widgets/ErrorDialog.h"
#include "./commands/AudacityCommand.h"

static MenuTable::BaseItemPtrs PopulateMacrosMenu(
   CommandFlag flags  );
static MenuTable::BaseItemPtrs PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   CommandFlag realflags);
static void AddEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   bool isDefault);
static void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const wxArrayString & names,
   const std::vector<bool> &vHasDialog,
   const PluginIDList & plugs,
   const std::vector<CommandFlag> & flags,
   bool isDefault);

constexpr size_t kAlignLabelsCount = 5;

static const AudacityProject::RegisteredAttachedObjectFactory factory{ []{
   return std::make_unique< MenuCommandHandler >();
} };

PrefsListener::~PrefsListener()
{
}

void PrefsListener::UpdatePrefs()
{
}

MenuCommandHandler &GetMenuCommandHandler(AudacityProject &project)
{
   return static_cast<MenuCommandHandler&>(
      project.GetAttachedObject( factory ) );
}

MenuManager &GetMenuManager(AudacityProject &project)
{ return *project.mMenuManager; }

MenuCommandHandler::MenuCommandHandler()
{
}

MenuCommandHandler::~MenuCommandHandler()
{
}

MenuCreator::MenuCreator(){
}

MenuCreator::~MenuCreator()
{
}

enum {
   kAlignStartZero = 0,
   kAlignStartSelStart,
   kAlignStartSelEnd,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   // The next two are only in one subMenu, so more easily handled at the end.
   kAlignEndToEnd,
   kAlignTogether
};

#include "commands/ScreenshotCommand.h"


//
// Effects menu arrays
//
static bool SortEffectsByName(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto akey = a->GetSymbol().Translation();
   auto bkey = b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

static bool SortEffectsByPublisher(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (akey.IsEmpty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.IsEmpty())
   {
      bkey = _("Uncategorized");
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

static bool SortEffectsByPublisherAndName(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (a->IsEffectDefault())
   {
      akey = wxEmptyString;
   }
   if (b->IsEffectDefault())
   {
      bkey = wxEmptyString;
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

static bool SortEffectsByTypeAndName(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.IsEmpty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.IsEmpty())
   {
      bkey = _("Uncategorized");
   }

   if (a->IsEffectDefault())
   {
      akey = wxEmptyString;
   }
   if (b->IsEffectDefault())
   {
      bkey = wxEmptyString;
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

static bool SortEffectsByType(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.IsEmpty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.IsEmpty())
   {
      bkey = _("Uncategorized");
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

void MenuCommandHandler::UpdatePrefs()
{
   gPrefs->Read(wxT("/GUI/CircularTrackNavigation"), &mCircularTrackNavigation,
                false);
}

void MenuManager::UpdatePrefs()
{
   bool bSelectAllIfNone;
   gPrefs->Read(wxT("/GUI/SelectAllOnNone"), &bSelectAllIfNone, false);
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
#ifdef EXPERIMENTAL_DA
   // DA warns or greys out.
   mWhatIfNoSelection = bSelectAllIfNone ? 2 : 0;
#else
   // Audacity autoselects or warns.
   mWhatIfNoSelection = bSelectAllIfNone ? 1 : 2;
#endif
   mStopIfWasPaused = true;  // not configurable for now, but could be later.
}

namespace MenuTable {

BaseItem::~BaseItem() {}

ComputedItem::~ComputedItem() {}

GroupItem::GroupItem( BaseItemPtrs &&items_ )
: items{ std::move( items_ ) }
{
}
void GroupItem::AppendOne( BaseItemPtr&& ptr )
{
   items.push_back( std::move( ptr ) );
}
GroupItem::~GroupItem() {}

MenuItem::MenuItem( const wxString &title_, BaseItemPtrs &&items_ )
: GroupItem{ std::move( items_ ) }, title{ title_ }
{
   wxASSERT( !title.empty() );
}
MenuItem::~MenuItem() {}

ConditionalGroupItem::ConditionalGroupItem(
   Condition condition_, BaseItemPtrs &&items_ )
: GroupItem{ std::move( items_ ) }, condition{ condition_ }
{
}
ConditionalGroupItem::~ConditionalGroupItem() {}

SeparatorItem::~SeparatorItem() {}

CommandItem::CommandItem(const wxString &name_,
         const wxString &label_in_,
         bool hasDialog_,
         CommandHandlerFinder finder_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         const CommandManager::Options &options_)
: name{ name_ }, label_in{ label_in_ }, hasDialog{ hasDialog_ }
, finder{ finder_ }, callback{ callback_ }
, flags{ flags_ }, options{ options_ }
{}
CommandItem::~CommandItem() {}

CommandGroupItem::CommandGroupItem(const wxString &name_,
         const IdentInterfaceSymbol items_[],
         size_t nItems_,
         CommandHandlerFinder finder_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         bool isEffect_)
: name{ name_ }, items{ items_, items_ + nItems_ }
, finder{ finder_ }, callback{ callback_ }
, flags{ flags_ }, isEffect{ isEffect_ }
{}
CommandGroupItem::~CommandGroupItem() {}

SpecialItem::~SpecialItem() {}

}

namespace {

void VisitItem( AudacityProject &project, MenuTable::BaseItem *pItem );

void VisitItems(
   AudacityProject &project, const MenuTable::BaseItemPtrs &items )
{
   for ( auto &pSubItem : items )
      VisitItem( project, pSubItem.get() );
}

void VisitItem( AudacityProject &project, MenuTable::BaseItem *pItem )
{
   if (!pItem)
      return;

   auto &manager = *project.GetCommandManager();

   using namespace MenuTable;
   if (const auto pComputed =
       dynamic_cast<ComputedItem*>( pItem )) {
      // TODO maybe?  memo-ize the results of the function, but that requires
      // invalidating the memo at the right times
      auto result = pComputed->factory( project );
      if (result)
         // recursion
         VisitItem( project, result.get() );
   }
   else
   if (const auto pCommand =
       dynamic_cast<CommandItem*>( pItem )) {
      manager.AddItem(
         pCommand->name, pCommand->label_in, pCommand->hasDialog,
         pCommand->finder, pCommand->callback,
         pCommand->flags, pCommand->options
      );
   }
   else
   if (const auto pCommandList =
      dynamic_cast<CommandGroupItem*>( pItem ) ) {
      manager.AddItemList(pCommandList->name,
         pCommandList->items.data(), pCommandList->items.size(),
         pCommandList->finder, pCommandList->callback,
         pCommandList->flags, pCommandList->isEffect);
   }
   else
   if (const auto pMenu =
       dynamic_cast<MenuItem*>( pItem )) {
      manager.BeginMenu( pMenu->title );
      // recursion
      VisitItems( project, pMenu->items );
      manager.EndMenu();
   }
   else
   if (const auto pConditionalGroup =
       dynamic_cast<ConditionalGroupItem*>( pItem )) {
      const auto flag = pConditionalGroup->condition();
      if (!flag)
         manager.BeginOccultCommands();
      // recursion
      VisitItems( project, pConditionalGroup->items );
      if (!flag)
         manager.EndOccultCommands();
   }
   else
   if (const auto pGroup =
       dynamic_cast<GroupItem*>( pItem )) {
      // recursion
      VisitItems( project, pGroup->items );
   }
   else
   if (dynamic_cast<SeparatorItem*>( pItem )) {
      manager.AddSeparator();
   }
   else
   if (const auto pSpecial =
       dynamic_cast<SpecialItem*>( pItem )) {
      const auto pMenu = manager.CurrentMenu();
      wxASSERT( pMenu );
      pSpecial->fn( project, *pMenu );
   }
   else
      wxASSERT( false );
}

}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

// To supply the "finder" argument in AddItem calls
static CommandHandlerObject &findMenuCommandHandler(AudacityProject &project)
{ return GetMenuCommandHandler( project ); }

#define FN(X) findMenuCommandHandler, \
   static_cast<CommandFunctorPointer>(& MenuCommandHandler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr FileMenu( AudacityProject& );

MenuTable::BaseItemPtr EditMenu( AudacityProject& );
MenuTable::BaseItemPtr ExtraEditMenu( AudacityProject & );

MenuTable::BaseItemPtr SelectMenu( AudacityProject& );
MenuTable::BaseItemPtr ExtraSelectionMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraCursorMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraSeekMenu( AudacityProject & );

MenuTable::BaseItemPtr ExtraToolsMenu( AudacityProject & );

MenuTable::BaseItemPtr ViewMenu( AudacityProject& );

MenuTable::BaseItemPtr TransportMenu( AudacityProject& );
MenuTable::BaseItemPtr ExtraTransportMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraPlayAtSpeedMenu( AudacityProject & );

namespace {
MenuTable::BaseItemPtr TracksMenu( AudacityProject& );
MenuTable::BaseItemPtr GenerateMenu( AudacityProject& );
MenuTable::BaseItemPtr EffectMenu( AudacityProject& );
MenuTable::BaseItemPtr AnalyzeMenu( AudacityProject& );
MenuTable::BaseItemPtr ToolsMenu( AudacityProject& );
MenuTable::BaseItemPtr WindowMenu( AudacityProject& );

MenuTable::BaseItemPtr ExtraMenu( AudacityProject& );
MenuTable::BaseItemPtr ExtraMixerMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraDeviceMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraGlobalCommands( AudacityProject & );
MenuTable::BaseItemPtr ExtraFocusMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraTrackMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraScriptablesIMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraScriptablesIIMenu( AudacityProject & );
MenuTable::BaseItemPtr ExtraMiscItems( AudacityProject & );

MenuTable::BaseItemPtr HelpMenu( AudacityProject& );
}

// Tables of menu factories.
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

static const auto menuTree = MenuTable::Items(
   FileMenu
   , EditMenu
   , SelectMenu
   , ViewMenu
   , TransportMenu
   , TracksMenu
   , GenerateMenu
   , EffectMenu
   , AnalyzeMenu
   , ToolsMenu
   , WindowMenu
   , ExtraMenu
   , HelpMenu
);

namespace {

MenuTable::BaseItemPtr TracksMenu( AudacityProject & )
{
   // Tracks Menu (formerly Project Menu)
   using namespace MenuTable;
   using Options = CommandManager::Options;
   
   return Menu( _("&Tracks"),
      Menu( _("Add &New"),
         Command( wxT("NewMonoTrack"), XXO("&Mono Track"), FN(OnNewWaveTrack),
            AudioIONotBusyFlag, wxT("Ctrl+Shift+N") ),
         Command( wxT("NewStereoTrack"), XXO("&Stereo Track"),
            FN(OnNewStereoTrack), AudioIONotBusyFlag ),
         Command( wxT("NewLabelTrack"), XXO("&Label Track"),
            FN(OnNewLabelTrack), AudioIONotBusyFlag ),
         Command( wxT("NewTimeTrack"), XXO("&Time Track"),
            FN(OnNewTimeTrack), AudioIONotBusyFlag )
      ),

      //////////////////////////////////////////////////////////////////////////

      Separator(),

      Menu( _("Mi&x"),
         // Stereo to Mono is an oddball command that is also subject to control by the
         // plug-in manager, as if an effect.  Decide whether to show or hide it.
         [](AudacityProject&) -> BaseItemPtr {
            const PluginID ID =
               EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono"));
            const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
            if (plug && plug->IsEnabled())
               return Command( wxT("Stereo to Mono"),
                  XXO("Mix Stereo Down to &Mono"), FN(OnStereoToMono),
                  AudioIONotBusyFlag | StereoRequiredFlag |
                     WaveTracksSelectedFlag );
            else
               return {};
         },
         Command( wxT("MixAndRender"), XXO("Mi&x and Render"), FN(OnMixAndRender),
            AudioIONotBusyFlag | WaveTracksSelectedFlag ),
         Command( wxT("MixAndRenderToNewTrack"), XXO("Mix and Render to Ne&w Track"), FN(OnMixAndRenderToNewTrack),
            AudioIONotBusyFlag | WaveTracksSelectedFlag, wxT("Ctrl+Shift+M") )
      ),

      Command( wxT("Resample"), XXO("&Resample..."), FN(OnResample),
         AudioIONotBusyFlag | WaveTracksSelectedFlag ),

      Separator(),

      Command( wxT("RemoveTracks"), XXO("Remo&ve Tracks"), FN(OnRemoveTracks),
         AudioIONotBusyFlag | TracksSelectedFlag ),

      Separator(),

      Menu( _("M&ute/Unmute"),
         Command( wxT("MuteAllTracks"), XXO("&Mute All Tracks"),
            FN(OnMuteAllTracks), AudioIONotBusyFlag, wxT("Ctrl+U") ),
         Command( wxT("UnmuteAllTracks"), XXO("&Unmute All Tracks"),
            FN(OnUnmuteAllTracks), AudioIONotBusyFlag, wxT("Ctrl+Shift+U") )
      ),

      Menu( _("&Pan"),
         // As Pan changes are not saved on Undo stack,
         // pan settings for all tracks
         // in the project could very easily be lost unless we
         // require the tracks to be selected.
         Command( wxT("PanLeft"), XXO("&Left"), FN(OnPanLeft),
            TracksSelectedFlag,
            Options{}.LongName( _("Pan Left") ) ),
         Command( wxT("PanRight"), XXO("&Right"), FN(OnPanRight),
            TracksSelectedFlag,
            Options{}.LongName( _("Pan Right") ) ),
         Command( wxT("PanCenter"), XXO("&Center"), FN(OnPanCenter),
            TracksSelectedFlag,
            Options{}.LongName( _("Pan Center") ) )
      ),

      Separator(),

      //////////////////////////////////////////////////////////////////////////

      Menu( _("&Align Tracks"), //_("Just Move Tracks"),
         []{
            // Mutual alignment of tracks independent of selection or zero
            static const IdentInterfaceSymbol alignLabelsNoSync[] = {
               { wxT("EndToEnd"),     XO("&Align End to End") },
               { wxT("Together"),     XO("Align &Together") },
            };
            return CommandGroup(wxT("Align"),
               alignLabelsNoSync, 2u, FN(OnAlignNoSync),
               AudioIONotBusyFlag | TracksSelectedFlag);
         }(),

         Separator(),

         []{
            // Alignment commands using selection or zero
            static const IdentInterfaceSymbol alignLabels[] = {
               { wxT("StartToZero"),     XO("Start to &Zero") },
               { wxT("StartToSelStart"), XO("Start to &Cursor/Selection Start") },
               { wxT("StartToSelEnd"),   XO("Start to Selection &End") },
               { wxT("EndToSelStart"),   XO("End to Cu&rsor/Selection Start") },
               { wxT("EndToSelEnd"),     XO("End to Selection En&d") },
            };
            static_assert(
               kAlignLabelsCount == sizeof(alignLabels) / sizeof(alignLabels[0]),
               "incorrect count"
            );
            return CommandGroup(wxT("Align"),
               alignLabels, kAlignLabelsCount, FN(OnAlign),
               AudioIONotBusyFlag | TracksSelectedFlag);
         }(),

         Separator(),

         Command( wxT("MoveSelectionWithTracks"),
            XXO("&Move Selection with Tracks (on/off)"),
            FN(OnMoveSelectionWithTracks),
            AlwaysEnabledFlag,
            Options{}.CheckState(
               gPrefs->Read(wxT("/GUI/MoveSelectionWithTracks"), 0L ) ) )
      ),

#if 0
      // TODO: Can these labels be made clearer?
      // Do we need this sub-menu at all?
      Menu( _("Move Sele&ction and Tracks"), {
         CommandGroup(wxT("AlignMove"), alignLabels, kAlignLabelsCount,
            FN(OnAlignMoveSel), AudioIONotBusyFlag | TracksSelectedFlag),
      } ),
#endif

      //////////////////////////////////////////////////////////////////////////

#ifdef EXPERIMENTAL_SCOREALIGN
      Command( wxT("ScoreAlign"), XXO("Synchronize MIDI with Audio"),
         FN(OnScoreAlign),
         AudioIONotBusyFlag | NoteTracksSelectedFlag | WaveTracksSelectedFlag ),
#endif // EXPERIMENTAL_SCOREALIGN

      //////////////////////////////////////////////////////////////////////////

      Menu( _("S&ort Tracks"),
         Command( wxT("SortByTime"), XXO("By &Start Time"), FN(OnSortTime),
            TracksExistFlag,
            Options{}.LongName( _("Sort by Time") ) ),
         Command( wxT("SortByName"), XXO("By &Name"), FN(OnSortName),
            TracksExistFlag,
            Options{}.LongName( _("Sort by Name") ) )
      )

      //////////////////////////////////////////////////////////////////////////

#ifdef EXPERIMENTAL_SYNC_LOCK

      ,
      Separator(),

      Command( wxT("SyncLock"), XXO("Sync-&Lock Tracks (on/off)"),
         FN(OnSyncLock), AlwaysEnabledFlag,
         Options{}.CheckState( gPrefs->Read(wxT("/GUI/SyncLockTracks"), 0L) ) )

#endif
   );
}

MenuTable::BaseItemPtr GenerateMenu( AudacityProject & )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   return Menu( _("&Generate"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageGenerators"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageGenerators), AudioIONotBusyFlag ),

      Separator(),

#endif

      Items( PopulateEffectsMenu(
         EffectTypeGenerate,
         AudioIONotBusyFlag,
         AudioIONotBusyFlag) )
   );
}

MenuTable::BaseItemPtr EffectMenu( AudacityProject &project )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   const auto &lastEffect = GetMenuManager(project).mLastEffect;
   wxString buildMenuLabel;
   if (!lastEffect.IsEmpty()) {
      buildMenuLabel.Printf(_("Repeat %s"),
         EffectManager::Get().GetCommandName(lastEffect));
   }
   else
      buildMenuLabel = _("Repeat Last Effect");

   return Menu( _("Effe&ct"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageEffects"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageEffects), AudioIONotBusyFlag ),

      Separator(),

#endif
      Command( wxT("RepeatLastEffect"), buildMenuLabel, false, FN(OnRepeatLastEffect),
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag, wxT("Ctrl+R") ),

      Separator(),

      Items( PopulateEffectsMenu(
         EffectTypeProcess,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         IsRealtimeNotActiveFlag ) )
   );
}

MenuTable::BaseItemPtr AnalyzeMenu( AudacityProject & )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   return Menu( _("&Analyze"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageAnalyzers"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageAnalyzers), AudioIONotBusyFlag ),

      Separator(),

#endif

      Command( wxT("ContrastAnalyser"), XXO("Contrast..."), FN(OnContrast),
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag, wxT("Ctrl+Shift+T") ),
      Command( wxT("PlotSpectrum"), XXO("Plot Spectrum..."), FN(OnPlotSpectrum),
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag ),

      Items( PopulateEffectsMenu(
         EffectTypeAnalyze,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         IsRealtimeNotActiveFlag ) )
   );
}

MenuTable::BaseItemPtr ToolsMenu( AudacityProject & )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;

   return Menu( _("T&ools"),

#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageTools"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageTools), AudioIONotBusyFlag ),

      //Separator(),

#endif

      Command( wxT("ManageMacros"), XXO("&Macros..."),
         FN(OnManageMacros), AudioIONotBusyFlag ),

      Menu( _("&Apply Macro"),
         // Palette has no access key to ensure first letter navigation of sub menu
         Command( wxT("ApplyMacrosPalette"), XXO("Palette..."),
            FN(OnApplyMacrosPalette), AudioIONotBusyFlag ),

         Separator(),

         Items( PopulateMacrosMenu( AudioIONotBusyFlag ) )
      ),

      Separator(),

      Command( wxT("FancyScreenshot"), XXO("&Screenshot..."),
         FN(OnScreenshot), AudioIONotBusyFlag ),

// PRL: team consensus for 2.2.0 was, we let end users have this diagnostic,
// as they used to in 1.3.x
//#ifdef IS_ALPHA
      // TODO: What should we do here?  Make benchmark a plug-in?
      // Easy enough to do.  We'd call it mod-self-test.
      Command( wxT("Benchmark"), XXO("&Run Benchmark..."),
         FN(OnBenchmark), AudioIONotBusyFlag ),
//#endif

      Separator(),

      Items( PopulateEffectsMenu(
         EffectTypeTool,
         AudioIONotBusyFlag,
         AudioIONotBusyFlag ) )

#ifdef IS_ALPHA
      ,

      Separator(),

      Command( wxT("SimulateRecordingErrors"),
         XXO("Simulate Recording Errors"),
         FN(OnSimulateRecordingErrors),
         AudioIONotBusyFlag,
         Options{}.CheckState( gAudioIO->mSimulateRecordingErrors ) ),
      Command( wxT("DetectUpstreamDropouts"),
         XXO("Detect Upstream Dropouts"),
         FN(OnDetectUpstreamDropouts),
         AudioIONotBusyFlag,
         Options{}.CheckState( gAudioIO->mDetectUpstreamDropouts ) )
#endif
   );
}

MenuTable::BaseItemPtr WindowMenu( AudacityProject & )
{
#ifdef __WXMAC__
      /////////////////////////////////////////////////////////////////////////////
      // poor imitation of the Mac Windows Menu
      /////////////////////////////////////////////////////////////////////////////
   using namespace MenuTable;
   return Menu( _("&Window"),
      /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
       * window) shrink to an icon on the dock */
      Command( wxT("MacMinimize"), XXO("&Minimize"), FN(OnMacMinimize),
         NotMinimizedFlag, wxT("Ctrl+M") ),
      /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
       * window) full sized */
      Command( wxT("MacZoom"), XXO("&Zoom"),
         FN(OnMacZoom), NotMinimizedFlag ),

      Separator(),

      /* i18n-hint: Standard Macintosh Window menu item:  Make all project
       * windows un-hidden */
      Command( wxT("MacBringAllToFront"), XXO("&Bring All to Front"),
         FN(OnMacBringAllToFront), AlwaysEnabledFlag )
   );
#else
   return {};
#endif
}

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

MenuTable::BaseItemPtr ExtraGlobalCommands( AudacityProject & )
{
   // Ceci n'est pas un menu
   using namespace MenuTable;
   using Options = CommandManager::Options;
   return Items(
      Command( wxT("PrevWindow"), XXO("Move Backward Through Active Windows"),
         FN(OnPrevWindow), AlwaysEnabledFlag,
         Options{ wxT("Alt+Shift+F6") }.IsGlobal() ),
      Command( wxT("NextWindow"), XXO("Move Forward Through Active Windows"),
         FN(OnNextWindow), AlwaysEnabledFlag,
         Options{ wxT("Alt+F6") }.IsGlobal() )
   );
}

MenuTable::BaseItemPtr ExtraFocusMenu( AudacityProject & )
{
   using namespace MenuTable;
   constexpr auto FocusedTracksFlags = TracksExistFlag | TrackPanelHasFocus;

   return Menu( _("F&ocus"),
      Command( wxT("PrevFrame"),
         XXO("Move &Backward from Toolbars to Tracks"), FN(OnPrevFrame),
         AlwaysEnabledFlag, wxT("Ctrl+Shift+F6") ),
      Command( wxT("NextFrame"),
         XXO("Move F&orward from Toolbars to Tracks"), FN(OnNextFrame),
         AlwaysEnabledFlag, wxT("Ctrl+F6") ),
      Command( wxT("PrevTrack"), XXO("Move Focus to &Previous Track"),
         FN(OnCursorUp), FocusedTracksFlags, wxT("Up") ),
      Command( wxT("NextTrack"), XXO("Move Focus to &Next Track"),
         FN(OnCursorDown), FocusedTracksFlags, wxT("Down") ),
      Command( wxT("FirstTrack"), XXO("Move Focus to &First Track"),
         FN(OnFirstTrack), FocusedTracksFlags, wxT("Ctrl+Home") ),
      Command( wxT("LastTrack"), XXO("Move Focus to &Last Track"),
         FN(OnLastTrack), FocusedTracksFlags, wxT("Ctrl+End") ),
      Command( wxT("ShiftUp"), XXO("Move Focus to P&revious and Select"),
         FN(OnShiftUp), FocusedTracksFlags, wxT("Shift+Up") ),
      Command( wxT("ShiftDown"), XXO("Move Focus to N&ext and Select"),
         FN(OnShiftDown), FocusedTracksFlags, wxT("Shift+Down") ),
      Command( wxT("Toggle"), XXO("&Toggle Focused Track"), FN(OnToggle),
         FocusedTracksFlags, wxT("Return") ),
      Command( wxT("ToggleAlt"), XXO("Toggle Focuse&d Track"), FN(OnToggle),
         FocusedTracksFlags, wxT("NUMPAD_ENTER") )
   );
}

MenuTable::BaseItemPtr ExtraTrackMenu( AudacityProject & )
{
   using namespace MenuTable;

   return Menu( _("&Track"),
      Command( wxT("TrackPan"), XXO("Change P&an on Focused Track..."),
         FN(OnTrackPan),
         TrackPanelHasFocus | TracksExistFlag, wxT("Shift+P") ),
      Command( wxT("TrackPanLeft"), XXO("Pan &Left on Focused Track"),
         FN(OnTrackPanLeft),
         TrackPanelHasFocus | TracksExistFlag, wxT("Alt+Shift+Left") ),
      Command( wxT("TrackPanRight"), XXO("Pan &Right on Focused Track"),
         FN(OnTrackPanRight),
         TrackPanelHasFocus | TracksExistFlag, wxT("Alt+Shift+Right") ),
      Command( wxT("TrackGain"), XXO("Change Gai&n on Focused Track..."),
         FN(OnTrackGain),
         TrackPanelHasFocus | TracksExistFlag, wxT("Shift+G") ),
      Command( wxT("TrackGainInc"), XXO("&Increase Gain on Focused Track"),
         FN(OnTrackGainInc),
         TrackPanelHasFocus | TracksExistFlag, wxT("Alt+Shift+Up") ),
      Command( wxT("TrackGainDec"), XXO("&Decrease Gain on Focused Track"),
         FN(OnTrackGainDec),
         TrackPanelHasFocus | TracksExistFlag, wxT("Alt+Shift+Down") ),
      Command( wxT("TrackMenu"), XXO("Op&en Menu on Focused Track..."),
         FN(OnTrackMenu),
         TracksExistFlag | TrackPanelHasFocus, wxT("Shift+M\tskipKeydown") ),
      Command( wxT("TrackMute"), XXO("M&ute/Unmute Focused Track"),
         FN(OnTrackMute),
         TracksExistFlag | TrackPanelHasFocus, wxT("Shift+U") ),
      Command( wxT("TrackSolo"), XXO("&Solo/Unsolo Focused Track"),
         FN(OnTrackSolo),
         TracksExistFlag | TrackPanelHasFocus, wxT("Shift+S") ),
      Command( wxT("TrackClose"), XXO("&Close Focused Track"),
         FN(OnTrackClose),
         AudioIONotBusyFlag | TrackPanelHasFocus | TracksExistFlag,
         wxT("Shift+C") ),
      Command( wxT("TrackMoveUp"), XXO("Move Focused Track U&p"),
         FN(OnTrackMoveUp),
         AudioIONotBusyFlag | TrackPanelHasFocus | TracksExistFlag ),
      Command( wxT("TrackMoveDown"), XXO("Move Focused Track Do&wn"),
         FN(OnTrackMoveDown),
         AudioIONotBusyFlag | TrackPanelHasFocus | TracksExistFlag ),
      Command( wxT("TrackMoveTop"), XXO("Move Focused Track to T&op"),
         FN(OnTrackMoveTop),
         AudioIONotBusyFlag | TrackPanelHasFocus | TracksExistFlag ),
      Command( wxT("TrackMoveBottom"), XXO("Move Focused Track to &Bottom"),
         FN(OnTrackMoveBottom),
         AudioIONotBusyFlag | TrackPanelHasFocus | TracksExistFlag )
   );
}

MenuTable::BaseItemPtr ExtraScriptablesIMenu( AudacityProject & )
{
   using namespace MenuTable;

   // These are the more useful to VI user Scriptables.
   // i18n-hint: Scriptables are commands normally used from Python, Perl etc.
   return Menu( _("Script&ables I"),
      // Note that the PLUGIN_SYMBOL must have a space between words, 
      // whereas the short-form used here must not.
      // (So if you did write "CompareAudio" for the PLUGIN_SYMBOL name, then
      // you would have to use "Compareaudio" here.)
      Command( wxT("SelectTime"), XXO("Select Time..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SelectFrequencies"), XXO("Select Frequencies..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SelectTracks"), XXO("Select Tracks..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrackStatus"), XXO("Set Track Status..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrackAudio"), XXO("Set Track Audio..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrackVisuals"), XXO("Set Track Visuals..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("GetPreference"), XXO("Get Preference..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetPreference"), XXO("Set Preference..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetClip"), XXO("Set Clip..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetEnvelope"), XXO("Set Envelope..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetLabel"), XXO("Set Label..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetProject"), XXO("Set Project..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag )
   );
}

MenuTable::BaseItemPtr ExtraScriptablesIIMenu( AudacityProject & )
{
   using namespace MenuTable;

   // Less useful to VI users.
   return Menu( _("Scripta&bles II"),
      Command( wxT("Select"), XXO("Select..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrack"), XXO("Set Track..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("GetInfo"), XXO("Get Info..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Message"), XXO("Message..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Help"), XXO("Help..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Import2"), XXO("Import..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Export2"), XXO("Export..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("OpenProject2"), XXO("Open Project..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SaveProject2"), XXO("Save Project..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Drag"), XXO("Move Mouse..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("CompareAudio"), XXO("Compare Audio..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      // i18n-hint: Screenshot in the help menu has a much bigger dialog.
      Command( wxT("Screenshot"), XXO("Screenshot (short format)..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag )
   );
}

MenuTable::BaseItemPtr ExtraWindowItems( AudacityProject & )
{
#ifdef __WXMAC__
   using namespace MenuTable;

   return Items(
      /* i18n-hint: Shrink all project windows to icons on the Macintosh
         tooldock */
      Command( wxT("MacMinimizeAll"), XXO("Minimize All Projects"),
         FN(OnMacMinimizeAll),
         AlwaysEnabledFlag, wxT("Ctrl+Alt+M") )
   );
#else
   return nullptr;
#endif
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

MenuTable::BaseItemPtr HelpMenu( AudacityProject & )
{
#ifdef __WXMAC__
      wxGetApp().s_macHelpMenuTitleName = _("&Help");
#endif

   using namespace MenuTable;

   return Menu( _("&Help"),
      Command( wxT("QuickFix"), XXO("&Quick Fix..."), FN(OnQuickFix),
         AlwaysEnabledFlag ),
      // DA: Emphasise it is the Audacity Manual (No separate DA manual).
#ifdef EXPERIMENTAL_DA
      // 'Getting Started' rather than 'Quick Help' for DarkAudacity.
      // At the moment the video tutorials are aspirational (aka do not exist yet).
      // Emphasise that manual is for Audacity, not DarkAudacity.
      Command( wxT("QuickHelp"), XXO("&Getting Started"), FN(OnQuickHelp) ),
      Command( wxT("Manual"), XXO("Audacity &Manual"), FN(OnManual) ),
#else
      Command( wxT("QuickHelp"), XXO("&Quick Help..."), FN(OnQuickHelp),
         AlwaysEnabledFlag ),
      Command( wxT("Manual"), XXO("&Manual..."), FN(OnManual),
         AlwaysEnabledFlag ),
#endif

      Separator(),

      Menu( _("&Diagnostics"),
         Command( wxT("DeviceInfo"), XXO("Au&dio Device Info..."),
            FN(OnAudioDeviceInfo),
            AudioIONotBusyFlag ),
   #ifdef EXPERIMENTAL_MIDI_OUT
         Command( wxT("MidiDeviceInfo"), XXO("&MIDI Device Info..."),
            FN(OnMidiDeviceInfo),
            AudioIONotBusyFlag ),
   #endif
         Command( wxT("Log"), XXO("Show &Log..."), FN(OnShowLog),
            AlwaysEnabledFlag ),
   #if defined(EXPERIMENTAL_CRASH_REPORT)
         Command( wxT("CrashReport"), XXO("&Generate Support Data..."),
            FN(OnCrashReport), AlwaysEnabledFlag ),
   #endif
         Command( wxT("CheckDeps"), XXO("Chec&k Dependencies..."),
            FN(OnCheckDependencies),
            AudioIONotBusyFlag )
      ),

#ifndef __WXMAC__
      Separator(),
#endif

      // DA: Does not fully support update checking.
#ifndef EXPERIMENTAL_DA
      Command( wxT("Updates"), XXO("&Check for Updates..."),
         FN(OnCheckForUpdates),
         AlwaysEnabledFlag ),
#endif
      Command( wxT("About"), XXO("&About Audacity..."), FN(OnAbout),
         AlwaysEnabledFlag )
   );
}

}

void MenuCreator::CreateMenusAndCommands(AudacityProject &project)
{
   CommandManager *c = project.GetCommandManager();

   // The list of defaults to exclude depends on
   // preference wxT("/GUI/Shortcuts/FullDefaults"), which may have changed.
   c->SetMaxList();

   auto menubar = c->AddMenuBar(wxT("appmenu"));
   wxASSERT(menubar);

   VisitItem( project, menuTree.get() );

   project.SetMenuBar(menubar.release());

   mLastFlags = AlwaysEnabledFlag;

#if defined(__WXDEBUG__)
//   c->CheckDups();
#endif
}

#undef XXO



MenuTable::BaseItemPtrs PopulateMacrosMenu( CommandFlag flags  )
{
   MenuTable::BaseItemPtrs result;
   wxArrayString names = MacroCommands::GetNames();
   int i;

   for (i = 0; i < (int)names.GetCount(); i++) {
      wxString MacroID = ApplyMacroDialog::MacroIdOfName( names[i] );
      result.push_back( MenuTable::Command( MacroID,
         names[i], false, FN(OnApplyMacroDirectly),
         flags ) );
   }

   return result;
}


/// The effects come from a plug in list
/// This code iterates through the list, adding effects into
/// the menu.
MenuTable::BaseItemPtrs PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   CommandFlag realflags)
{
   MenuTable::BaseItemPtrs result;
   PluginManager & pm = PluginManager::Get();

   std::vector<const PluginDescriptor*> defplugs;
   std::vector<const PluginDescriptor*> optplugs;

   const PluginDescriptor *plug = pm.GetFirstPluginForEffectType(type);
   while (plug)
   {
      if ( !plug->IsEnabled() ){
         ;// don't add to menus!
      }
      else if (plug->IsEffectDefault()
#ifdef EXPERIMENTAL_DA
         // Move Nyquist prompt into nyquist group.
         && (plug->GetSymbol() != IdentInterfaceSymbol("Nyquist Effects Prompt"))
         && (plug->GetSymbol() != IdentInterfaceSymbol("Nyquist Tools Prompt"))
         && (plug->GetSymbol() != IdentInterfaceSymbol("Nyquist Prompt"))
#endif
         )
         defplugs.push_back(plug);
      else
         optplugs.push_back(plug);
      plug = pm.GetNextPluginForEffectType(type);
   }

   wxString groupby = gPrefs->Read(wxT("/Effects/GroupBy"), wxT("name"));

   using Comparator = bool(*)(const PluginDescriptor*, const PluginDescriptor*);
   Comparator comp1, comp2;
   if (groupby == wxT("sortby:name"))
      comp1 = comp2 = SortEffectsByName;
   else if (groupby == wxT("sortby:publisher:name"))
      comp1 = SortEffectsByName, comp2 = SortEffectsByPublisherAndName;
   else if (groupby == wxT("sortby:type:name"))
      comp1 = SortEffectsByName, comp2 = SortEffectsByTypeAndName;
   else if (groupby == wxT("groupby:publisher"))
      comp1 = comp2 = SortEffectsByPublisher;
   else if (groupby == wxT("groupby:type"))
      comp1 = comp2 = SortEffectsByType;
   else // name
      comp1 = comp2 = SortEffectsByName;

   std::sort( defplugs.begin(), defplugs.end(), comp1 );
   if ( comp1 != comp2 )
      std::stable_sort( optplugs.begin(), optplugs.end(), comp2 );

   AddEffectMenuItems( result, defplugs, batchflags, realflags, true );

   if (defplugs.size() && optplugs.size())
      result.push_back( MenuTable::Separator() );

   AddEffectMenuItems( result, optplugs, batchflags, realflags, false );

   return result;
}

void AddEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   bool isDefault)
{
   size_t pluginCnt = plugs.size();

   wxString groupBy = gPrefs->Read(wxT("/Effects/GroupBy"), wxT("name"));

   bool grouped = false;
   if (groupBy.StartsWith(wxT("groupby")))
   {
      grouped = true;
   }

   std::vector<bool> vHasDialog;
   wxArrayString groupNames;
   PluginIDList groupPlugs;
   std::vector<CommandFlag> groupFlags;
   if (grouped)
   {
      wxString last;
      wxString current;

      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         bool hasDialog = plug->GetSymbol().Msgid().Contains("...");
         auto name = plug->GetSymbol().Translation();

         if (plug->IsEffectInteractive())
         {
            name += wxT("...");
         }

         if (groupBy == wxT("groupby:publisher"))
         {
            current = EffectManager::Get().GetVendorName(plug->GetID());
            if (current.IsEmpty())
            {
               current = _("Unknown");
            }
         }
         else if (groupBy == wxT("groupby:type"))
         {
            current = EffectManager::Get().GetEffectFamilyName(plug->GetID());
            if (current.IsEmpty())
            {
               current = _("Unknown");
            }
         }

         if (current != last)
         {
            using namespace MenuTable;
            BaseItemPtrs temp;
            bool bInSubmenu = !last.IsEmpty() && (groupNames.Count() > 1);

            AddEffectMenuItemGroup(temp,
               groupNames, vHasDialog,
               groupPlugs, groupFlags, isDefault);

            table.push_back( MenuOrItems(
               ( bInSubmenu ? last : wxString{} ), std::move( temp )
            ) );

            groupNames.Clear();
            vHasDialog.clear();
            groupPlugs.Clear();
            groupFlags.clear();
            last = current;
         }

         groupNames.Add(name);
         vHasDialog.push_back(hasDialog);
         groupPlugs.Add(plug->GetID());
         groupFlags.push_back(plug->IsEffectRealtime() ? realflags : batchflags);
      }

      if (groupNames.GetCount() > 0)
      {
         using namespace MenuTable;
         BaseItemPtrs temp;
         bool bInSubmenu = groupNames.Count() > 1;

         AddEffectMenuItemGroup(temp,
            groupNames, vHasDialog, groupPlugs, groupFlags, isDefault);

         table.push_back( MenuOrItems(
            ( bInSubmenu ? current : wxString{} ), std::move( temp )
         ) );
      }
   }
   else
   {
      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         bool hasDialog = plug->GetSymbol().Msgid().Contains("...");
         auto name = plug->GetSymbol().Translation();

         if (plug->IsEffectInteractive())
         {
            name += wxT("...");
         }

         wxString group = wxEmptyString;
         if (groupBy == wxT("sortby:publisher:name"))
         {
            group = EffectManager::Get().GetVendorName(plug->GetID());
         }
         else if (groupBy == wxT("sortby:type:name"))
         {
            group = EffectManager::Get().GetEffectFamilyName(plug->GetID());
         }

         if (plug->IsEffectDefault())
         {
            group = wxEmptyString;
         }

         if (!group.IsEmpty())
         {
            group += wxT(": ");
         }

         groupNames.Add(group + name);
         vHasDialog.push_back(hasDialog);
         groupPlugs.Add(plug->GetID());
         groupFlags.push_back(plug->IsEffectRealtime() ? realflags : batchflags);
      }

      if (groupNames.GetCount() > 0)
      {
         AddEffectMenuItemGroup(table, groupNames, vHasDialog, groupPlugs, groupFlags, isDefault);
      }

   }

   return;
}

void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const wxArrayString & names,
   const std::vector<bool> &vHasDialog,
   const PluginIDList & plugs,
   const std::vector<CommandFlag> & flags,
   bool isDefault)
{
   const int namesCnt = (int) names.GetCount();
   int perGroup;

#if defined(__WXGTK__)
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 15);
#else
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 0);
#endif

   int groupCnt = namesCnt;
   for (int i = 0; i < namesCnt; i++)
   {
      while (i + 1 < namesCnt && names[i].IsSameAs(names[i + 1]))
      {
         i++;
         groupCnt--;
      }
   }

   // The "default" effects shouldn't be broken into subgroups
   if (namesCnt > 0 && isDefault)
   {
      perGroup = 0;
   }

   int max = perGroup;
   int items = perGroup;

   if (max > groupCnt)
   {
      max = 0;
   }

   using namespace MenuTable;
   auto pTable = &table;
   BaseItemPtrs temp1;

   int groupNdx = 0;
   for (int i = 0; i < namesCnt; i++)
   {
      if (max > 0 && items == max)
      {
         // start collecting items for the next submenu
         pTable = &temp1;
      }

      if (i + 1 < namesCnt && names[i].IsSameAs(names[i + 1]))
      {
         // collect a sub-menu for like-named items
         const wxString name = names[i];
         BaseItemPtrs temp2;
         while (i < namesCnt && names[i].IsSameAs(name))
         {
            const PluginDescriptor *plug = PluginManager::Get().GetPlugin(plugs[i]);
            wxString item = plug->GetPath();
            if( plug->GetPluginType() == PluginTypeEffect )
               temp2.push_back( Command( item,
                  item,
                  item.Contains("..."),
                  FN(OnEffect),
                  flags[i],
                  CommandManager::Options{}
                     .IsEffect().Parameter( plugs[i] ) ) );

            i++;
         }
         pTable->push_back( Menu( name, std::move( temp2 ) ) );
         i--;
      }
      else
      {
         // collect one item
         const PluginDescriptor *plug = PluginManager::Get().GetPlugin(plugs[i]);
         if( plug->GetPluginType() == PluginTypeEffect )
            pTable->push_back( Command( names[i],
               names[i],
               vHasDialog[i],
               FN(OnEffect),
               flags[i],
               CommandManager::Options{}
                  .IsEffect().Parameter( plugs[i] ) ) );
      }

      if (max > 0)
      {
         items--;
         if (items == 0 || i + 1 == namesCnt)
         {
            int end = groupNdx + max;
            if (end + 1 > groupCnt)
            {
               end = groupCnt;
            }
            // Done collecting
            table.push_back( Menu(
               wxString::Format(_("Plug-in %d to %d"), groupNdx + 1, end),
               std::move( temp1 )
            ) );
            items = max;
            pTable = &table;
         }
         groupNdx++;
      }
   }

   return;
}

#undef FN

// TODO: This surely belongs in CommandManager?
void MenuManager::ModifyUndoMenuItems(AudacityProject &project)
{
   wxString desc;
   auto &undoManager = *project.GetUndoManager();
   auto &commandManager = *project.GetCommandManager();
   int cur = undoManager.GetCurrentState();

   if (undoManager.UndoAvailable()) {
      undoManager.GetShortDescription(cur, &desc);
      commandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo %s"),
                                              desc));
      commandManager.Enable(wxT("Undo"), project.UndoAvailable());
   }
   else {
      commandManager.Modify(wxT("Undo"),
                            _("&Undo"));
   }

   if (undoManager.RedoAvailable()) {
      undoManager.GetShortDescription(cur+1, &desc);
      commandManager.Modify(wxT("Redo"),
                             wxString::Format(_("&Redo %s"),
                                              desc));
      commandManager.Enable(wxT("Redo"), project.RedoAvailable());
   }
   else {
      commandManager.Modify(wxT("Redo"),
                            _("&Redo"));
      commandManager.Enable(wxT("Redo"), false);
   }
}

void MenuCreator::RebuildMenuBar(AudacityProject &project)
{
   // On OSX, we can't rebuild the menus while a modal dialog is being shown
   // since the enabled state for menus like Quit and Preference gets out of
   // sync with wxWidgets idea of what it should be.
#if defined(__WXMAC__) && defined(__WXDEBUG__)
   {
      wxDialog *dlg =
         wxDynamicCast(wxGetTopLevelParent(wxWindow::FindFocus()), wxDialog);
      wxASSERT((!dlg || !dlg->IsModal()));
   }
#endif

   // Delete the menus, since we will soon recreate them.
   // Rather oddly, the menus don't vanish as a result of doing this.
   {
      std::unique_ptr<wxMenuBar> menuBar{ project.GetMenuBar() };
      project.DetachMenuBar();
      // menuBar gets deleted here
   }

   project.GetCommandManager()->PurgeData();

   CreateMenusAndCommands(project);

   ModuleManager::Get().Dispatch(MenusRebuilt);
}

void AudacityProject::RebuildOtherMenus()
{
}

CommandFlag MenuManager::GetFocusedFrame(AudacityProject &project)
{
   wxWindow *w = wxWindow::FindFocus();

   while (w && project.GetToolManager() && project.GetTrackPanel()) {
      if (w == project.GetToolManager()->GetTopDock()) {
         return TopDockHasFocus;
      }

      if (w == project.GetRulerPanel())
         return RulerHasFocus;

      if (w == project.GetTrackPanel()) {
         return TrackPanelHasFocus;
      }
      // LIE if Lyrics window has focus.
      // we want to act as if TrackPanel has focus.
      if (w == project.GetLyricsWindow()) {
         return TrackPanelHasFocus;
      }
      if (w == project.GetToolManager()->GetBotDock()) {
         return BotDockHasFocus;
      }

      w = w->GetParent();
   }

   return AlwaysEnabledFlag;
}

CommandFlag MenuManager::GetUpdateFlags
(AudacityProject &project, bool checkActive)
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.
   auto flags = AlwaysEnabledFlag;
   // static variable, used to remember flags for next time.
   static auto lastFlags = flags;

   if (auto focus = wxWindow::FindFocus()) {
      while (focus && focus->GetParent())
         focus = focus->GetParent();
      if (focus && !static_cast<wxTopLevelWindow*>(focus)->IsIconized())
         flags |= NotMinimizedFlag;
   }

   // quick 'short-circuit' return.
   if ( checkActive && !project.IsActive() ){
      // short cirucit return should preserve flags that have not been calculated.
      flags = (lastFlags & ~NotMinimizedFlag) | flags;
      lastFlags = flags;
      return flags;
   }

   if (!gAudioIO->IsAudioTokenActive(project.GetAudioIOToken()))
      flags |= AudioIONotBusyFlag;
   else
      flags |= AudioIOBusyFlag;

   if( gAudioIO->IsPaused() )
      flags |= PausedFlag;
   else
      flags |= NotPausedFlag;

   auto &viewInfo = project.GetViewInfo();
   const auto &selectedRegion = viewInfo.selectedRegion;

   if (!selectedRegion.isPoint())
      flags |= TimeSelectedFlag;

   auto tracks = project.GetTracks();
   auto trackRange = tracks->Any();
   if ( trackRange )
      flags |= TracksExistFlag;
   trackRange.Visit(
      [&](LabelTrack *lt) {
         flags |= LabelTracksExistFlag;

         if (lt->GetSelected()) {
            flags |= TracksSelectedFlag;
            for (int i = 0; i < lt->GetNumLabels(); i++) {
               const LabelStruct *ls = lt->GetLabel(i);
               if (ls->getT0() >= selectedRegion.t0() &&
                   ls->getT1() <= selectedRegion.t1()) {
                  flags |= LabelsSelectedFlag;
                  break;
               }
            }
         }

         if (lt->IsTextSelected()) {
            flags |= CutCopyAvailableFlag;
         }
      },
      [&](WaveTrack *t) {
         flags |= WaveTracksExistFlag;
         flags |= PlayableTracksExistFlag;
         if (t->GetSelected()) {
            flags |= TracksSelectedFlag;
            // TODO: more-than-two-channels
            if (TrackList::Channels(t).size() > 1) {
               flags |= StereoRequiredFlag;
            }
            flags |= WaveTracksSelectedFlag;
            flags |= AudioTracksSelectedFlag;
         }
         if( t->GetEndTime() > t->GetStartTime() )
            flags |= HasWaveDataFlag;
      }
#if defined(USE_MIDI)
      ,
      [&](NoteTrack *nt) {
         flags |= NoteTracksExistFlag;
#ifdef EXPERIMENTAL_MIDI_OUT
         flags |= PlayableTracksExistFlag;
#endif

         if (nt->GetSelected()) {
            flags |= TracksSelectedFlag;
            flags |= NoteTracksSelectedFlag;
            flags |= AudioTracksSelectedFlag; // even if not EXPERIMENTAL_MIDI_OUT
         }
      }
#endif
   );

   if((AudacityProject::msClipT1 - AudacityProject::msClipT0) > 0.0)
      flags |= ClipboardFlag;

   auto &undoManager = *project.GetUndoManager();

   if (undoManager.UnsavedChanges() || !project.IsProjectSaved())
      flags |= UnsavedChangesFlag;

   if (!mLastEffect.IsEmpty())
      flags |= HasLastEffectFlag;

   if (project.UndoAvailable())
      flags |= UndoAvailableFlag;

   if (project.RedoAvailable())
      flags |= RedoAvailableFlag;

   if (project.ZoomInAvailable() && (flags & TracksExistFlag))
      flags |= ZoomInAvailableFlag;

   if (project.ZoomOutAvailable() && (flags & TracksExistFlag))
      flags |= ZoomOutAvailableFlag;

   // TextClipFlag is currently unused (Jan 2017, 2.1.3 alpha)
   // and LabelTrack::IsTextClipSupported() is quite slow on Linux,
   // so disable for now (See bug 1575).
   // if ((flags & LabelTracksExistFlag) && LabelTrack::IsTextClipSupported())
   //    flags |= TextClipFlag;

   flags |= GetFocusedFrame(project);

   double start, end;
   project.GetPlayRegion(&start, &end);
   if (project.IsPlayRegionLocked())
      flags |= PlayRegionLockedFlag;
   else if (start != end)
      flags |= PlayRegionNotLockedFlag;

   if (flags & AudioIONotBusyFlag) {
      if (flags & TimeSelectedFlag) {
         if (flags & TracksSelectedFlag) {
            flags |= CutCopyAvailableFlag;
         }
      }
   }

   if (wxGetApp().GetRecentFiles()->GetCount() > 0)
      flags |= HaveRecentFiles;

   if (project.IsSyncLocked())
      flags |= IsSyncLockedFlag;
   else
      flags |= IsNotSyncLockedFlag;

   if (!EffectManager::Get().RealtimeIsActive())
      flags |= IsRealtimeNotActiveFlag;

      if (!project.IsCapturing())
      flags |= CaptureNotBusyFlag;

   ControlToolBar *bar = project.GetControlToolBar();
   if (bar->ControlToolBar::CanStopAudioStream())
      flags |= CanStopAudioStreamFlag;

   lastFlags = flags;
   return flags;
}

// Select the full time range, if no
// time range is selected.
void AudacityProject::SelectAllIfNone()
{
   auto flags = GetMenuManager(*this).GetUpdateFlags(*this);
   if(!(flags & TracksSelectedFlag) ||
      (mViewInfo.selectedRegion.isPoint()))
      SelectActions::DoSelectSomething(*this);
}

// Stop playing or recording, if paused.
void AudacityProject::StopIfPaused()
{
   auto flags = GetMenuManager(*this).GetUpdateFlags(*this);
   if( flags & PausedFlag )
      TransportActions::DoStop(*this);
}

void MenuManager::ModifyAllProjectToolbarMenus()
{
   AProjectArray::iterator i;
   for (i = gAudacityProjects.begin(); i != gAudacityProjects.end(); ++i) {
      auto &project = **i;
      GetMenuManager(project).ModifyToolbarMenus(project);
   }
}

void MenuManager::ModifyToolbarMenus(AudacityProject &project)
{
   // Refreshes can occur during shutdown and the toolmanager may already
   // be deleted, so protect against it.
   auto toolManager = project.GetToolManager();
   if (!toolManager) {
      return;
   }

   auto &commandManager = *project.GetCommandManager();

   commandManager.Check(wxT("ShowScrubbingTB"),
                         toolManager->IsVisible(ScrubbingBarID));
   commandManager.Check(wxT("ShowDeviceTB"),
                         toolManager->IsVisible(DeviceBarID));
   commandManager.Check(wxT("ShowEditTB"),
                         toolManager->IsVisible(EditBarID));
   commandManager.Check(wxT("ShowMeterTB"),
                         toolManager->IsVisible(MeterBarID));
   commandManager.Check(wxT("ShowRecordMeterTB"),
                         toolManager->IsVisible(RecordMeterBarID));
   commandManager.Check(wxT("ShowPlayMeterTB"),
                         toolManager->IsVisible(PlayMeterBarID));
   commandManager.Check(wxT("ShowMixerTB"),
                         toolManager->IsVisible(MixerBarID));
   commandManager.Check(wxT("ShowSelectionTB"),
                         toolManager->IsVisible(SelectionBarID));
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   commandManager.Check(wxT("ShowSpectralSelectionTB"),
                         toolManager->IsVisible(SpectralSelectionBarID));
#endif
   commandManager.Check(wxT("ShowToolsTB"),
                         toolManager->IsVisible(ToolsBarID));
   commandManager.Check(wxT("ShowTranscriptionTB"),
                         toolManager->IsVisible(TranscriptionBarID));
   commandManager.Check(wxT("ShowTransportTB"),
                         toolManager->IsVisible(TransportBarID));

   // Now, go through each toolbar, and call EnableDisableButtons()
   for (int i = 0; i < ToolBarCount; i++) {
      toolManager->GetToolBar(i)->EnableDisableButtons();
   }

   // These don't really belong here, but it's easier and especially so for
   // the Edit toolbar and the sync-lock menu item.
   bool active;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"),&active, false);
   commandManager.Check(wxT("SoundActivation"), active);
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"),&active, false);
   commandManager.Check(wxT("AutomatedInputLevelAdjustmentOnOff"), active);
#endif

   active = TracksPrefs::GetPinnedHeadPreference();
   commandManager.Check(wxT("PinnedHead"), active);

#ifdef EXPERIMENTAL_DA
   gPrefs->Read(wxT("/AudioIO/Duplex"),&active, false);
#else
   gPrefs->Read(wxT("/AudioIO/Duplex"),&active, true);
#endif
   commandManager.Check(wxT("Overdub"), active);
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"),&active, false);
   commandManager.Check(wxT("SWPlaythrough"), active);
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &active, false);
   project.SetSyncLock(active);
   commandManager.Check(wxT("SyncLock"), active);
   gPrefs->Read(wxT("/GUI/TypeToCreateLabel"),&active, true);
   commandManager.Check(wxT("TypeToCreateLabel"), active);
}

// checkActive is a temporary hack that should be removed as soon as we
// get multiple effect preview working
void MenuManager::UpdateMenus(AudacityProject &project, bool checkActive)
{
   //ANSWER-ME: Why UpdateMenus only does active project?
   //JKC: Is this test fixing a bug when multiple projects are open?
   //so that menu states work even when different in different projects?
   if (&project != GetActiveProject())
      return;

   auto flags = GetMenuManager(project).GetUpdateFlags(project, checkActive);
   auto flags2 = flags;

   // We can enable some extra items if we have select-all-on-none.
   //EXPLAIN-ME: Why is this here rather than in GetUpdateFlags()?
   //ANSWER: Because flags2 is used in the menu enable/disable.
   //The effect still needs flags to determine whether it will need
   //to actually do the 'select all' to make the command valid.
   if (mWhatIfNoSelection != 0)
   {
      if ((flags & TracksExistFlag))
      {
         flags2 |= TracksSelectedFlag;
         if ((flags & WaveTracksExistFlag))
         {
            flags2 |= TimeSelectedFlag
                   |  WaveTracksSelectedFlag
                   |  CutCopyAvailableFlag;
         }
      }
   }

   if( mStopIfWasPaused )
   {
      if( flags & PausedFlag ){
         flags2 |= AudioIONotBusyFlag;
      }
   }

   // Return from this function if nothing's changed since
   // the last time we were here.
   if (flags == mLastFlags)
      return;
   mLastFlags = flags;

   auto &commandManager = *project.GetCommandManager();

   commandManager.EnableUsingFlags(flags2 , NoFlagsSpecified);

   // With select-all-on-none, some items that we don't want enabled may have
   // been enabled, since we changed the flags.  Here we manually disable them.
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   if (mWhatIfNoSelection != 0)
   {
      if (!(flags & TimeSelectedFlag) | !(flags & TracksSelectedFlag))
      {
         commandManager.Enable(wxT("SplitCut"), false);
         commandManager.Enable(wxT("SplitDelete"), false);
      }
      if (!(flags & WaveTracksSelectedFlag))
      {
         commandManager.Enable(wxT("Split"), false);
      }
      if (!(flags & TimeSelectedFlag) | !(flags & WaveTracksSelectedFlag))
      {
         commandManager.Enable(wxT("ExportSel"), false);
         commandManager.Enable(wxT("SplitNew"), false);
      }
      if (!(flags & TimeSelectedFlag) | !(flags & AudioTracksSelectedFlag))
      {
         commandManager.Enable(wxT("Trim"), false);
      }
   }

#if 0
   if (flags & CutCopyAvailableFlag) {
      GetCommandManager()->Enable(wxT("Copy"), true);
      GetCommandManager()->Enable(wxT("Cut"), true);
   }
#endif

   MenuManager::ModifyToolbarMenus(project);
}

//sort based on flags.  see Project.h for sort flags
void AudacityProject::SortTracks(int flags)
{
   auto GetTime = [](const Track *t) {
      return t->TypeSwitch< double >(
         [&](const WaveTrack* w) {
            auto stime = w->GetEndTime();

            int ndx;
            for (ndx = 0; ndx < w->GetNumClips(); ndx++) {
               const auto c = w->GetClipByIndex(ndx);
               if (c->GetNumSamples() == 0)
                  continue;
               stime = std::min(stime, c->GetStartTime());
            }
            return stime;
         },
         [&](const LabelTrack* l) {
            return l->GetStartTime();
         }
      );
   };

   size_t ndx = 0;
   // This one place outside of TrackList where we must use undisguised
   // std::list iterators!  Avoid this elsewhere!
   std::vector<TrackNodePointer> arr;
   arr.reserve(mTracks->size());

   // First find the permutation.
   // This routine, very unusually, deals with the underlying stl list
   // iterators, not with TrackIter!  Dangerous!
   for (auto iter = mTracks->ListOfTracks::begin(),
        end = mTracks->ListOfTracks::end(); iter != end; ++iter) {
      const auto &track = *iter;
      if ( !track->IsLeader() )
         // keep channels contiguous
         ndx++;
      else {
         auto size = arr.size();
         for (ndx = 0; ndx < size;) {
            Track &arrTrack = **arr[ndx].first;
            auto channels = TrackList::Channels(&arrTrack);
            if(flags & kAudacitySortByName) {
               //do case insensitive sort - cmpNoCase returns less than zero if the string is 'less than' its argument
               //also if we have case insensitive equality, then we need to sort by case as well
               //We sort 'b' before 'B' accordingly.  We uncharacteristically use greater than for the case sensitive
               //compare because 'b' is greater than 'B' in ascii.
               auto cmpValue = track->GetName().CmpNoCase(arrTrack.GetName());
               if ( cmpValue < 0 ||
                     ( 0 == cmpValue &&
                        track->GetName().CompareTo(arrTrack.GetName()) > 0 ) )
                  break;
            }
            //sort by time otherwise
            else if(flags & kAudacitySortByTime) {
               auto time1 = TrackList::Channels(track.get()).min( GetTime );

               //get candidate's (from sorted array) time
               auto time2 = channels.min( GetTime );

               if (time1 < time2)
                  break;
            }
            ndx += channels.size();
         }
      }
      arr.insert(arr.begin() + ndx, TrackNodePointer{iter, mTracks.get()});
   }

   // Now apply the permutation
   mTracks->Permute(arr);
}

void MenuCommandHandler::OnSortTime(const CommandContext &context)
{
   auto &project = context.project;
   project.SortTracks(kAudacitySortByTime);

   project.PushState(_("Tracks sorted by time"), _("Sort by Time"));

   auto trackPanel = project.GetTrackPanel();
   trackPanel->Refresh(false);
}

void MenuCommandHandler::OnSortName(const CommandContext &context)
{
   auto &project = context.project;
   project.SortTracks(kAudacitySortByName);

   project.PushState(_("Tracks sorted by name"), _("Sort by Name"));

   auto trackPanel = project.GetTrackPanel();
   trackPanel->Refresh(false);
}

/// The following method moves to the previous track
/// selecting and unselecting depending if you are on the start of a
/// block or not.

/// \todo Merge related methods, OnPrevTrack and OnNextTrack.
void MenuCommandHandler::DoPrevTrack( AudacityProject &project, bool shift )
{
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();
   auto &selectionState = project.GetSelectionState();
   auto mixerBoard = project.GetMixerBoard();

   Track* t = trackPanel->GetFocusedTrack();
   if( t == NULL )   // if there isn't one, focus on last
   {
      t = *tracks->Any().rbegin();
      trackPanel->SetFocusedTrack( t );
      trackPanel->EnsureVisible( t );
      project.ModifyState(false);
      return;
   }

   Track* p = NULL;
   bool tSelected = false;
   bool pSelected = false;
   if( shift )
   {
      p = * -- tracks->FindLeader( t ); // Get previous track
      if( p == NULL )   // On first track
      {
         // JKC: wxBell() is probably for accessibility, so a blind
         // user knows they were at the top track.
         wxBell();
         if( mCircularTrackNavigation )
            p = *tracks->Any().rbegin();
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      tSelected = t->GetSelected();
      if (p)
         pSelected = p->GetSelected();
      if( tSelected && pSelected )
      {
         selectionState.SelectTrack
            ( *t, false, false, mixerBoard );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
      if( tSelected && !pSelected )
      {
         selectionState.SelectTrack
            ( *p, true, false, mixerBoard );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && pSelected )
      {
         selectionState.SelectTrack
            ( *p, false, false, mixerBoard );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && !pSelected )
      {
         selectionState.SelectTrack
            ( *t, true, false, mixerBoard );
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
          project.ModifyState(false);
         return;
      }
   }
   else
   {
      p = * -- tracks->FindLeader( t ); // Get previous track
      if( p == NULL )   // On first track so stay there?
      {
         wxBell();
         if( mCircularTrackNavigation )
         {
            auto range = tracks->Leaders();
            p = * range.rbegin(); // null if range is empty
            trackPanel->SetFocusedTrack( p );   // Wrap to the last track
            trackPanel->EnsureVisible( p );
             project.ModifyState(false);
            return;
         }
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      else
      {
         trackPanel->SetFocusedTrack( p );   // move focus to next track up
         trackPanel->EnsureVisible( p );
         project.ModifyState(false);
         return;
      }
   }
}

/// The following method moves to the next track,
/// selecting and unselecting depending if you are on the start of a
/// block or not.
void MenuCommandHandler::DoNextTrack( AudacityProject &project, bool shift )
{
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();
   auto &selectionState = project.GetSelectionState();
   auto mixerBoard = project.GetMixerBoard();

   auto t = trackPanel->GetFocusedTrack();   // Get currently focused track
   if( t == NULL )   // if there isn't one, focus on first
   {
      t = *tracks->Any().begin();
      trackPanel->SetFocusedTrack( t );
      trackPanel->EnsureVisible( t );
      project.ModifyState(false);
      return;
   }

   if( shift )
   {
      auto n = * ++ tracks->FindLeader( t ); // Get next track
      if( n == NULL )   // On last track so stay there
      {
         wxBell();
         if( mCircularTrackNavigation )
            n = *tracks->Any().begin();
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      auto tSelected = t->GetSelected();
      auto nSelected = n->GetSelected();
      if( tSelected && nSelected )
      {
         selectionState.SelectTrack
            ( *t, false, false, mixerBoard );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
      if( tSelected && !nSelected )
      {
         selectionState.SelectTrack
            ( *n, true, false, mixerBoard );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && nSelected )
      {
         selectionState.SelectTrack
            ( *n, false, false, mixerBoard );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
      if( !tSelected && !nSelected )
      {
         selectionState.SelectTrack
            ( *t, true, false, mixerBoard );
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
   }
   else
   {
      auto n = * ++ tracks->FindLeader( t ); // Get next track
      if( n == NULL )   // On last track so stay there
      {
         wxBell();
         if( mCircularTrackNavigation )
         {
            n = *tracks->Any().begin();
            trackPanel->SetFocusedTrack( n );   // Wrap to the first track
            trackPanel->EnsureVisible( n );
            project.ModifyState(false);
            return;
         }
         else
         {
            trackPanel->EnsureVisible( t );
            return;
         }
      }
      else
      {
         trackPanel->SetFocusedTrack( n );   // move focus to next track down
         trackPanel->EnsureVisible( n );
         project.ModifyState(false);
         return;
      }
   }
}

void MenuCommandHandler::OnCursorUp(const CommandContext &context)
{
   auto &project = context.project;
   DoPrevTrack( project, false );
}

void MenuCommandHandler::OnCursorDown(const CommandContext &context)
{
   auto &project = context.project;
   DoNextTrack( project, false );
}

void MenuCommandHandler::OnFirstTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *t = trackPanel->GetFocusedTrack();
   if (!t)
      return;

   auto f = *tracks->Any().begin();
   if (t != f)
   {
      trackPanel->SetFocusedTrack(f);
      project.ModifyState(false);
   }
   trackPanel->EnsureVisible(f);
}

void MenuCommandHandler::OnLastTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *t = trackPanel->GetFocusedTrack();
   if (!t)
      return;

   auto l = *tracks->Any().rbegin();
   if (t != l)
   {
      trackPanel->SetFocusedTrack(l);
      project.ModifyState(false);
   }
   trackPanel->EnsureVisible(l);
}

void MenuCommandHandler::OnShiftUp(const CommandContext &context)
{
   auto &project = context.project;
   DoPrevTrack( project, true );
}

void MenuCommandHandler::OnShiftDown(const CommandContext &context)
{
   auto &project = context.project;
   DoNextTrack( project, true );
}

#include "TrackPanelAx.h"
void MenuCommandHandler::OnToggle(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto &selectionState = project.GetSelectionState();
   auto mixerBoard = project.GetMixerBoard();

   Track *t;

   t = trackPanel->GetFocusedTrack();   // Get currently focused track
   if (!t)
      return;

   selectionState.SelectTrack
      ( *t, !t->GetSelected(), true, mixerBoard );
   trackPanel->EnsureVisible( t );
   project.ModifyState(false);

   trackPanel->GetAx().Updated();

   return;
}

void MenuCommandHandler::NextOrPrevFrame(AudacityProject &project, bool forward)
{
   // Focus won't take in a dock unless at least one descendant window
   // accepts focus.  Tell controls to take focus for the duration of this
   // function, only.  Outside of this, they won't steal the focus when
   // clicked.
   auto temp1 = AButton::TemporarilyAllowFocus();
   auto temp2 = ASlider::TemporarilyAllowFocus();
   auto temp3 = MeterPanel::TemporarilyAllowFocus();

   auto toolManager = project.GetToolManager();
   auto botDock = toolManager->GetBotDock();


   // Define the set of windows we rotate among.
   static const unsigned rotationSize = 3u;

   wxWindow *const begin [rotationSize] = {
      project.GetTopPanel(),
      project.GetTrackPanel(),
      botDock,
   };

   const auto end = begin + rotationSize;

   // helper functions
   auto IndexOf = [&](wxWindow *pWindow) {
      return std::find(begin, end, pWindow) - begin;
   };

   auto FindAncestor = [&]() {
      wxWindow *pWindow = wxWindow::FindFocus();
      unsigned index = rotationSize;
      while ( pWindow &&
              (rotationSize == (index = IndexOf(pWindow) ) ) )
         pWindow = pWindow->GetParent();
      return index;
   };

   const auto idx = FindAncestor();
   if (idx == rotationSize)
      return;

   auto idx2 = idx;
   auto increment = (forward ? 1 : rotationSize - 1);

   while( idx != (idx2 = (idx2 + increment) % rotationSize) ) {
      wxWindow *toFocus = begin[idx2];
      bool bIsAnEmptyDock=false;
      if( idx2 != 1 )
         bIsAnEmptyDock = ((idx2==0) ? toolManager->GetTopDock() : botDock)->
         GetChildren().GetCount() < 1;

      // Skip docks that are empty (Bug 1564).
      if( !bIsAnEmptyDock ){
         toFocus->SetFocus();
         if ( FindAncestor() == idx2 )
            // The focus took!
            break;
      }
   }
}

void MenuCommandHandler::OnNextFrame(const CommandContext &context)
{
   auto &project = context.project;
   NextOrPrevFrame(project, true);
}

void MenuCommandHandler::OnPrevFrame(const CommandContext &context)
{
   auto &project = context.project;
   NextOrPrevFrame(project, false);
}

void MenuCommandHandler::OnNextWindow(const CommandContext &context)
{
   auto &project = context.project;
   auto isEnabled = project.IsEnabled();

   wxWindow *w = wxGetTopLevelParent(wxWindow::FindFocus());
   const auto & list = project.GetChildren();
   auto iter = list.begin(), end = list.end();

   // If the project window has the current focus, start the search with the first child
   if (w == &project)
   {
   }
   // Otherwise start the search with the current window's next sibling
   else
   {
      // Find the window in this projects children.  If the window with the
      // focus isn't a child of this project (like when a dialog is created
      // without specifying a parent), then we'll get back NULL here.
      while (iter != end && *iter != w)
         ++iter;
      if (iter != end)
         ++iter;
   }

   // Search for the next toplevel window
   for (; iter != end; ++iter)
   {
      // If it's a toplevel, visible (we have hidden windows) and is enabled,
      // then we're done.  The IsEnabled() prevents us from moving away from
      // a modal dialog because all other toplevel windows will be disabled.
      w = *iter;
      if (w->IsTopLevel() && w->IsShown() && w->IsEnabled())
      {
         break;
      }
   }

   // Ran out of siblings, so make the current project active
   if ((iter == end) && isEnabled)
   {
      w = &project;
   }

   // And make sure it's on top (only for floating windows...project window will not raise)
   // (Really only works on Windows)
   w->Raise();


#if defined(__WXMAC__) || defined(__WXGTK__)
   // bug 868
   // Simulate a TAB key press before continuing, else the cycle of
   // navigation among top level windows stops because the keystrokes don't
   // go to the CommandManager.
   if (dynamic_cast<wxDialog*>(w)) {
      w->SetFocus();
   }
#endif
}

void MenuCommandHandler::OnPrevWindow(const CommandContext &context)
{
   auto &project = context.project;
   auto isEnabled = project.IsEnabled();

   wxWindow *w = wxGetTopLevelParent(wxWindow::FindFocus());
   const auto & list = project.GetChildren();
   auto iter = list.rbegin(), end = list.rend();

   // If the project window has the current focus, start the search with the last child
   if (w == &project)
   {
   }
   // Otherwise start the search with the current window's previous sibling
   else
   {
      while (iter != end && *iter != w)
         ++iter;
      if (iter != end)
         ++iter;
   }

   // Search for the previous toplevel window
   for (; iter != end; ++iter)
   {
      // If it's a toplevel and is visible (we have come hidden windows), then we're done
      w = *iter;
      if (w->IsTopLevel() && w->IsShown() && isEnabled)
      {
         break;
      }
   }

   // Ran out of siblings, so make the current project active
   if ((iter == end) && isEnabled)
   {
      w = &project;
   }

   // And make sure it's on top (only for floating windows...project window will not raise)
   // (Really only works on Windows)
   w->Raise();


#if defined(__WXMAC__) || defined(__WXGTK__)
   // bug 868
   // Simulate a TAB key press before continuing, else the cycle of
   // navigation among top level windows stops because the keystrokes don't
   // go to the CommandManager.
   if (dynamic_cast<wxDialog*>(w)) {
      w->SetFocus();
   }
#endif
}

///The following methods operate controls on specified tracks,
///This will pop up the track panning dialog for specified track
void MenuCommandHandler::OnTrackPan(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   Track *const track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = trackPanel->PanSlider(wt);
      if (slider->ShowDialog())
         project.SetTrackPan(wt, slider);
   });
}

void MenuCommandHandler::OnTrackPanLeft(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   Track *const track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = trackPanel->PanSlider(wt);
      slider->Decrease(1);
      project.SetTrackPan(wt, slider);
   });
}

void MenuCommandHandler::OnTrackPanRight(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   Track *const track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = trackPanel->PanSlider(wt);
      slider->Increase(1);
      project.SetTrackPan(wt, slider);
   });
}

void MenuCommandHandler::OnTrackGain(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   /// This will pop up the track gain dialog for specified track
   Track *const track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = trackPanel->GainSlider(wt);
      if (slider->ShowDialog())
         project.SetTrackGain(wt, slider);
   });
}

void MenuCommandHandler::OnTrackGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   Track *const track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = trackPanel->GainSlider(wt);
      slider->Increase(1);
      project.SetTrackGain(wt, slider);
   });
}

void MenuCommandHandler::OnTrackGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   Track *const track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](WaveTrack *wt) {
      LWSlider *slider = trackPanel->GainSlider(wt);
      slider->Decrease(1);
      project.SetTrackGain(wt, slider);
   });
}

void MenuCommandHandler::OnTrackMenu(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   trackPanel->OnTrackMenu();
}

void MenuCommandHandler::OnTrackMute(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   const auto track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](PlayableTrack *t) {
      project.DoTrackMute(t, false);
   });
}

void MenuCommandHandler::OnTrackSolo(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   const auto track = trackPanel->GetFocusedTrack();
   if (track) track->TypeSwitch( [&](PlayableTrack *t) {
      project.DoTrackSolo(t, false);
   });
}

void MenuCommandHandler::OnTrackClose(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   Track *t = trackPanel->GetFocusedTrack();
   if (!t)
      return;

   auto isAudioActive = project.IsAudioActive();

   if (isAudioActive)
   {
      project.TP_DisplayStatusMessage(_("Can't delete track with active audio"));
      wxBell();
      return;
   }

   project.RemoveTrack(t);

   trackPanel->UpdateViewIfNoTracks();
   trackPanel->Refresh(false);
}

void MenuCommandHandler::OnTrackMoveUp(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *const focusedTrack = trackPanel->GetFocusedTrack();
   if (tracks->CanMoveUp(focusedTrack)) {
      MoveTrack(project, focusedTrack, OnMoveUpID);
      trackPanel->Refresh(false);
   }
}

void MenuCommandHandler::OnTrackMoveDown(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *const focusedTrack = trackPanel->GetFocusedTrack();
   if (tracks->CanMoveDown(focusedTrack)) {
      MoveTrack(project, focusedTrack, OnMoveDownID);
      trackPanel->Refresh(false);
   }
}

void MenuCommandHandler::OnTrackMoveTop(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *const focusedTrack = trackPanel->GetFocusedTrack();
   if (tracks->CanMoveUp(focusedTrack)) {
      MoveTrack(project, focusedTrack, OnMoveTopID);
      trackPanel->Refresh(false);
   }
}

void MenuCommandHandler::OnTrackMoveBottom(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();

   Track *const focusedTrack = trackPanel->GetFocusedTrack();
   if (tracks->CanMoveDown(focusedTrack)) {
      MoveTrack(project, focusedTrack, OnMoveBottomID);
      trackPanel->Refresh(false);
   }
}

/// Move a track up, down, to top or to bottom.

void MenuCommandHandler::MoveTrack
(AudacityProject &project, Track* target, MoveChoice choice)
{
   auto trackPanel = project.GetTrackPanel();
   auto tracks = project.GetTracks();
   auto mixerBoard = project.GetMixerBoard(); // Update mixer board.

   wxString longDesc, shortDesc;

   auto pt = dynamic_cast<PlayableTrack*>(target);
   switch (choice)
   {
   case OnMoveTopID:
      /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
      longDesc = _("Moved '%s' to Top");
      shortDesc = _("Move Track to Top");

      while (tracks->CanMoveUp(target)) {
         if (tracks->Move(target, true)) {
            if (mixerBoard && pt)
               mixerBoard->MoveTrackCluster(pt, true);
         }
      }
      break;
   case OnMoveBottomID:
      /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
      longDesc = _("Moved '%s' to Bottom");
      shortDesc = _("Move Track to Bottom");

      while (tracks->CanMoveDown(target)) {
         if(tracks->Move(target, false)) {
            if (mixerBoard && pt)
               mixerBoard->MoveTrackCluster(pt, false);
         }
      }
      break;
   default:
      bool bUp = (OnMoveUpID == choice);

      if (tracks->Move(target, bUp)) {
         if (mixerBoard && pt)
            mixerBoard->MoveTrackCluster(pt, bUp);
      }
      longDesc =
         /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
         bUp? _("Moved '%s' Up")
         : _("Moved '%s' Down");
      shortDesc =
         /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
         bUp? _("Move Track Up")
         : _("Move Track Down");

   }

   longDesc = longDesc.Format(target->GetName());

   project.PushState(longDesc, shortDesc);
   trackPanel->Refresh(false);
}

void MenuCommandHandler::OnInputDevice(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowInputDialog();
   }
}

void MenuCommandHandler::OnOutputDevice(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowOutputDialog();
   }
}

void MenuCommandHandler::OnAudioHost(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowHostDialog();
   }
}

void MenuCommandHandler::OnInputChannels(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetDeviceToolBar();

   if (tb) {
      tb->ShowChannelsDialog();
   }
}

void MenuCommandHandler::OnOutputGain(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->ShowOutputGainDialog();
   }
}

void MenuCommandHandler::OnInputGain(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->ShowInputGainDialog();
   }
}

void MenuCommandHandler::OnOutputGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustOutputGain(1);
   }
}

void MenuCommandHandler::OnOutputGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustOutputGain(-1);
   }
}

void MenuCommandHandler::OnInputGainInc(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustInputGain(1);
   }
}

void MenuCommandHandler::OnInputGainDec(const CommandContext &context)
{
   auto &project = context.project;
   auto tb = project.GetMixerToolBar();

   if (tb) {
      tb->AdjustInputGain(-1);
   }
}

/// DoAudacityCommand() takes a PluginID and executes the assocated effect.
///
/// At the moment flags are used only to indicate whether to prompt for parameters,
bool MenuCommandHandler::DoAudacityCommand(const PluginID & ID, const CommandContext & context, int flags)
{
   auto &project = context.project;
   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   if (flags & OnEffectFlags::kConfigured)
   {
      TransportActions::DoStop(project);
//    SelectAllIfNone();
   }

   EffectManager & em = EffectManager::Get();
   bool success = em.DoAudacityCommand(ID, 
      context,
      &project,
      (flags & OnEffectFlags::kConfigured) == 0);

   if (!success)
      return false;

/*
   if (em.GetSkipStateFlag())
      flags = flags | OnEffectFlags::kSkipState;

   if (!(flags & OnEffectFlags::kSkipState))
   {
      wxString shortDesc = em.GetCommandName(ID);
      wxString longDesc = em.GetCommandDescription(ID);
      PushState(longDesc, shortDesc);
   }
*/
   project.RedrawProject();
   return true;
}



//
// Effect Menus
//

/// DoEffect() takes a PluginID and has the EffectManager execute the assocated effect.
///
/// At the moment flags are used only to indicate whether to prompt for parameters,
/// whether to save the state to history and whether to allow 'Repeat Last Effect'.
bool MenuCommandHandler::DoEffect(
   const PluginID & ID, const CommandContext &context, int flags)
{
   AudacityProject &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto trackFactory = project.GetTrackFactory();
   auto rate = project.GetRate();
   auto &selectedRegion = project.GetSelection();
   auto commandManager = project.GetCommandManager();

   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   EffectType type = plug->GetEffectType();

   // Make sure there's no activity since the effect is about to be applied
   // to the project's tracks.  Mainly for Apply during RTP, but also used
   // for batch commands
   if (flags & MenuCommandHandler::OnEffectFlags::kConfigured)
   {
      TransportActions::DoStop(project);
      project.SelectAllIfNone();
   }

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   auto nTracksOriginally = project.GetTrackCount();
   wxWindow *focus = wxWindow::FindFocus();
   wxWindow *parent = nullptr;
   if (focus != nullptr) {
      parent = focus->GetParent();
   }

   bool success = false;
   auto cleanup = finally( [&] {

      if (!success) {
         // For now, we're limiting realtime preview to a single effect, so
         // make sure the menus reflect that fact that one may have just been
         // opened.
         GetMenuManager(project).UpdateMenus(project, false);
      }

   } );

   int count = 0;
   bool clean = true;
   for (auto t : tracks->Selected< const WaveTrack >()) {
      if (t->GetEndTime() != 0.0)
         clean = false;
      count++;
   }

   EffectManager & em = EffectManager::Get();

   success = em.DoEffect(ID, &project, rate,
      tracks, trackFactory, &selectedRegion,
      (flags & MenuCommandHandler::OnEffectFlags::kConfigured) == 0);

   if (!success)
      return false;

   if (em.GetSkipStateFlag())
      flags = flags | MenuCommandHandler::OnEffectFlags::kSkipState;

   if (!(flags & MenuCommandHandler::OnEffectFlags::kSkipState))
   {
      wxString shortDesc = em.GetCommandName(ID);
      wxString longDesc = em.GetCommandDescription(ID);
      project.PushState(longDesc, shortDesc);
   }

   if (!(flags & MenuCommandHandler::OnEffectFlags::kDontRepeatLast))
   {
      // Only remember a successful effect, don't remember insert,
      // or analyze effects.
      if (type == EffectTypeProcess) {
         wxString shortDesc = em.GetCommandName(ID);
         GetMenuManager(project).mLastEffect = ID;
         wxString lastEffectDesc;
         /* i18n-hint: %s will be the name of the effect which will be
          * repeated if this menu item is chosen */
         lastEffectDesc.Printf(_("Repeat %s"), shortDesc);
         commandManager->Modify(wxT("RepeatLastEffect"), lastEffectDesc);
      }
   }

   //STM:
   //The following automatically re-zooms after sound was generated.
   // IMO, it was disorienting, removing to try out without re-fitting
   //mchinen:12/14/08 reapplying for generate effects
   if (type == EffectTypeGenerate)
   {
      if (count == 0 || (clean && selectedRegion.t0() == 0.0))
         ViewActions::DoZoomFit(project);
         //  trackPanel->Refresh(false);
   }
   project.RedrawProject();
   if (focus != nullptr && focus->GetParent()==parent) {
      focus->SetFocus();
   }

   // A fix for Bug 63
   // New tracks added?  Scroll them into view so that user sees them.
   // Don't care what track type.  An analyser might just have added a
   // Label track and we want to see it.
   if( project.GetTrackCount() > nTracksOriginally ){
      // 0.0 is min scroll position, 1.0 is max scroll position.
      trackPanel->VerticalScroll( 1.0 );
   }  else {
      trackPanel->EnsureVisible(trackPanel->GetFirstSelectedTrack());
      trackPanel->Refresh(false);
   }

   return true;
}

void MenuCommandHandler::OnEffect(const CommandContext &context)
{
   DoEffect(context.parameter, context, 0);
}

void MenuCommandHandler::OnRepeatLastEffect(const CommandContext &context)
{
   auto lastEffect = GetMenuManager(context.project).mLastEffect;
   if (!lastEffect.IsEmpty())
   {
      DoEffect(lastEffect,
         context, OnEffectFlags::kConfigured);
   }
}


void MenuCommandHandler::RebuildAllMenuBars()
{
   for( size_t i = 0; i < gAudacityProjects.size(); i++ ) {
      AudacityProject *p = gAudacityProjects[i].get();

      GetMenuManager(*p).RebuildMenuBar(*p);
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
      wxRect r = p->GetRect();
      p->SetSize(wxSize(1,1));
      p->SetSize(r.GetSize());
#endif
   }
}

void MenuCommandHandler::DoManagePluginsMenu
(AudacityProject &project, EffectType type)
{
   if (PluginManager::Get().ShowManager(&project, type))
      RebuildAllMenuBars();
}

void MenuCommandHandler::OnManageGenerators(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeGenerate);
}

void MenuCommandHandler::OnManageEffects(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeProcess);
}

void MenuCommandHandler::OnManageAnalyzers(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeAnalyze);
}

void MenuCommandHandler::OnManageTools(const CommandContext &context )
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeTool);
}


void MenuCommandHandler::OnStereoToMono(const CommandContext &context)
{
   DoEffect(EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono")),
      context,
      OnEffectFlags::kConfigured);
}

void MenuCommandHandler::OnAudacityCommand(const CommandContext & ctx)
{
   wxLogDebug( "Command was: %s", ctx.parameter);
   DoAudacityCommand(EffectManager::Get().GetEffectByIdentifier(ctx.parameter),
      ctx,
      OnEffectFlags::kNone);  // Not configured, so prompt user.
}

//
// File Menu
//

void MenuCommandHandler::OnCheckDependencies(const CommandContext &context)
{
   auto &project = context.project;
   ::ShowDependencyDialogIfNeeded(&project, false);
}

//
// Edit Menu
//


void AudacityProject::SelectNone()
{
   for (auto t : GetTracks()->Any())
      t->SetSelected(false);

   mTrackPanel->Refresh(false);
   if (mMixerBoard)
      mMixerBoard->Refresh(false);
}

//
// View Menu
//

double AudacityProject::GetScreenEndTime() const
{
   return mTrackPanel->GetScreenEndTime();
}

void AudacityProject::ZoomInByFactor( double ZoomFactor )
{
   // LLL: Handling positioning differently when audio is
   // actively playing.  Don't do this if paused.
   if ((gAudioIO->IsStreamActive(GetAudioIOToken()) != 0) && !gAudioIO->IsPaused()){
      ZoomBy(ZoomFactor);
      mTrackPanel->ScrollIntoView(gAudioIO->GetStreamTime());
      mTrackPanel->Refresh(false);
      return;
   }

   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   const double endTime = GetScreenEndTime();
   const double duration = endTime - mViewInfo.h;

   bool selectionIsOnscreen =
      (mViewInfo.selectedRegion.t0() < endTime) &&
      (mViewInfo.selectedRegion.t1() >= mViewInfo.h);

   bool selectionFillsScreen =
      (mViewInfo.selectedRegion.t0() < mViewInfo.h) &&
      (mViewInfo.selectedRegion.t1() > endTime);

   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (mViewInfo.selectedRegion.t0() +
                          mViewInfo.selectedRegion.t1()) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < mViewInfo.h)
         selCenter = mViewInfo.h +
                     (mViewInfo.selectedRegion.t1() - mViewInfo.h) / 2;
      if (selCenter > endTime)
         selCenter = endTime -
            (endTime - mViewInfo.selectedRegion.t0()) / 2;

      // Zoom in
      ZoomBy(ZoomFactor);
      const double newDuration = GetScreenEndTime() - mViewInfo.h;

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - newDuration / 2);
      return;
   }


   double origLeft = mViewInfo.h;
   double origWidth = duration;
   ZoomBy(ZoomFactor);

   const double newDuration = GetScreenEndTime() - mViewInfo.h;
   double newh = origLeft + (origWidth - newDuration) / 2;

   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.selectedRegion.t1() < newh + mViewInfo.screen / 3)
      newh = mViewInfo.selectedRegion.t1() - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.selectedRegion.t0() > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.selectedRegion.t0() - mViewInfo.screen * 2 / 3;
   */

   TP_ScrollWindow(newh);
}


void AudacityProject::ZoomOutByFactor( double ZoomFactor )
{
   //Zoom() may change these, so record original values:
   const double origLeft = mViewInfo.h;
   const double origWidth = GetScreenEndTime() - origLeft;

   ZoomBy(ZoomFactor);
   const double newWidth = GetScreenEndTime() - mViewInfo.h;

   const double newh = origLeft + (origWidth - newWidth) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);
}

void MenuCommandHandler::OnApplyMacroDirectly(const CommandContext &context )
{
   auto &project = context.project;

   //wxLogDebug( "Macro was: %s", context.parameter);
   ApplyMacroDialog dlg( &project );
   wxString Name = context.parameter;

// We used numbers previously, but macros could get renumbered, making
// macros containing macros unpredictable.
#ifdef MACROS_BY_NUMBERS
   long item=0;
   // Take last three letters (of e.g. Macro007) and convert to a number.
   Name.Mid( Name.Length() - 3 ).ToLong( &item, 10 );
   dlg.ApplyMacroToProject( item, false );
#else
   dlg.ApplyMacroToProject( Name, false );
#endif
   MenuManager::ModifyUndoMenuItems( project );
}

void MenuCommandHandler::OnApplyMacrosPalette(const CommandContext &context )
{
   auto &project = context.project;
   project.GetMacrosWindow( false, true );
}

void MenuCommandHandler::OnManageMacros(const CommandContext &context )
{
   auto &project = context.project;
   project.GetMacrosWindow( true, true );
}

void MenuCommandHandler::OnPlotSpectrum(const CommandContext &context)
{
   auto &project = context.project;
   auto freqWindow = project.GetFreqWindow(true);


   if( ScreenshotCommand::MayCapture( freqWindow ) )
      return;
   freqWindow->Show(true);
   freqWindow->Raise();
   freqWindow->SetFocus();
}

void MenuCommandHandler::OnContrast(const CommandContext &context)
{
   auto &project = context.project;
   auto contrastDialog = project.GetContrastDialog(true);


   contrastDialog->CentreOnParent();
   if( ScreenshotCommand::MayCapture( contrastDialog ) )
      return;
   contrastDialog->Show();
}


//
// Project Menu
//

void MenuCommandHandler::HandleMixAndRender
(AudacityProject &project, bool toNewTrack)
{
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto rate = project.GetRate();
   auto defaultFormat = project.GetDefaultFormat();
   auto trackPanel = project.GetTrackPanel();

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   WaveTrack::Holder uNewLeft, uNewRight;
   ::MixAndRender(
      tracks, trackFactory, rate, defaultFormat, 0.0, 0.0, uNewLeft, uNewRight);

   if (uNewLeft) {
      // Remove originals, get stats on what tracks were mixed

      auto trackRange = tracks->Selected< WaveTrack >();
      auto selectedCount = (trackRange + &Track::IsLeader).size();
      wxString firstName;
      if (selectedCount > 0)
         firstName = (*trackRange.begin())->GetName();
      if (!toNewTrack)  {
         // Beware iterator invalidation!
         for (auto &it = trackRange.first, &end = trackRange.second; it != end;)
            tracks->Remove( *it++ );
      }

      // Add NEW tracks

      auto pNewLeft = tracks->Add(std::move(uNewLeft));
      decltype(pNewLeft) pNewRight{};
      if (uNewRight)
         pNewRight = tracks->Add(std::move(uNewRight));

      // Do this only after adding tracks to the list
      tracks->GroupChannels(*pNewLeft, pNewRight ? 2 : 1);

      // If we're just rendering (not mixing), keep the track name the same
      if (selectedCount==1) {
         pNewLeft->SetName(firstName);
         if (pNewRight)
            pNewRight->SetName(firstName);
      }

      // Smart history/undo message
      if (selectedCount==1) {
         wxString msg;
         msg.Printf(_("Rendered all audio in track '%s'"), firstName);
         /* i18n-hint: Convert the audio into a more usable form, so apply
          * panning and amplification and write to some external file.*/
         project.PushState(msg, _("Render"));
      }
      else {
         wxString msg;
         if (pNewRight)
            msg.Printf(_("Mixed and rendered %d tracks into one new stereo track"),
                       selectedCount);
         else
            msg.Printf(_("Mixed and rendered %d tracks into one new mono track"),
                       selectedCount);
         project.PushState(msg, _("Mix and Render"));
      }

      trackPanel->SetFocus();
      trackPanel->SetFocusedTrack(pNewLeft);
      trackPanel->EnsureVisible(pNewLeft);
      project.RedrawProject();
   }
}

void MenuCommandHandler::OnMixAndRender(const CommandContext &context)
{
   auto &project = context.project;
   HandleMixAndRender(project, false);
}

void MenuCommandHandler::OnMixAndRenderToNewTrack(const CommandContext &context)
{
   auto &project = context.project;
   HandleMixAndRender(project, true);
}

void MenuCommandHandler::HandleAlign
(AudacityProject &project, int index, bool moveSel)
{
   auto tracks = project.GetTracks();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;

   wxString action;
   wxString shortAction;
   double delta = 0.0;
   double newPos = -1.0;

   auto channelRange = tracks->Selected< AudioTrack >();
   auto trackRange = tracks->SelectedLeaders< AudioTrack >();

   auto FindOffset = []( const Track *pTrack ) {
      return TrackList::Channels(pTrack).min( &Track::GetOffset ); };

   auto firstTrackOffset = [&]{ return FindOffset( *trackRange.begin() ); };
   auto minOffset = [&]{ return trackRange.min( FindOffset ); };
   auto avgOffset = [&]{
      return trackRange.sum( FindOffset ) /
                             std::max( size_t(1), trackRange.size() ); };

   auto maxEndOffset = [&]{
      return std::max(0.0, channelRange.max( &Track::GetEndTime ) ); };

   switch(index) {
   case kAlignStartZero:
      delta = -minOffset();
      action = moveSel
         /* i18n-hint: In this and similar messages describing editing actions,
            the starting or ending points of tracks are re-"aligned" to other
            times, and the time selection may be "moved" too.  The first
            noun -- "start" in this example -- is the object of a verb (not of
            an implied preposition "from"). */
         ? _("Aligned/Moved start to zero")
         : _("Aligned start to zero");
         /* i18n-hint: This and similar messages give shorter descriptions of
            the aligning and moving editing actions */
      shortAction = moveSel
         ? _("Align/Move Start")
         : _("Align Start");
      break;
   case kAlignStartSelStart:
      delta = selectedRegion.t0() - minOffset();
      action = moveSel
         ? _("Aligned/Moved start to cursor/selection start")
         : _("Aligned start to cursor/selection start");
      shortAction = moveSel
         ? _("Align/Move Start")
         : _("Align Start");
      break;
   case kAlignStartSelEnd:
      delta = selectedRegion.t1() - minOffset();
      action = moveSel
         ? _("Aligned/Moved start to selection end")
         : _("Aligned start to selection end");
      shortAction = moveSel
         ? _("Align/Move Start")
         : _("Align Start");
      break;
   case kAlignEndSelStart:
      delta = selectedRegion.t0() - maxEndOffset();
      action = moveSel
         ? _("Aligned/Moved end to cursor/selection start")
         : _("Aligned end to cursor/selection start");
      shortAction =
         moveSel
         ? _("Align/Move End")
         : _("Align End");
      break;
   case kAlignEndSelEnd:
      delta = selectedRegion.t1() - maxEndOffset();
      action = moveSel
         ? _("Aligned/Moved end to selection end")
         : _("Aligned end to selection end");
      shortAction =
         moveSel
         ? _("Align/Move End")
         : _("Align End");
      break;
   // index set in alignLabelsNoSync
   case kAlignEndToEnd:
      newPos = firstTrackOffset();
      action = moveSel
         ? _("Aligned/Moved end to end")
         : _("Aligned end to end");
      shortAction =
         moveSel
         ? _("Align/Move End to End")
         : _("Align End to End");
      break;
   case kAlignTogether:
      newPos = avgOffset();
      action = moveSel
         ? _("Aligned/Moved together")
         : _("Aligned together");
      shortAction =
         moveSel
         ? _("Align/Move Together")
         : _("Align Together");
   }

   if ((unsigned)index >= kAlignLabelsCount) { // This is an alignLabelsNoSync command.
      for (auto t : tracks->SelectedLeaders< AudioTrack >()) {
         // This shifts different tracks in different ways, so no sync-lock move.
         // Only align Wave and Note tracks end to end.
         auto channels = TrackList::Channels(t);

         auto trackStart = channels.min( &Track::GetStartTime );
         auto trackEnd = channels.max( &Track::GetEndTime );

         for (auto channel : channels)
            // Move the track
            channel->SetOffset(newPos + channel->GetStartTime() - trackStart);

         if (index == kAlignEndToEnd)
            newPos += (trackEnd - trackStart);
      }
      if (index == kAlignEndToEnd) {
         ViewActions::DoZoomFit(project);
      }
   }

   if (delta != 0.0) {
      // For a fixed-distance shift move sync-lock selected tracks also.
      for (auto t : tracks->Any() + &Track::IsSelectedOrSyncLockSelected )
         t->SetOffset(t->GetOffset() + delta);
   }

   if (moveSel)
      selectedRegion.move(delta);

   project.PushState(action, shortAction);

   project.RedrawProject();
}

void MenuCommandHandler::OnAlignNoSync(const CommandContext &context)
{
   auto &project = context.project;

   // Add length of alignLabels array so that we can handle this in AudacityProject::HandleAlign.
   HandleAlign(project,
      context.index + kAlignLabelsCount, false);
}

void MenuCommandHandler::OnAlign(const CommandContext &context)
{
   auto &project = context.project;

   bool bMoveWith;
   gPrefs->Read(wxT("/GUI/MoveSelectionWithTracks"), &bMoveWith, false);
   HandleAlign(project, context.index, bMoveWith);
}
/*
// Now handled in OnAlign.
void AudacityProject::OnAlignMoveSel(int index)
{
   HandleAlign(index, true);
}
*/

#ifdef EXPERIMENTAL_SCOREALIGN
// rough relative amount of time to compute one
//    frame of audio or midi, or one cell of matrix, or one iteration
//    of smoothing, measured on a 1.9GHz Core 2 Duo in 32-bit mode
//    (see COLLECT_TIMING_DATA below)
#define AUDIO_WORK_UNIT 0.004F
#define MIDI_WORK_UNIT 0.0001F
#define MATRIX_WORK_UNIT 0.000002F
#define SMOOTHING_WORK_UNIT 0.000001F

// Write timing data to a file; useful for calibrating AUDIO_WORK_UNIT,
// MIDI_WORK_UNIT, MATRIX_WORK_UNIT, and SMOOTHING_WORK_UNIT coefficients
// Data is written to timing-data.txt; look in
//     audacity-src/win/Release/modules/
#define COLLECT_TIMING_DATA

// Audacity Score Align Progress class -- progress reports come here
class ASAProgress final : public SAProgress {
 private:
   float mTotalWork;
   float mFrames[2];
   long mTotalCells; // how many matrix cells?
   long mCellCount; // how many cells so far?
   long mPrevCellCount; // cell_count last reported with Update()
   Maybe<ProgressDialog> mProgress;
   #ifdef COLLECT_TIMING_DATA
      FILE *mTimeFile;
      wxDateTime mStartTime;
      long iterations;
   #endif

 public:
   ASAProgress() {
      smoothing = false;
      #ifdef COLLECT_TIMING_DATA
         mTimeFile = fopen("timing-data.txt", "w");
      #endif
   }
   ~ASAProgress() {
      #ifdef COLLECT_TIMING_DATA
         fclose(mTimeFile);
      #endif
   }
   void set_phase(int i) override {
      float work[2]; // chromagram computation work estimates
      float work2, work3 = 0; // matrix and smoothing work estimates
      SAProgress::set_phase(i);
      #ifdef COLLECT_TIMING_DATA
         long ms = 0;
         wxDateTime now = wxDateTime::UNow();
         wxFprintf(mTimeFile, "Phase %d begins at %s\n",
                 i, now.FormatTime());
         if (i != 0)
            ms = now.Subtract(mStartTime).GetMilliseconds().ToLong();
         mStartTime = now;
      #endif
      if (i == 0) {
         mCellCount = 0;
         for (int j = 0; j < 2; j++) {
            mFrames[j] = durations[j] / frame_period;
         }
         mTotalWork = 0;
         for (int j = 0; j < 2; j++) {
             work[j] =
                (is_audio[j] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[j];
             mTotalWork += work[j];
         }
         mTotalCells = mFrames[0] * mFrames[1];
         work2 = mTotalCells * MATRIX_WORK_UNIT;
         mTotalWork += work2;
         // arbitarily assume 60 iterations to fit smooth segments and
         // per frame per iteration is SMOOTHING_WORK_UNIT
         if (smoothing) {
            work3 =
               wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT * 40;
            mTotalWork += work3;
         }
         #ifdef COLLECT_TIMING_DATA
            wxFprintf(mTimeFile, " mTotalWork (an estimate) = %g\n", mTotalWork);
            wxFprintf(mTimeFile, " work0 = %g, frames %g, is_audio %d\n",
                    work[0], mFrames[0], is_audio[0]);
            wxFprintf(mTimeFile, " work1 = %g, frames %g, is_audio %d\n",
                    work[1], mFrames[1], is_audio[1]);
            wxFprintf(mTimeFile, "work2 = %g, work3 = %g\n", work2, work3);
         #endif
         mProgress.create(_("Synchronize MIDI with Audio"),
                               _("Synchronizing MIDI and Audio Tracks"));
      } else if (i < 3) {
         wxFprintf(mTimeFile,
               "Phase %d took %d ms for %g frames, coefficient = %g s/frame\n",
               i - 1, ms, mFrames[i - 1], (ms * 0.001) / mFrames[i - 1]);
      } else if (i == 3) {
        wxFprintf(mTimeFile,
                "Phase 2 took %d ms for %d cells, coefficient = %g s/cell\n",
                ms, mCellCount, (ms * 0.001) / mCellCount);
      } else if (i == 4) {
        wxFprintf(mTimeFile, "Phase 3 took %d ms for %d iterations on %g frames, coefficient = %g s per frame per iteration\n",
                ms, iterations, wxMax(mFrames[0], mFrames[1]),
                (ms * 0.001) / (wxMax(mFrames[0], mFrames[1]) * iterations));
      }
   }
   bool set_feature_progress(float s) override {
      float work;
      if (phase == 0) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      } else if (phase == 1) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
                (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      }
      auto updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == ProgressResult::Success);
   }
   bool set_matrix_progress(int cells) override {
      mCellCount += cells;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1];
      work += mCellCount * MATRIX_WORK_UNIT;
      auto updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == ProgressResult::Success);
   }
   bool set_smoothing_progress(int i) override {
      iterations = i;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1] +
             MATRIX_WORK_UNIT * mFrames[0] * mFrames[1];
      work += i * wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT;
      auto updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == ProgressResult::Success);
   }
};


long mixer_process(void *mixer, float **buffer, long n)
{
   Mixer *mix = (Mixer *) mixer;
   long frame_count = mix->Process(std::max(0L, n));
   *buffer = (float *) mix->GetBuffer();
   return frame_count;
}

void MenuCommandHandler::OnScoreAlign(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   const auto rate = project.GetRate();

   int numWaveTracksSelected = 0;
   int numNoteTracksSelected = 0;
   int numOtherTracksSelected = 0;
   double endTime = 0.0;

   // Iterate through once to make sure that there is exactly
   // one WaveTrack and one NoteTrack selected.
   GetTracks()->Selected().Visit(
      [&](WaveTrack *wt) {
         numWaveTracksSelected++;
         endTime = endTime > wt->GetEndTime() ? endTime : wt->GetEndTime();
      },
      [&](NoteTrack *) {
         numNoteTracksSelected++;
      },
      [&](Track*) {
         numOtherTracksSelected++;
      }
   );

   if(numWaveTracksSelected == 0 ||
      numNoteTracksSelected != 1 ||
      numOtherTracksSelected != 0){
      AudacityMessageBox(wxString::Format(wxT("Please select at least one audio track and one MIDI track.")));
      return;
   }

   // Creating the dialog also stores dialog into gScoreAlignDialog so
   // that it can be delted by CloseScoreAlignDialog() either here or
   // if the program is quit by the user while the dialog is up.
   ScoreAlignParams params;

   // safe because the class maintains a global resource pointer
   safenew ScoreAlignDialog(params);

   CloseScoreAlignDialog();

   if (params.mStatus != wxID_OK) return;

   // We're going to do it.
   //pushing the state before the change is wrong (I think)
   //PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));
   // Make a copy of the note track in case alignment is canceled or fails
   auto holder = nt->Duplicate();
   auto alignedNoteTrack = static_cast<NoteTrack*>(holder.get());
   // Remove offset from NoteTrack because audio is
   // mixed starting at zero and incorporating clip offsets.
   if (alignedNoteTrack->GetOffset() < 0) {
      // remove the negative offset data before alignment
      nt->Clear(alignedNoteTrack->GetOffset(), 0);
   } else if (alignedNoteTrack->GetOffset() > 0) {
      alignedNoteTrack->Shift(alignedNoteTrack->GetOffset());
   }
   alignedNoteTrack->SetOffset(0);

   WaveTrackConstArray waveTracks =
      tracks->GetWaveTrackConstArray(true /* selectionOnly */);

   int result;
   {
      Mixer mix(
         waveTracks,              // const WaveTrackConstArray &inputTracks
         false, // mayThrow -- is this right?
         Mixer::WarpOptions{ tracks->GetTimeTrack() }, // const WarpOptions &warpOptions
         0.0,                     // double startTime
         endTime,                 // double stopTime
         2,                       // int numOutChannels
         44100u,                   // size_t outBufferSize
         true,                    // bool outInterleaved
         rate,                   // double outRate
         floatSample,             // sampleFormat outFormat
         true,                    // bool highQuality = true
         NULL);                   // MixerSpec *mixerSpec = NULL

      ASAProgress progress;

      // There's a lot of adjusting made to incorporate the note track offset into
      // the note track while preserving the position of notes within beats and
      // measures. For debugging, you can see just the pre-scorealign note track
      // manipulation by setting SKIP_ACTUAL_SCORE_ALIGNMENT. You could then, for
      // example, save the modified note track in ".gro" form to read the details.
      //#define SKIP_ACTUAL_SCORE_ALIGNMENT 1
#ifndef SKIP_ACTUAL_SCORE_ALIGNMENT
      result = scorealign((void *) &mix, &mixer_process,
         2 /* channels */, 44100.0 /* srate */, endTime,
         &alignedNoteTrack->GetSeq(), &progress, params);
#else
      result = SA_SUCCESS;
#endif
   }

   if (result == SA_SUCCESS) {
      tracks->Replace(nt, std::move(holder));
      project.RedrawProject();
      AudacityMessageBox(wxString::Format(
         _("Alignment completed: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs."),
         params.mMidiStart, params.mMidiEnd,
         params.mAudioStart, params.mAudioEnd));
      project.PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));
   } else if (result == SA_TOOSHORT) {
      AudacityMessageBox(wxString::Format(
         _("Alignment error: input too short: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs."),
         params.mMidiStart, params.mMidiEnd,
         params.mAudioStart, params.mAudioEnd));
   } else if (result == SA_CANCEL) {
      // wrong way to recover...
      //GetActiveProject()->OnUndo(); // recover any changes to note track
      return; // no message when user cancels alignment
   } else {
      //GetActiveProject()->OnUndo(); // recover any changes to note track
      AudacityMessageBox(_("Internal error reported by alignment process."));
   }
}
#endif /* EXPERIMENTAL_SCOREALIGN */


void MenuCommandHandler::OnNewWaveTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto trackPanel = project.GetTrackPanel();
   auto defaultFormat = project.GetDefaultFormat();
   auto rate = project.GetRate();

   auto t = tracks->Add(trackFactory->NewWaveTrack(defaultFormat, rate));
   project.SelectNone();

   t->SetSelected(true);

   project.PushState(_("Created new audio track"), _("New Track"));

   project.RedrawProject();
   trackPanel->EnsureVisible(t);
}

void MenuCommandHandler::OnNewStereoTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto trackPanel = project.GetTrackPanel();
   auto defaultFormat = project.GetDefaultFormat();
   auto rate = project.GetRate();

   project.SelectNone();

   auto left = tracks->Add(trackFactory->NewWaveTrack(defaultFormat, rate));
   left->SetSelected(true);

   auto right = tracks->Add(trackFactory->NewWaveTrack(defaultFormat, rate));
   right->SetSelected(true);

   tracks->GroupChannels(*left, 2);

   project.PushState(_("Created new stereo audio track"), _("New Track"));

   project.RedrawProject();
   trackPanel->EnsureVisible(left);
}

void MenuCommandHandler::OnNewLabelTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto trackPanel = project.GetTrackPanel();

   auto t = tracks->Add(trackFactory->NewLabelTrack());

   project.SelectNone();

   t->SetSelected(true);

   project.PushState(_("Created new label track"), _("New Track"));

   project.RedrawProject();
   trackPanel->EnsureVisible(t);
}

void MenuCommandHandler::OnNewTimeTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackFactory = project.GetTrackFactory();
   auto trackPanel = project.GetTrackPanel();

   if (tracks->GetTimeTrack()) {
      AudacityMessageBox(_("This version of Audacity only allows one time track for each project window."));
      return;
   }

   auto t = tracks->AddToHead(trackFactory->NewTimeTrack());

   project.SelectNone();

   t->SetSelected(true);

   project.PushState(_("Created new time track"), _("New Track"));

   project.RedrawProject();
   trackPanel->EnsureVisible(t);
}

void MenuCommandHandler::OnMoveSelectionWithTracks(const CommandContext &WXUNUSED(context) )
{
   bool bMoveWith;
   gPrefs->Read(wxT("/GUI/MoveSelectionWithTracks"), &bMoveWith, false);
   gPrefs->Write(wxT("/GUI/MoveSelectionWithTracks"), !bMoveWith);
   gPrefs->Flush();

}

void MenuCommandHandler::OnSyncLock(const CommandContext &context)
{
   auto &project = context.project;
   auto trackPanel = project.GetTrackPanel();

   bool bSyncLockTracks;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &bSyncLockTracks, false);
   gPrefs->Write(wxT("/GUI/SyncLockTracks"), !bSyncLockTracks);
   gPrefs->Flush();

   // Toolbar, project sync-lock handled within
   MenuManager::ModifyAllProjectToolbarMenus();

   trackPanel->Refresh(false);
}



void MenuCommandHandler::OnRemoveTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto mixerBoard = project.GetMixerBoard();

   std::vector<Track*> toRemove;
   for (auto track : tracks->Selected())
      toRemove.push_back(track);

   // Capture the track preceding the first removed track
   Track *f{};
   if (!toRemove.empty()) {
      auto found = tracks->Find(toRemove[0]);
      f = *--found;
   }

   if (mixerBoard)
      for (auto track : tracks->Selected<PlayableTrack>())
         mixerBoard->RemoveTrackCluster(track);

   for (auto track : toRemove)
      tracks->Remove(track);

   if (!f)
      // try to use the last track
      f = *tracks->Any().rbegin();
   if (f) {
      // Try to use the first track after the removal
      auto found = tracks->FindLeader(f);
      auto t = *++found;
      if (t)
         f = t;
   }

   // If we actually have something left, then make sure it's seen
   if (f)
      trackPanel->EnsureVisible(f);

   project.PushState(_("Removed audio track(s)"), _("Remove Track"));

   trackPanel->UpdateViewIfNoTracks();
   trackPanel->Refresh(false);

   if (mixerBoard)
      mixerBoard->Refresh(true);
}

//
// Help Menu
//

void MenuCommandHandler::OnAbout(const CommandContext &context)
{
#ifdef __WXMAC__
   // Modeless dialog, consistent with other Mac applications
   wxCommandEvent dummy;
   wxGetApp().OnMenuAbout(dummy);
#else
   auto &project = context.project;

   // Windows and Linux still modal.
   AboutDialog dlog(&project);
   dlog.ShowModal();
#endif
}

void MenuCommandHandler::OnHelpWelcome(const CommandContext &context)
{
   auto &project = context.project;
   SplashDialog::Show2( &project );
}

void MenuCommandHandler::OnQuickHelp(const CommandContext &context)
{
   auto &project = context.project;
   HelpSystem::ShowHelp(
      &project,
      wxT("Quick_Help"));
}


void MenuCommandHandler::OnQuickFix(const CommandContext &context)
{
   auto &project = context.project;
   QuickFixDialog dlg( &project );
   dlg.ShowModal();
}

void MenuCommandHandler::OnManual(const CommandContext &context)
{
   auto &project = context.project;
   HelpSystem::ShowHelp(
      &project,
      wxT("Main_Page"));
}

void MenuCommandHandler::OnCheckForUpdates(const CommandContext &WXUNUSED(context))
{
   ::OpenInDefaultBrowser( VerCheckUrl());
}

// Only does the update checks if it's an ALPHA build and not disabled by preferences.
void MenuCommandHandler::MayCheckForUpdates(AudacityProject &project)
{
#ifdef IS_ALPHA
   OnCheckForUpdates(project);
#endif
}

void MenuCommandHandler::OnShowLog(const CommandContext &WXUNUSED(context) )
{
   AudacityLogger *logger = wxGetApp().GetLogger();
   if (logger) {
      logger->Show();
   }
}

void MenuCommandHandler::OnBenchmark(const CommandContext &context)
{
   auto &project = context.project;
   ::RunBenchmark(&project);
}

#if defined(EXPERIMENTAL_CRASH_REPORT)
void MenuCommandHandler::OnCrashReport(const CommandContext &WXUNUSED(context) )
{
// Change to "1" to test a real crash
#if 0
   char *p = 0;
   *p = 1234;
#endif
   wxGetApp().GenerateCrashReport(wxDebugReport::Context_Current);
}
#endif

void MenuCommandHandler::OnSimulateRecordingErrors(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool &setting = gAudioIO->mSimulateRecordingErrors;
   commandManager->Check(wxT("SimulateRecordingErrors"), !setting);
   setting = !setting;
}

void MenuCommandHandler::OnDetectUpstreamDropouts(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool &setting = gAudioIO->mDetectUpstreamDropouts;
   commandManager->Check(wxT("DetectUpstreamDropouts"), !setting);
   setting = !setting;
}

void MenuCommandHandler::OnScreenshot(const CommandContext &WXUNUSED(context) )
{
   ::OpenScreenshotTools();
}

void MenuCommandHandler::OnAudioDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;

   wxString info = gAudioIO->GetDeviceInfo();

   wxDialogWrapper dlg(&project, wxID_ANY, wxString(_("Audio Device Info")));
   dlg.SetName(dlg.GetTitle());
   ShuttleGui S(&dlg, eIsCreating);

   wxTextCtrl *text;
   S.StartVerticalLay();
   {
      S.SetStyle(wxTE_MULTILINE | wxTE_READONLY);
      text = S.Id(wxID_STATIC).AddTextWindow(info);
      S.AddStandardButtons(eOkButton | eCancelButton);
   }
   S.EndVerticalLay();

   dlg.FindWindowById(wxID_OK)->SetLabel(_("&Save"));
   dlg.SetSize(350, 450);

   if (dlg.ShowModal() == wxID_OK)
   {
      wxString fName = FileNames::SelectFile(FileNames::Operation::Export,
                                    _("Save Device Info"),
                                    wxEmptyString,
                                    wxT("deviceinfo.txt"),
                                    wxT("txt"),
                                    wxT("*.txt"),
                                    wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                                    &project);
      if (!fName.IsEmpty())
      {
         if (!text->SaveFile(fName))
         {
            AudacityMessageBox(_("Unable to save device info"), _("Save Device Info"));
         }
      }
   }
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MenuCommandHandler::OnMidiDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;

   wxString info = gAudioIO->GetMidiDeviceInfo();

   wxDialogWrapper dlg(&project, wxID_ANY, wxString(_("MIDI Device Info")));
   dlg.SetName(dlg.GetTitle());
   ShuttleGui S(&dlg, eIsCreating);

   wxTextCtrl *text;
   S.StartVerticalLay();
   {
      S.SetStyle(wxTE_MULTILINE | wxTE_READONLY);
      text = S.Id(wxID_STATIC).AddTextWindow(info);
      S.AddStandardButtons(eOkButton | eCancelButton);
   }
   S.EndVerticalLay();

   dlg.FindWindowById(wxID_OK)->SetLabel(_("&Save"));
   dlg.SetSize(350, 450);

   if (dlg.ShowModal() == wxID_OK)
   {
      wxString fName = FileNames::SelectFile(FileNames::Operation::Export,
         _("Save MIDI Device Info"),
         wxEmptyString,
         wxT("midideviceinfo.txt"),
         wxT("txt"),
         wxT("*.txt"),
         wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
         &project);
      if (!fName.IsEmpty())
      {
         if (!text->SaveFile(fName))
         {
            AudacityMessageBox(_("Unable to save MIDI device info"), _("Save MIDI Device Info"));
         }
      }
   }
}
#endif

void MenuCommandHandler::DoPanTracks(AudacityProject &project, float PanValue)
{
   auto tracks = project.GetTracks();
   auto mixerBoard = project.GetMixerBoard();

   // count selected wave tracks
   const auto range = tracks->Any< WaveTrack >();
   const auto selectedRange = range + &Track::IsSelected;
   auto count = selectedRange.size();

   // iter through them, all if none selected.
   for (auto left : count == 0 ? range : selectedRange )
      left->SetPan( PanValue );

   project.RedrawProject();
   if (mixerBoard)
      mixerBoard->UpdatePan();

   auto flags = UndoPush::AUTOSAVE;
   /*i18n-hint: One or more audio tracks have been panned*/
   project.PushState(_("Panned audio track(s)"), _("Pan Track"), flags);
         flags = flags | UndoPush::CONSOLIDATE;
}

void MenuCommandHandler::OnPanLeft(const CommandContext &context)
{
   auto &project = context.project;
   DoPanTracks( project, -1.0);
}

void MenuCommandHandler::OnPanRight(const CommandContext &context)
{
   auto &project = context.project;
   DoPanTracks( project, 1.0);
}

void MenuCommandHandler::OnPanCenter(const CommandContext &context)
{
   auto &project = context.project;
   DoPanTracks( project, 0.0);
}

void MenuCommandHandler::OnMuteAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto soloSimple = project.IsSoloSimple();
   auto soloNone = project.IsSoloNone();
   auto mixerBoard = project.GetMixerBoard();

   for (auto pt : tracks->Any<PlayableTrack>())
   {
      pt->SetMute(true);
      if (soloSimple || soloNone)
         pt->SetSolo(false);
   }

   project.ModifyState(true);
   project.RedrawProject();
   if (mixerBoard) {
      mixerBoard->UpdateMute();
      if (soloSimple || soloNone)
         mixerBoard->UpdateSolo();
   }
}

void MenuCommandHandler::OnUnmuteAllTracks(const CommandContext &context)
{
   auto &project = context.project;
   auto tracks = project.GetTracks();
   auto soloSimple = project.IsSoloSimple();
   auto soloNone = project.IsSoloNone();
   auto mixerBoard = project.GetMixerBoard();

   for (auto pt : tracks->Any<PlayableTrack>())
   {
      pt->SetMute(false);
      if (soloSimple || soloNone)
         pt->SetSolo(false);
   }

   project.ModifyState(true);
   project.RedrawProject();
   if (mixerBoard) {
      mixerBoard->UpdateMute();
      if (soloSimple || soloNone)
         mixerBoard->UpdateSolo();
   }
}

void MenuCommandHandler::OnResample(const CommandContext &context)
{
   auto &project = context.project;
   auto projectRate = project.GetRate();
   auto tracks = project.GetTracks();
   auto &undoManager = *project.GetUndoManager();

   int newRate;

   while (true)
   {
      wxDialogWrapper dlg(&project, wxID_ANY, wxString(_("Resample")));
      dlg.SetName(dlg.GetTitle());
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxArrayString rates;
      wxComboBox *cb;

      rate.Printf(wxT("%ld"), lrint(projectRate));

      rates.Add(wxT("8000"));
      rates.Add(wxT("11025"));
      rates.Add(wxT("16000"));
      rates.Add(wxT("22050"));
      rates.Add(wxT("32000"));
      rates.Add(wxT("44100"));
      rates.Add(wxT("48000"));
      rates.Add(wxT("88200"));
      rates.Add(wxT("96000"));
      rates.Add(wxT("176400"));
      rates.Add(wxT("192000"));
      rates.Add(wxT("352800"));
      rates.Add(wxT("384000"));

      S.StartVerticalLay(true);
      {
         S.AddSpace(-1, 15);

         S.StartHorizontalLay(wxCENTER, false);
         {
            cb = S.AddCombo(_("New sample rate (Hz):"),
                            rate,
                            &rates);
         }
         S.EndHorizontalLay();

         S.AddSpace(-1, 15);

         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      dlg.Layout();
      dlg.Fit();
      dlg.Center();

      if (dlg.ShowModal() != wxID_OK)
      {
         return;  // user cancelled dialog
      }

      long lrate;
      if (cb->GetValue().ToLong(&lrate) && lrate >= 1 && lrate <= 1000000)
      {
         newRate = (int)lrate;
         break;
      }

      AudacityMessageBox(_("The entered value is invalid"), _("Error"),
                   wxICON_ERROR, &project);
   }

   int ndx = 0;
   auto flags = UndoPush::AUTOSAVE;
   for (auto wt : tracks->Selected< WaveTrack >())
   {
      wxString msg;

      msg.Printf(_("Resampling track %d"), ++ndx);

      ProgressDialog progress(_("Resample"), msg);

      // The resampling of a track may be stopped by the user.  This might
      // leave a track with multiple clips in a partially resampled state.
      // But the thrown exception will cause rollback in the application
      // level handler.

       wt->Resample(newRate, &progress);

      // Each time a track is successfully, completely resampled,
      // commit that to the undo stack.  The second and later times,
      // consolidate.

      project.PushState(_("Resampled audio track(s)"), _("Resample Track"), flags);
      flags = flags | UndoPush::CONSOLIDATE;
   }

   undoManager.StopConsolidating();
   project.RedrawProject();

   // Need to reset
   project.FinishAutoScroll();
}

void MenuCommandHandler::OnFullScreen(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool bChecked = !project.wxTopLevelWindow::IsFullScreen();
   project.wxTopLevelWindow::ShowFullScreen(bChecked);
   commandManager->Check(wxT("FullScreenOnOff"), bChecked);
}
