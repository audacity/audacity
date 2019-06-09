/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et al.

*******************************************************************//**

\file Menus.cpp
\brief Functions for building toobar menus and enabling and disabling items

*//****************************************************************//**

\class MenuCreator
\brief MenuCreator is responsible for creating the main menu bar.

*//****************************************************************//**

\class MenuManager
\brief MenuManager handles updates to menu state.

*//*******************************************************************/

#include "Audacity.h" // for USE_* macros
#include "Menus.h"

#include "Experimental.h"

#include <wx/frame.h>

#include "AdornedRulerPanel.h"
#include "AudioIO.h"
#include "LabelTrack.h"
#include "ModuleManager.h"
#ifdef USE_MIDI
#include "NoteTrack.h"
#endif // USE_MIDI
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "commands/CommandManager.h"
#include "commands/CommandManagerWindowClasses.h"
#include "effects/EffectManager.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/ToolManager.h"

#include <wx/menu.h>
#include <wx/windowptr.h>

MenuCreator::MenuCreator()
{
}

MenuCreator::~MenuCreator()
{
}

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &project ){
     return std::make_shared< MenuManager >( project ); }
};

MenuManager &MenuManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< MenuManager >( key );
}

const MenuManager &MenuManager::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

MenuManager::MenuManager( AudacityProject &project )
   : mProject{ project }
{
   UpdatePrefs();
   mProject.Bind( EVT_UNDO_OR_REDO, &MenuManager::OnUndoRedo, this );
   mProject.Bind( EVT_UNDO_RESET, &MenuManager::OnUndoRedo, this );
   mProject.Bind( EVT_UNDO_PUSHED, &MenuManager::OnUndoRedo, this );
}

MenuManager::~MenuManager()
{
   mProject.Unbind( EVT_UNDO_OR_REDO, &MenuManager::OnUndoRedo, this );
   mProject.Unbind( EVT_UNDO_RESET, &MenuManager::OnUndoRedo, this );
   mProject.Unbind( EVT_UNDO_PUSHED, &MenuManager::OnUndoRedo, this );
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

/// Namespace for structures that go into building a menu
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

CommandItem::CommandItem(const CommandID &name_,
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
         std::initializer_list< ComponentInterfaceSymbol > items_,
         CommandHandlerFinder finder_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         bool isEffect_)
: name{ name_ }, items{ items_ }
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

   auto &manager = CommandManager::Get( project );

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
      const auto pCurrentMenu = manager.CurrentMenu();
      wxASSERT( pCurrentMenu );
      pSpecial->fn( project, *pCurrentMenu );
   }
   else
      wxASSERT( false );
}

}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

MenuTable::BaseItemPtr FileMenu( AudacityProject& );

MenuTable::BaseItemPtr EditMenu( AudacityProject& );

MenuTable::BaseItemPtr SelectMenu( AudacityProject& );

MenuTable::BaseItemPtr ViewMenu( AudacityProject& );

MenuTable::BaseItemPtr TransportMenu( AudacityProject& );

MenuTable::BaseItemPtr TracksMenu( AudacityProject& );

MenuTable::BaseItemPtr GenerateMenu( AudacityProject& );
MenuTable::BaseItemPtr EffectMenu( AudacityProject& );
MenuTable::BaseItemPtr AnalyzeMenu( AudacityProject& );
MenuTable::BaseItemPtr ToolsMenu( AudacityProject& );

MenuTable::BaseItemPtr WindowMenu( AudacityProject& );

MenuTable::BaseItemPtr ExtraMenu( AudacityProject& );

MenuTable::BaseItemPtr HelpMenu( AudacityProject& );

// Table of menu factories.
// TODO:  devise a registration system instead.
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

void MenuCreator::CreateMenusAndCommands(AudacityProject &project)
{
   auto &commandManager = CommandManager::Get( project );

   // The list of defaults to exclude depends on
   // preference wxT("/GUI/Shortcuts/FullDefaults"), which may have changed.
   commandManager.SetMaxList();

   auto menubar = commandManager.AddMenuBar(wxT("appmenu"));
   wxASSERT(menubar);

   VisitItem( project, menuTree.get() );

   GetProjectFrame( project ).SetMenuBar(menubar.release());

   mLastFlags = AlwaysEnabledFlag;

#if defined(__WXDEBUG__)
//   c->CheckDups();
#endif
}

// TODO: This surely belongs in CommandManager?
void MenuManager::ModifyUndoMenuItems(AudacityProject &project)
{
   wxString desc;
   auto &undoManager = UndoManager::Get( project );
   auto &commandManager = CommandManager::Get( project );
   int cur = undoManager.GetCurrentState();

   if (undoManager.UndoAvailable()) {
      undoManager.GetShortDescription(cur, &desc);
      commandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo %s"),
                                              desc));
      commandManager.Enable(wxT("Undo"),
         ProjectHistory::Get( project ).UndoAvailable());
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
      commandManager.Enable(wxT("Redo"),
         ProjectHistory::Get( project ).RedoAvailable());
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
      auto &window = ProjectWindow::Get( project );
      wxWindowPtr<wxMenuBar> menuBar{ window.GetMenuBar() };
      window.DetachMenuBar();
      // menuBar gets deleted here
   }

   CommandManager::Get( project ).PurgeData();

   CreateMenusAndCommands(project);

   ModuleManager::Get().Dispatch(MenusRebuilt);
}

void MenuManager::OnUndoRedo( wxCommandEvent &evt )
{
   evt.Skip();
   ModifyUndoMenuItems( mProject );
   UpdateMenus();
}

// Really means, some track is selected, that isn't a time track
const auto TracksSelectedPred =
   [](const AudacityProject &project){
      auto range = TrackList::Get( project ).Selected()
        - []( const Track *pTrack ){
           return track_cast<const TimeTrack*>( pTrack ); };
      return !range.empty();
   };

const auto AudioIOBusyPred =
   [](const AudacityProject &project ){
      return AudioIOBase::Get()->IsAudioTokenActive(
         ProjectAudioIO::Get( project ).GetAudioIOToken());
   };

const auto TimeSelectedPred =
   [](const AudacityProject &project){
      // This is equivalent to check if there is a valid selection,
      // so it's used for Zoom to Selection too
      return !ViewInfo::Get( project ).selectedRegion.isPoint();
   };

namespace{
   using Predicates = std::vector< ReservedCommandFlag::Predicate >;
   Predicates &RegisteredPredicates()
   {
      static Predicates thePredicates;
      return thePredicates;
   }
   std::vector< CommandFlagOptions > &Options()
   {
      static std::vector< CommandFlagOptions > options;
      return options;
   }
}

ReservedCommandFlag::ReservedCommandFlag(
   const Predicate &predicate, const CommandFlagOptions &options )
{
   static size_t sNextReservedFlag = 0;
   // This will throw std::out_of_range if the constant NCommandFlags is too
   // small
   set( sNextReservedFlag++ );
   RegisteredPredicates().emplace_back( predicate );
   Options().emplace_back( options );
}

const ReservedCommandFlag
   AudioIONotBusyFlag{
      [](const AudacityProject &project ){
         return !AudioIOBusyPred( project );
      },
      CommandFlagOptions{}.QuickTest()
   },
   TimeSelectedFlag{
      TimeSelectedPred
   },
   TracksSelectedFlag{
      TracksSelectedPred
   },
   TracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any().empty();
      }
   },
   LabelTracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any<const LabelTrack>().empty();
      }
   },
   WaveTracksSelectedFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Selected<const WaveTrack>().empty();
      }
   },
   UnsavedChangesFlag{
      [](const AudacityProject &project){
         auto &undoManager = UndoManager::Get( project );
         return
            undoManager.UnsavedChanges()
         ||
            !ProjectFileIO::Get( project ).IsProjectSaved()
         ;
      }
   },
   HasLastEffectFlag{
      [](const AudacityProject &project){
         return !MenuManager::Get( project ).mLastEffect.empty();
      }
   },
   UndoAvailableFlag{
      [](const AudacityProject &project){
         return ProjectHistory::Get( project ).UndoAvailable();
      }
   },
   RedoAvailableFlag{
      [](const AudacityProject &project){
         return ProjectHistory::Get( project ).RedoAvailable();
      }
   },
   ZoomInAvailableFlag{
      [](const AudacityProject &project){
         return
            ViewInfo::Get( project ).ZoomInAvailable()
         &&
            !TrackList::Get( project ).Any().empty()
         ;
      }
   },
   ZoomOutAvailableFlag{
      [](const AudacityProject &project){
         return
            ViewInfo::Get( project ).ZoomOutAvailable()
         &&
            !TrackList::Get( project ).Any().empty()
         ;
      }
   },
   StereoRequiredFlag{
      [](const AudacityProject &project){
         // True iff at least one stereo track is selected, i.e., at least
         // one right channel is selected.
         // TODO: more-than-two-channels
         auto range = TrackList::Get( project ).Selected<const WaveTrack>()
            - &Track::IsLeader;
         return !range.empty();
      }
   },  //lda
   TrackPanelHasFocus{
      [](const AudacityProject &project){
         for (auto w = wxWindow::FindFocus(); w; w = w->GetParent()) {
            if (dynamic_cast<const NonKeystrokeInterceptingWindow*>(w))
               return true;
         }
         return false;
      }
   },  //lll
   LabelsSelectedFlag{
      [](const AudacityProject &project){
         // At least one label track selected, having at least one label
         // completely within the time selection.
         const auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
         const auto &test = [&]( const LabelTrack *pTrack ){
            const auto &labels = pTrack->GetLabels();
            return std::any_of( labels.begin(), labels.end(),
               [&](const LabelStruct &label){
                  return
                     label.getT0() >= selectedRegion.t0()
                  &&
                     label.getT1() <= selectedRegion.t1()
                  ;
               }
            );
         };
         auto range = TrackList::Get( project ).Selected<const LabelTrack>()
            + test;
         return !range.empty();
      }
   },
   AudioIOBusyFlag{
      AudioIOBusyPred,
      CommandFlagOptions{}.QuickTest()
   }, //lll
   PlayRegionLockedFlag{
      [](const AudacityProject &project){
         return ViewInfo::Get(project).playRegion.Locked();
      }
   },  //msmeyer
   PlayRegionNotLockedFlag{
      [](const AudacityProject &project){
         const auto &playRegion = ViewInfo::Get(project).playRegion;
         return !playRegion.Locked() && !playRegion.Empty();
      }
   },  //msmeyer
   CutCopyAvailableFlag{
      [](const AudacityProject &project){
         auto range = TrackList::Get( project ).Any<const LabelTrack>()
            + &LabelTrack::IsTextSelected;
         if ( !range.empty() )
            return true;

         if (
            !AudioIOBusyPred( project )
         &&
            TimeSelectedPred( project )
         &&
            TracksSelectedPred( project )
         )
            return true;

         return false;
      }
   },
   WaveTracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any<const WaveTrack>().empty();
      }
   },
   NoteTracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any<const NoteTrack>().empty();
      }
   },  //gsw
   NoteTracksSelectedFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Selected<const NoteTrack>().empty();
      }
   },  //gsw
   IsNotSyncLockedFlag{
      [](const AudacityProject &project){
         return !ProjectSettings::Get( project ).IsSyncLocked();
      }
   },  //awd
   IsSyncLockedFlag{
      [](const AudacityProject &project){
         return ProjectSettings::Get( project ).IsSyncLocked();
      }
   },  //awd
   IsRealtimeNotActiveFlag{
      [](const AudacityProject &){
         return !EffectManager::Get().RealtimeIsActive();
      }
   },  //lll
   CaptureNotBusyFlag{
      [](const AudacityProject &){
         auto gAudioIO = AudioIO::Get();
         return !(
            gAudioIO->IsBusy() &&
            gAudioIO->GetNumCaptureChannels() > 0
         );
      }
   },
   CanStopAudioStreamFlag{
      [](const AudacityProject &project){
         return ControlToolBar::Get( project ).CanStopAudioStream();
      }
   },
   NotMinimizedFlag{
      [](const AudacityProject &project){
         const wxWindow *focus = FindProjectFrame( &project );
         if (focus) {
            while (focus && focus->GetParent())
               focus = focus->GetParent();
         }
         return (focus &&
            !static_cast<const wxTopLevelWindow*>(focus)->IsIconized()
         );
      },
      CommandFlagOptions{}.QuickTest()
   }, // prl
   PausedFlag{
      [](const AudacityProject&){
         return AudioIOBase::Get()->IsPaused();
      },
      CommandFlagOptions{}.QuickTest()
   },
   HasWaveDataFlag{
      [](const AudacityProject &project){
         auto range = TrackList::Get( project ).Any<const WaveTrack>()
            + [](const WaveTrack *pTrack){
               return pTrack->GetEndTime() > pTrack->GetStartTime();
            };
         return !range.empty();
      }
   }, // jkc
   PlayableTracksExistFlag{
      [](const AudacityProject &project){
         auto &tracks = TrackList::Get( project );
         return
#ifdef EXPERIMENTAL_MIDI_OUT
            !tracks.Any<const NoteTrack>().empty()
         ||
#endif
            !tracks.Any<const WaveTrack>().empty()
         ;
      }
   },
   AudioTracksSelectedFlag{
      [](const AudacityProject &project){
         auto &tracks = TrackList::Get( project );
         return
            !tracks.Selected<const NoteTrack>().empty()
            // even if not EXPERIMENTAL_MIDI_OUT
         ||
            tracks.Selected<const WaveTrack>().empty()
         ;
      }
   },
   NoAutoSelect{
     [](const AudacityProject &){ return true; }
   } // jkc
;

CommandFlag MenuManager::GetUpdateFlags( bool checkActive )
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.

   // static variable, used to remember flags for next time.
   static CommandFlag lastFlags;

   CommandFlag flags, quickFlags;

   const auto &options = Options();
   size_t ii = 0;
   for ( const auto &predicate : RegisteredPredicates() ) {
      if ( options[ii].quickTest ) {
         quickFlags[ii] = true;
         if( predicate( mProject ) )
            flags[ii] = true;
      }
      ++ii;
   }

   if ( checkActive && !GetProjectFrame( mProject ).IsActive() )
      // quick 'short-circuit' return.
      flags = (lastFlags & ~quickFlags) | flags;
   else {
      ii = 0;
      for ( const auto &predicate : RegisteredPredicates() ) {
         if ( !options[ii].quickTest && predicate( mProject ) )
            flags[ii] = true;
         ++ii;
      }
   }

   lastFlags = flags;
   return flags;
}

void MenuManager::ModifyAllProjectToolbarMenus()
{
   for (auto pProject : AllProjects{}) {
      auto &project = *pProject;
      MenuManager::Get(project).ModifyToolbarMenus(project);
   }
}

void MenuManager::ModifyToolbarMenus(AudacityProject &project)
{
   // Refreshes can occur during shutdown and the toolmanager may already
   // be deleted, so protect against it.
   auto &toolManager = ToolManager::Get( project );

   auto &commandManager = CommandManager::Get( project );

   auto &settings = ProjectSettings::Get( project );

   commandManager.Check(wxT("ShowScrubbingTB"),
                         toolManager.IsVisible(ScrubbingBarID));
   commandManager.Check(wxT("ShowDeviceTB"),
                         toolManager.IsVisible(DeviceBarID));
   commandManager.Check(wxT("ShowEditTB"),
                         toolManager.IsVisible(EditBarID));
   commandManager.Check(wxT("ShowMeterTB"),
                         toolManager.IsVisible(MeterBarID));
   commandManager.Check(wxT("ShowRecordMeterTB"),
                         toolManager.IsVisible(RecordMeterBarID));
   commandManager.Check(wxT("ShowPlayMeterTB"),
                         toolManager.IsVisible(PlayMeterBarID));
   commandManager.Check(wxT("ShowMixerTB"),
                         toolManager.IsVisible(MixerBarID));
   commandManager.Check(wxT("ShowSelectionTB"),
                         toolManager.IsVisible(SelectionBarID));
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   commandManager.Check(wxT("ShowSpectralSelectionTB"),
                         toolManager.IsVisible(SpectralSelectionBarID));
#endif
   commandManager.Check(wxT("ShowToolsTB"),
                         toolManager.IsVisible(ToolsBarID));
   commandManager.Check(wxT("ShowTranscriptionTB"),
                         toolManager.IsVisible(TranscriptionBarID));
   commandManager.Check(wxT("ShowTransportTB"),
                         toolManager.IsVisible(TransportBarID));

   // Now, go through each toolbar, and call EnableDisableButtons()
   for (int i = 0; i < ToolBarCount; i++) {
      auto bar = toolManager.GetToolBar(i);
      if (bar)
         bar->EnableDisableButtons();
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
   settings.SetSyncLock(active);

   commandManager.Check(wxT("SyncLock"), active);
   gPrefs->Read(wxT("/GUI/TypeToCreateLabel"),&active, false);
   commandManager.Check(wxT("TypeToCreateLabel"), active);
}

// checkActive is a temporary hack that should be removed as soon as we
// get multiple effect preview working
void MenuManager::UpdateMenus( bool checkActive )
{
   auto &project = mProject;

   //ANSWER-ME: Why UpdateMenus only does active project?
   //JKC: Is this test fixing a bug when multiple projects are open?
   //so that menu states work even when different in different projects?
   if (&project != GetActiveProject())
      return;

   auto flags = GetUpdateFlags(checkActive);
   auto flags2 = flags;

   // We can enable some extra items if we have select-all-on-none.
   //EXPLAIN-ME: Why is this here rather than in GetUpdateFlags()?
   //ANSWER: Because flags2 is used in the menu enable/disable.
   //The effect still needs flags to determine whether it will need
   //to actually do the 'select all' to make the command valid.
   if (mWhatIfNoSelection != 0)
   {
      if ( (flags & TracksExistFlag).any() )
      {
         flags2 |= TracksSelectedFlag;
         if ( (flags & WaveTracksExistFlag).any() )
         {
            flags2 |= TimeSelectedFlag
                   |  WaveTracksSelectedFlag
                   |  CutCopyAvailableFlag;
         }
      }
   }

   if( mStopIfWasPaused )
   {
      if( (flags & PausedFlag).any() ){
         flags2 |= AudioIONotBusyFlag;
      }
   }

   // Return from this function if nothing's changed since
   // the last time we were here.
   if (flags == mLastFlags)
      return;
   mLastFlags = flags;

   auto &commandManager = CommandManager::Get( project );

   // With select-all-on-none, some items that we don't want enabled may have
   // been enabled, since we changed the flags.  Here we manually disable them.
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   commandManager.EnableUsingFlags(
      flags2, // the "lax" flags
      (mWhatIfNoSelection == 0 ? flags2 : flags) // the "strict" flags
   );

   MenuManager::ModifyToolbarMenus(project);
}

/// The following method moves to the previous track
/// selecting and unselecting depending if you are on the start of a
/// block or not.

void MenuCreator::RebuildAllMenuBars()
{
   for( auto p : AllProjects{} ) {
      MenuManager::Get(*p).RebuildMenuBar(*p);
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
      auto &window = GetProjectFrame( *p );
      wxRect r = window.GetRect();
      window.SetSize(wxSize(1,1));
      window.SetSize(r.GetSize());
#endif
   }
}

bool MenuManager::ReportIfActionNotAllowed(
   const wxString & Name, CommandFlag & flags, CommandFlag flagsRqd )
{
   auto &project = mProject;
   bool bAllowed = TryToMakeActionAllowed( flags, flagsRqd, flagsRqd );
   if( bAllowed )
      return true;
   auto &cm = CommandManager::Get( project );
   cm.TellUserWhyDisallowed( Name, flags & flagsRqd, flagsRqd);
   return false;
}


/// Determines if flags for command are compatible with current state.
/// If not, then try some recovery action to make it so.
/// @return whether compatible or not after any actions taken.
bool MenuManager::TryToMakeActionAllowed(
   CommandFlag & flags, CommandFlag flagsRqd, CommandFlag mask )
{
   auto &project = mProject;
   bool bAllowed;

   if( flags.none() )
      flags = GetUpdateFlags();

   bAllowed = ((flags & mask) == (flagsRqd & mask));
   if( bAllowed )
      return true;

   // Why is action not allowed?
   // 1's wherever a required flag is missing.
   auto MissingFlags = (~flags & flagsRqd) & mask;

   if( mStopIfWasPaused && (MissingFlags & AudioIONotBusyFlag ).any() ){
      TransportActions::StopIfPaused( project );
      // Hope this will now reflect stopped audio.
      flags = GetUpdateFlags();
      bAllowed = ((flags & mask) == (flagsRqd & mask));
      if( bAllowed )
         return true;
   }

   //We can only make the action allowed if we select audio when no selection.
   // IF not set up to select all audio when none, THEN return with failure.
   if( mWhatIfNoSelection != 1 )
      return false;

   // Some effects disallow autoselection.
   if( (flagsRqd & NoAutoSelect).any() )
      return false;

   // Why is action still not allowed?
   // 0's wherever a required flag is missing (or is don't care)
   MissingFlags = (flags & ~flagsRqd) & mask;

   // IF selecting all audio won't do any good, THEN return with failure.
   if( (flags & WaveTracksExistFlag).none() )
      return false;
   // returns if mask wants a zero in some flag and that's not present.
   // logic seems a bit peculiar and worth revisiting.
   if( (MissingFlags &
        ~( TimeSelectedFlag | WaveTracksSelectedFlag )
       ).any() )
      return false;

   // This was 'DoSelectSomething()'.  
   // This made autoselect more confusing.
   // When autoselect triggers, it might not select all audio in all tracks.
   // So changed to DoSelectAllAudio.
   SelectActions::DoSelectAllAudio(project);
   flags = GetUpdateFlags();
   bAllowed = ((flags & mask) == (flagsRqd & mask));
   return bAllowed;
}
