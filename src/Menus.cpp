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

#include "ModuleManager.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "UndoManager.h"
#include "commands/CommandManager.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ToolManager.h"
#include "widgets/ErrorDialog.h"

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

MenuItem::MenuItem( const TranslatableString &title_, BaseItemPtrs &&items_ )
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
         const TranslatableString &label_in_,
         CommandHandlerFinder finder_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         const CommandManager::Options &options_)
: name{ name_ }, label_in{ label_in_ }
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
         pCommand->name, pCommand->label_in,
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
   TranslatableString desc;
   auto &undoManager = UndoManager::Get( project );
   auto &commandManager = CommandManager::Get( project );
   int cur = undoManager.GetCurrentState();

   if (undoManager.UndoAvailable()) {
      undoManager.GetShortDescription(cur, &desc);
      commandManager.Modify(wxT("Undo"),
                             XO("&Undo %s").Format( desc ));
      commandManager.Enable(wxT("Undo"),
         ProjectHistory::Get( project ).UndoAvailable());
   }
   else {
      commandManager.Modify(wxT("Undo"),
                            XO("&Undo"));
   }

   if (undoManager.RedoAvailable()) {
      undoManager.GetShortDescription(cur+1, &desc);
      commandManager.Modify(wxT("Redo"),
                             XO("&Redo %s").Format( desc));
      commandManager.Enable(wxT("Redo"),
         ProjectHistory::Get( project ).RedoAvailable());
   }
   else {
      commandManager.Modify(wxT("Redo"),
                            XO("&Redo"));
      commandManager.Enable(wxT("Redo"), false);
   }
}

// Get hackcess to a protected method
class wxFrameEx : public wxFrame
{
public:
   using wxFrame::DetachMenuBar;
};

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
      auto &window = static_cast<wxFrameEx&>( GetProjectFrame( project ) );
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

CommandFlag MenuManager::GetUpdateFlags( bool checkActive ) const
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

namespace
{
   using MenuItemEnablers = std::vector<MenuItemEnabler>;
   MenuItemEnablers &Enablers()
   {
      static MenuItemEnablers enablers;
      return enablers;
   }
}

RegisteredMenuItemEnabler::RegisteredMenuItemEnabler(
   const MenuItemEnabler &enabler )
{
   Enablers().emplace_back( enabler );
}

// checkActive is a temporary hack that should be removed as soon as we
// get multiple effect preview working
void MenuManager::UpdateMenus( bool checkActive )
{
   auto &project = mProject;

   auto flags = GetUpdateFlags(checkActive);
   // Return from this function if nothing's changed since
   // the last time we were here.
   if (flags == mLastFlags)
      return;
   mLastFlags = flags;

   auto flags2 = flags;

   // We can enable some extra items if we have select-all-on-none.
   //EXPLAIN-ME: Why is this here rather than in GetUpdateFlags()?
   //ANSWER: Because flags2 is used in the menu enable/disable.
   //The effect still needs flags to determine whether it will need
   //to actually do the 'select all' to make the command valid.

   for ( const auto &enabler : Enablers() ) {
      auto actual = enabler.actualFlags();
      if (
         enabler.applicable( project ) && (flags & actual) == actual
      )
         flags2 |= enabler.possibleFlags();
   }

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
   const TranslatableString & Name, CommandFlag & flags, CommandFlag flagsRqd )
{
   auto &project = mProject;
   bool bAllowed = TryToMakeActionAllowed( flags, flagsRqd );
   if( bAllowed )
      return true;
   auto &cm = CommandManager::Get( project );
   TellUserWhyDisallowed( Name, flags & flagsRqd, flagsRqd);
   return false;
}

/// Determines if flags for command are compatible with current state.
/// If not, then try some recovery action to make it so.
/// @return whether compatible or not after any actions taken.
bool MenuManager::TryToMakeActionAllowed(
   CommandFlag & flags, CommandFlag flagsRqd )
{
   auto &project = mProject;

   if( flags.none() )
      flags = GetUpdateFlags();

   // Visit the table of recovery actions
   auto &enablers = Enablers();
   auto iter = enablers.begin(), end = enablers.end();
   while ((flags & flagsRqd) != flagsRqd && iter != end) {
      const auto &enabler = *iter;
      auto actual = enabler.actualFlags();
      auto MissingFlags = (~flags & flagsRqd);
      if (
         // Do we have the right precondition?
         (flags & actual) == actual
      &&
         // Can we get the condition we need?
         (MissingFlags & enabler.possibleFlags()).any()
      ) {
         // Then try the function
         enabler.tryEnable( project, flagsRqd );
         flags = GetUpdateFlags();
      }
      ++iter;
   }
   return (flags & flagsRqd) == flagsRqd;
}

void MenuManager::TellUserWhyDisallowed(
   const TranslatableString & Name, CommandFlag flagsGot, CommandFlag flagsRequired )
{
   // The default string for 'reason' is a catch all.  I hope it won't ever be seen
   // and that we will get something more specific.
   auto reason = XO("There was a problem with your last action. If you think\nthis is a bug, please tell us exactly where it occurred.");
   // The default title string is 'Disallowed'.
   auto untranslatedTitle = XO("Disallowed");
   wxString helpPage;

   bool enableDefaultMessage = true;
   bool defaultMessage = true;

   auto doOption = [&](const CommandFlagOptions &options) {
      if ( options.message ) {
         reason = options.message( Name );
         defaultMessage = false;
         if ( !options.title.empty() )
            untranslatedTitle = options.title;
         helpPage = options.helpPage;
         return true;
      }
      else {
         enableDefaultMessage =
            enableDefaultMessage && options.enableDefaultMessage;
         return false;
      }
   };

   const auto &alloptions = Options();
   auto missingFlags = flagsRequired & ~flagsGot;

   // Find greatest priority
   unsigned priority = 0;
   for ( const auto &options : alloptions )
      priority = std::max( priority, options.priority );

   // Visit all unsatisfied conditions' options, by descending priority,
   // stopping when we find a message
   ++priority;
   while( priority-- ) {
      size_t ii = 0;
      for ( const auto &options : alloptions ) {
         if (
            priority == options.priority
         &&
            missingFlags[ii]
         &&
            doOption( options ) )
            goto done;

         ++ii;
      }
   }
   done:

   if (
      // didn't find a message
      defaultMessage
   &&
      // did find a condition that suppresses the default message
      !enableDefaultMessage
   )
      return;

   // Does not have the warning icon...
   ShowErrorDialog(
      NULL,
      untranslatedTitle,
      reason,
      helpPage);
}
