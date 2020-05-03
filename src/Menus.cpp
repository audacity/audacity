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


#include "Menus.h"



#include <wx/frame.h>

#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindows.h"
#include "UndoManager.h"
#include "commands/CommandManager.h"
#include "toolbars/ToolManager.h"
#include "widgets/AudacityMessageBox.h"
#include "BasicUI.h"
#include "widgets/BasicMenu.h"

#include <unordered_set>

#include <wx/windowptr.h>
#include <wx/log.h>

MenuCreator::MenuCreator()
{
   mLastAnalyzerRegistration = repeattypenone;
   mLastToolRegistration = repeattypenone;
   
   mRepeatGeneratorFlags = 0;
   mRepeatEffectFlags = 0;
   mRepeatAnalyzerFlags = 0;
   mRepeatToolFlags = 0;
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
   mUndoSubscription = UndoManager::Get(project)
      .Subscribe(*this, &MenuManager::OnUndoRedo);
}

MenuManager::~MenuManager()
{
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

void MenuVisitor::BeginGroup( Registry::GroupItem &item, const Path &path )
{
   bool isMenu = false;
   bool isExtension = false;
   auto pItem = &item;
   if ( pItem->Transparent() ) {
   }
   else if ( dynamic_cast<MenuTable::MenuSection*>( pItem ) ) {
      if ( !needSeparator.empty() )
         needSeparator.back() = true;
   }
   else if ( auto pWhole = dynamic_cast<MenuTable::WholeMenu*>( pItem ) ) {
      isMenu = true;
      isExtension = pWhole->extension;
      MaybeDoSeparator();
   }

   DoBeginGroup( item, path );

   if ( isMenu ) {
      needSeparator.push_back( false );
      firstItem.push_back( !isExtension );
   }
}

void MenuVisitor::EndGroup( Registry::GroupItem &item, const Path &path )
{
   auto pItem = &item;
   if ( pItem->Transparent() ) {
   }
   else if ( dynamic_cast<MenuTable::MenuSection*>( pItem ) ) {
      if ( !needSeparator.empty() )
         needSeparator.back() = true;
   }
   else if ( dynamic_cast<MenuTable::WholeMenu*>( pItem ) ) {
      firstItem.pop_back();
      needSeparator.pop_back();
   }

   DoEndGroup( item, path );
}

void MenuVisitor::Visit( Registry::SingleItem &item, const Path &path )
{
   MaybeDoSeparator();
   DoVisit( item, path );
}

void MenuVisitor::MaybeDoSeparator()
{
   bool separate = false;
   if ( !needSeparator.empty() ) {
      separate = needSeparator.back() && !firstItem.back();
      needSeparator.back() = false;
      firstItem.back() = false;
   }

   if ( separate )
      DoSeparator();
}

void MenuVisitor::DoBeginGroup( Registry::GroupItem &, const Path & )
{
}

void MenuVisitor::DoEndGroup( Registry::GroupItem &, const Path & )
{
}

void MenuVisitor::DoVisit( Registry::SingleItem &, const Path & )
{
}

void MenuVisitor::DoSeparator()
{
}

namespace MenuTable {

MenuItem::MenuItem( const Identifier &internalName,
   const BasicMenu::Item::Text &text, BaseItemPtrs &&items_ )
: ConcreteGroupItem< false, ToolbarMenuVisitor >{
   internalName, std::move( items_ ) }, text{ text }
{
   wxASSERT( !text.label.main.empty() );
}
MenuItem::~MenuItem() {}

ConditionalGroupItem::ConditionalGroupItem(
   const Identifier &internalName, Condition condition_, BaseItemPtrs &&items_ )
: ConcreteGroupItem< false, ToolbarMenuVisitor >{
   internalName, std::move( items_ ) }, condition{ condition_ }
{
}
ConditionalGroupItem::~ConditionalGroupItem() {}

CommandItem::CommandItem(const CommandID &name_,
         const BasicMenu::Item::Text &text,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         const CommandManager::Options &options_,
         CommandHandlerFinder finder_)
: SingleItem{ name_ }, text{ text }
, finder{ finder_ }, callback{ callback_ }
, flags{ flags_ }, options{ options_ }
{}
CommandItem::~CommandItem() {}

CommandGroupItem::CommandGroupItem(const Identifier &name_,
         std::vector< ComponentInterfaceSymbol > items_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         bool isEffect_,
         CommandHandlerFinder finder_)
: SingleItem{ name_ }, items{ std::move(items_) }
, finder{ finder_ }, callback{ callback_ }
, flags{ flags_ }, isEffect{ isEffect_ }
{}
CommandGroupItem::~CommandGroupItem() {}

SpecialItem::~SpecialItem() {}

MenuSection::~MenuSection() {}
WholeMenu::~WholeMenu() {}

CommandHandlerFinder FinderScope::sFinder =
   [](AudacityProject &project) -> CommandHandlerObject & {
      // If this default finder function is reached, then FinderScope should
      // have been used somewhere but was not, or an explicit
      // CommandHandlerFinder was not passed to menu item constructors
      wxASSERT( false );
      return project;
   };

}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

namespace {

using namespace Registry;

const auto MenuPathStart = wxT("MenuBar");

static Registry::GroupItem &sRegistry()
{
   static Registry::TransparentGroupItem<> registry{ MenuPathStart };
   return registry;
}
}

MenuTable::AttachedItem::AttachedItem(
   const Placement &placement, BaseItemPtr pItem )
{
   Registry::RegisterItem( sRegistry(), placement, std::move( pItem ) );
}

void MenuTable::DestroyRegistry()
{
   sRegistry().items.clear();
}

namespace {

using namespace MenuTable;

struct MenuItemVisitor : ToolbarMenuVisitor
{
   MenuItemVisitor( AudacityProject &proj, CommandManager &man )
      : ToolbarMenuVisitor(proj), manager( man ) {}

   void DoBeginGroup( GroupItem &item, const Path& ) override
   {
      auto pItem = &item;
      if (const auto pMenu =
          dynamic_cast<MenuItem*>( pItem )) {
         manager.BeginMenu( pMenu->text );
      }
      else
      if (const auto pConditionalGroup =
          dynamic_cast<ConditionalGroupItem*>( pItem )) {
         const auto flag = pConditionalGroup->condition();
         if (!flag)
            manager.BeginOccultCommands();
         // to avoid repeated call of condition predicate in EndGroup():
         flags.push_back(flag);
      }
      else
      if ( pItem->Transparent() ) {
      }
      else
      if ( const auto pGroup = dynamic_cast<MenuSection*>( pItem ) ) {
      }
      else
         wxASSERT( false );
   }

   void DoEndGroup( GroupItem &item, const Path& ) override
   {
      auto pItem = &item;
      if (const auto pMenu =
          dynamic_cast<MenuItem*>( pItem )) {
         manager.EndMenu();
      }
      else
      if (const auto pConditionalGroup =
          dynamic_cast<ConditionalGroupItem*>( pItem )) {
         const bool flag = flags.back();
         if (!flag)
            manager.EndOccultCommands();
         flags.pop_back();
      }
      else
      if ( pItem->Transparent() ) {
      }
      else
      if ( const auto pGroup = dynamic_cast<MenuSection*>( pItem ) ) {
      }
      else
         wxASSERT( false );
   }

   void DoVisit( SingleItem &item, const Path& ) override
   {
      const auto pCurrentMenu = manager.CurrentMenu();
      if ( !pCurrentMenu ) {
         // There may have been a mistake in the placement hint that registered
         // this single item.  It's not within any menu.
         wxASSERT( false );
         return;
      }
      auto pItem = &item;
      if (const auto pCommand =
          dynamic_cast<CommandItem*>( pItem )) {
         manager.AddItem( project,
            pCommand->name, pCommand->text,
            pCommand->finder, pCommand->callback,
            pCommand->flags, pCommand->options
         );
      }
      else
      if (const auto pCommandList =
         dynamic_cast<CommandGroupItem*>( pItem ) ) {
         manager.AddItemList(project, pCommandList->name,
            pCommandList->items.data(), pCommandList->items.size(),
            pCommandList->finder, pCommandList->callback,
            pCommandList->flags, pCommandList->isEffect);
      }
      else
      if (const auto pSpecial =
          dynamic_cast<SpecialItem*>( pItem )) {
         wxASSERT( pCurrentMenu );
         pSpecial->fn( project, pCurrentMenu );
      }
      else
         wxASSERT( false );
   }

   void DoSeparator() override
   {
      manager.AddSeparator();
   }

   CommandManager &manager;
   std::vector<bool> flags;
};
}

void MenuCreator::CreateMenusAndCommands(AudacityProject &project)
{
   // Once only, cause initial population of preferences for the ordering
   // of some menu items that used to be given in tables but are now separately
   // registered in several .cpp files; the sequence of registration depends
   // on unspecified accidents of static initialization order across
   // compilation units, so we need something specific here to preserve old
   // default appearance of menus.
   // But this needs only to mention some strings -- there is no compilation or
   // link dependency of this source file on those other implementation files.
   static Registry::OrderingPreferenceInitializer init{
      MenuPathStart,
      {
         {wxT(""), wxT(
   "File,Edit,Select,View,Transport,Tracks,Generate,Effect,Analyze,Tools,Window,Optional,Help"
          )},
         {wxT("/Optional/Extra/Part1"), wxT(
   "Transport,Tools,Mixer,Edit,PlayAtSpeed,Seek,Device,Select"
          )},
         {wxT("/Optional/Extra/Part2"), wxT(
   "Navigation,Focus,Cursor,Track,Scriptables1,Scriptables2"
          )},
         {wxT("/View/Windows"), wxT("UndoHistory,Karaoke,MixerBoard")},
         {wxT("/Analyze/Analyzers/Windows"), wxT("ContrastAnalyser,PlotSpectrum")},
         {wxT("/Transport/Basic"), wxT("Play,Record,Scrubbing,Cursor")},
         {wxT("/View/Other/Toolbars/Toolbars/Other"), wxT(
"ShowTransportTB,ShowToolsTB,ShowRecordMeterTB,ShowPlayMeterTB,"
//"ShowMeterTB,"
"ShowMixerTB,"
"ShowEditTB,ShowTranscriptionTB,ShowScrubbingTB,ShowDeviceTB,ShowSelectionTB,"
"ShowSpectralSelectionTB") },
         {wxT("/Tracks/Add/Add"), wxT(
   "NewMonoTrack,NewStereoTrack,NewLabelTrack,NewTimeTrack")},
      }
   };

   auto &commandManager = CommandManager::Get( project );

   // The list of defaults to exclude depends on
   // preference wxT("/GUI/Shortcuts/FullDefaults"), which may have changed.
   commandManager.SetMaxList();

   auto menubar = commandManager.AddMenuBar(wxT("appmenu"));
   wxASSERT(menubar);

   MenuItemVisitor visitor{ project, commandManager };
   MenuManager::Visit( visitor );

   std::move( menubar ).AttachTo( *ProjectFramePlacement( &project ) );

   mLastFlags = AlwaysEnabledFlag;

#if defined(_DEBUG)
//   c->CheckDups();
#endif
}

void MenuManager::Visit( ToolbarMenuVisitor &visitor )
{
   static const auto menuTree = MenuTable::Items( MenuPathStart );

   wxLogNull nolog;
   Registry::Visit( visitor, menuTree.get(), &sRegistry() );
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
         XXO("&Undo %s")
            .Format( desc ));
      commandManager.Enable(wxT("Undo"),
         ProjectHistory::Get( project ).UndoAvailable());
   }
   else {
      commandManager.Modify(wxT("Undo"),
                            XXO("&Undo"));
   }

   if (undoManager.RedoAvailable()) {
      undoManager.GetShortDescription(cur+1, &desc);
      commandManager.Modify(wxT("Redo"),
         XXO("&Redo %s")
            .Format( desc ));
      commandManager.Enable(wxT("Redo"),
         ProjectHistory::Get( project ).RedoAvailable());
   }
   else {
      commandManager.Modify(wxT("Redo"),
                            XXO("&Redo"));
      commandManager.Enable(wxT("Redo"), false);
   }
}

void MenuCreator::RebuildMenuBar(AudacityProject &project)
{
   // On OSX, we can't rebuild the menus while a modal dialog is being shown
   // since the enabled state for menus like Quit and Preference gets out of
   // sync with wxWidgets idea of what it should be.
#if defined(__WXMAC__) && defined(_DEBUG)
   {
      wxDialog *dlg =
         wxDynamicCast(wxGetTopLevelParent(wxWindow::FindFocus()), wxDialog);
      wxASSERT((!dlg || !dlg->IsModal()));
   }
#endif

   CommandManager::Get( project ).PurgeData();

   CreateMenusAndCommands(project);
}

void MenuManager::OnUndoRedo(UndoRedoMessage message)
{
   switch (message.type) {
   case UndoRedoMessage::UndoOrRedo:
   case UndoRedoMessage::Reset:
   case UndoRedoMessage::Pushed:
   case UndoRedoMessage::Renamed:
      break;
   default:
      return;
   }
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

   auto &settings = ProjectSettings::Get( project );

   // Now, go through each toolbar, and call EnableDisableButtons()
   for (int i = 0; i < ToolBarCount; i++) {
      auto bar = toolManager.GetToolBar(i);
      if (bar)
         bar->EnableDisableButtons();
   }

   // These don't really belong here, but it's easier and especially so for
   // the Edit toolbar and the sync-lock menu item.
   bool active;

   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &active, false);
   settings.SetSyncLock(active);

   CommandManager::Get( project ).UpdateCheckmarks( project );
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
   BasicUI::ShowErrorDialog( {},
      untranslatedTitle,
      reason,
      helpPage);
}
