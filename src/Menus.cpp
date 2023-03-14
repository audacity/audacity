/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et al.

*******************************************************************//**

\class MenuManager
\brief MenuManager handles updates to menu state.

*//*******************************************************************/
#include "Menus.h"

#include <wx/frame.h>
#include "Project.h"
#include "ProjectWindows.h"
#include "commands/CommandManager.h"
#include "BasicUI.h"
#include <wx/log.h>

static const AudacityProject::AttachedObjects::RegisteredFactory key{
   [](AudacityProject &project){
      return MenuManager::Factory::Call(project);
   }
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

std::pair<bool, bool> MenuTable::detail::VisitorBase::ShouldBeginGroup(
   const MenuItemProperties *pProperties)
{
   const auto properties =
      pProperties ? pProperties->GetProperties() : MenuItemProperties::None;

   bool inlined = false;
   bool shouldDoSeparator = false;

   switch (properties) {
   case MenuItemProperties::Inline: {
      inlined = true;
      break;
   }
   case MenuItemProperties::Section: {
      if (!needSeparator.empty())
         needSeparator.back() = true;
      break;
   }
   case MenuItemProperties::Whole:
   case MenuItemProperties::Extension: {
      shouldDoSeparator = ShouldDoSeparator();
      break;
   }
   default:
      break;
   }

   return { !inlined, shouldDoSeparator };
}

void MenuTable::detail::VisitorBase::AfterBeginGroup(
   const MenuItemProperties *pProperties)
{
   const auto properties =
      pProperties ? pProperties->GetProperties() : MenuItemProperties::None;

   bool isMenu = false;
   bool isExtension = false;

   switch (properties) {
   case MenuItemProperties::Whole:
   case MenuItemProperties::Extension: {
      isMenu = true;
      isExtension = (properties == MenuItemProperties::Extension);
      break;
   }
   default:
      break;
   }

   if (isMenu) {
      needSeparator.push_back(false);
      firstItem.push_back(!isExtension);
   }
}

bool MenuTable::detail::VisitorBase::ShouldEndGroup(
   const MenuItemProperties *pProperties)
{
   const auto properties =
      pProperties ? pProperties->GetProperties() : MenuItemProperties::None;

   bool inlined = false;

   switch (properties) {
   case MenuItemProperties::Inline: {
      inlined = true;
      break;
   }
   case MenuItemProperties::Section: {
      if ( !needSeparator.empty() )
         needSeparator.back() = true;
      break;
   }
   case MenuItemProperties::Whole:
   case MenuItemProperties::Extension: {
      firstItem.pop_back();
      needSeparator.pop_back();
      break;
   }
   default:
      break;
   }

   return !inlined;
}

bool MenuTable::detail::VisitorBase::ShouldDoSeparator()
{
   bool separate = false;
   if (!needSeparator.empty()) {
      separate = needSeparator.back() && !firstItem.back();
      needSeparator.back() = false;
      firstItem.back() = false;
   }
   return separate;
}

namespace MenuTable {

MenuItem::~MenuItem() {}
auto MenuItem::GetProperties() const -> Properties { return Whole; }

ConditionalGroupItem::~ConditionalGroupItem() {}

CommandItem::CommandItem(const CommandID &name_,
         const TranslatableString &label_in_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         const CommandManager::Options &options_,
         CommandHandlerFinder finder_)
: SingleItem{ name_ }, label_in{ label_in_ }
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
MenuPart::~MenuPart() {}
auto MenuPart::GetProperties() const -> Properties { return Section; }

MenuItems::~MenuItems() {}
auto MenuItems::GetOrdering() const -> Ordering {
   return name.empty() ? Anonymous : Weak;
}
auto MenuItems::GetProperties() const -> Properties { return Inline; }

MenuItemProperties::~MenuItemProperties() {}

CommandHandlerFinder FinderScope::sFinder =
   [](AudacityProject &project) -> CommandHandlerObject & {
      // If this default finder function is reached, then FinderScope should
      // have been used somewhere but was not, or an explicit
      // CommandHandlerFinder was not passed to menu item constructors
      wxASSERT( false );
      return project;
   };

}

namespace {

using namespace Registry;

const auto MenuPathStart = wxT("MenuBar");

}

auto MenuTable::ItemRegistry::Registry() -> Registry::GroupItem<Traits> &
{
   static GroupItem<Traits> registry{ MenuPathStart };
   return registry;
}

void MenuManager::Visit(MenuTable::Visitor<MenuTable::Traits> &visitor,
   AudacityProject &project)
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
         {wxT("/Optional/Extra/Part2/Scriptables1"), wxT(
"SelectTime,SelectFrequencies,SelectTracks,SetTrackStatus,SetTrackAudio,"
"SetTrackVisuals,GetPreference,SetPreference,SetClip,SetEnvelope,SetLabel"
"SetProject") },
         {wxT("/Optional/Extra/Part2/Scriptables2"), wxT(
"Select,SetTrack,GetInfo,Message,Help,Import2,Export2,OpenProject2,"
"SaveProject2,Drag,CompareAudio,Screenshot") },
      }
   };

   static const auto menuTree = MenuTable::Items( MenuPathStart );

   wxLogNull nolog;
   Registry::VisitWithFunctions(visitor, menuTree.get(),
      &MenuTable::ItemRegistry::Registry(), project);
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

   const auto &options = ReservedCommandFlag::Options();
   size_t ii = 0;
   for ( const auto &predicate : ReservedCommandFlag::RegisteredPredicates() ) {
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
      for ( const auto &predicate
           : ReservedCommandFlag::RegisteredPredicates() ) {
         if ( !options[ii].quickTest && predicate( mProject ) )
            flags[ii] = true;
         ++ii;
      }
   }

   lastFlags = flags;
   return flags;
}

bool MenuManager::ReportIfActionNotAllowed(
   const TranslatableString & Name, CommandFlag & flags, CommandFlag flagsRqd )
{
   auto &project = mProject;
   bool bAllowed = TryToMakeActionAllowed( flags, flagsRqd );
   if( bAllowed )
      return true;
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
   auto &enablers = RegisteredMenuItemEnabler::Enablers();
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

   const auto &alloptions = ReservedCommandFlag::Options();
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
