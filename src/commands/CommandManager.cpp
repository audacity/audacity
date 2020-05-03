/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.cpp

  Brian Gunlogson
  Dominic Mazzoni

*******************************************************************//**

\class CommandManager
\brief CommandManager implements a system for organizing all user-callable
  commands.

  It creates and manages a menu bar with a command
  associated with each item, and managing other commands callable
  by keyboard shortcuts.

  Commands are implemented by overriding an abstract functor class.
  See Menus.cpp for an example use.

  Menus or submenus containing lists of items can be added at once,
  with a single function (functor) to be called when any of the
  items is selected, with the index number of the selection as the
  parameter.  This is useful for dynamic menus (effects) and
  submenus containing a list of choices (selection formats).

  Menu items can be enabled or disabled individually, groups of
  "multi-items" can be enabled or disabled all at once, or entire
  sets of commands can be enabled or disabled all at once using
  flags.  The flags should be a bitfield stored in a 32-bit
  integer but can be whatever you want.  You specify both the
  desired values of the flags, and the set of flags relevant to
  a particular command, by using a combination of a flags parameter
  and a mask parameter.  Any flag set to 0 in the mask parameter is
  the same as "don't care".  Any command whose mask is set to zero
  will not be affected by enabling/disabling by flags.

*//****************************************************************//**

\class CommandFunctor
\brief CommandFunctor is a very small class that works with
CommandManager.  It holds the callback for one command.

*//****************************************************************//**

\class MenuBarListEntry
\brief MenuBarListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class SubMenuListEntry
\brief SubMenuListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class CommandListEntry
\brief CommandListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class MenuBarList
\brief List of MenuBarListEntry.

*//****************************************************************//**

\class SubMenuList
\brief List of SubMenuListEntry.

*//****************************************************************//**

\class CommandList
\brief List of CommandListEntry.

*//******************************************************************/



#include "CommandManager.h"

#include "CommandContext.h"
#include "CommandManagerWindowClasses.h"

#include <wx/app.h>
#include <wx/defs.h>
#include <wx/evtloop.h>
#include <wx/frame.h>
#include <wx/hash.h>
#include <wx/intl.h>
#include <wx/log.h>

#include "../ActiveProject.h"
#include "../Journal.h"
#include "../JournalOutput.h"
#include "../JournalRegistry.h"
#include "../Menus.h"
#include "Project.h"
#include "ProjectWindows.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/HelpSystem.h"
#include "../widgets/BasicMenu.h"


// On wxGTK, there may be many many many plugins, but the menus don't automatically
// allow for scrolling, so we build sub-menus.  If the menu gets longer than
// MAX_MENU_LEN, we put things in submenus that have MAX_SUBMENU_LEN items in them.
//
#ifdef __WXGTK__
#define MAX_MENU_LEN 20
#define MAX_SUBMENU_LEN 15
#else
#define MAX_MENU_LEN 1000
#define MAX_SUBMENU_LEN 1000
#endif

#define COMMAND XO("Command")


struct MenuBarListEntry
{
   explicit MenuBarListEntry(const wxString &name_);
   MenuBarListEntry( MenuBarListEntry&& ) = default;
   MenuBarListEntry &operator =( MenuBarListEntry&& ) = default;
   ~MenuBarListEntry();

   wxString name;
   BasicMenu::BarHandle menubar{ BasicMenu::FreshMenu };
};

struct SubMenuListEntry
{
   explicit SubMenuListEntry( const BasicMenu::Item::Text &text );
   SubMenuListEntry( SubMenuListEntry&& ) = default;
   ~SubMenuListEntry();

   BasicMenu::Item::Text text;
   BasicMenu::Handle menu{ BasicMenu::FreshMenu };
};

struct CommandListEntry
{
   explicit CommandListEntry( BasicMenu::Handle menu )
      : menu{ menu }
   {}

   int id;
   CommandID name;
   TranslatableString longLabel;
   NormalizedKeyString key;
   NormalizedKeyString defaultKey;
   TranslatableString label;
   TranslatableString labelPrefix;
   TranslatableString labelTop;
   BasicMenu::Handle menu;
   CommandHandlerFinder finder;
   CommandFunctorPointer callback;
   CommandParameter parameter;

   // type of a function that determines checkmark state
   using CheckFn = std::function< bool(AudacityProject&) >;
   CheckFn checkmarkFn;

   bool multi;
   int index;
   int count;
   bool enabled;
   bool skipKeydown;
   bool wantKeyup;
   bool allowDup;
   bool isGlobal;
   bool isOccult;
   bool isEffect;
   bool excludeFromMacros;
   CommandFlag flags;
   bool useStrictFlags{ false };
};

NonKeystrokeInterceptingWindow::~NonKeystrokeInterceptingWindow() = default;

TopLevelKeystrokeHandlingWindow::~TopLevelKeystrokeHandlingWindow() = default;

bool TopLevelKeystrokeHandlingWindow::HandleCommandKeystrokes()
{
   return true;
}

MenuBarListEntry::MenuBarListEntry( const wxString &name_ )
   : name{ name_ }
{
}

MenuBarListEntry::~MenuBarListEntry()
{
}

SubMenuListEntry::SubMenuListEntry( const BasicMenu::Item::Text &text )
   : text(text)
{
}

SubMenuListEntry::~SubMenuListEntry()
{
}

///
static const AudacityProject::AttachedObjects::RegisteredFactory key{
   [](AudacityProject&) {
      return std::make_unique<CommandManager>();
   }
};

CommandManager &CommandManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< CommandManager >( key );
}

const CommandManager &CommandManager::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

///
///  Standard Constructor
///
CommandManager::CommandManager():
   mCurrentID(17000),
   mCurrentMenuText(COMMAND),
   bMakingOccultCommands( false )
   , uCurrentMenu{ std::make_unique<BasicMenu::Handle>() }
{
   mbSeparatorAllowed = false;
   SetMaxList();
   mLastProcessId = 0;
}

///
///  Class Destructor.  Includes PurgeData, which removes
///  menubars
CommandManager::~CommandManager()
{
   //WARNING: This removes menubars that could still be assigned to windows!
   PurgeData();
}

const std::vector<NormalizedKeyString> &CommandManager::ExcludedList()
{
   static const auto list = [] {
      // These short cuts are for the max list only....
      const char *const strings[] = {
         // "Ctrl+I",
         "Ctrl+Alt+I",
         "Ctrl+J",
         "Ctrl+Alt+J",
         "Ctrl+Alt+V",
         "Alt+X",
         "Alt+K",
         "Shift+Alt+X",
         "Shift+Alt+K",
         "Alt+L",
         "Shift+Alt+C",
         "Alt+I",
         "Alt+J",
         "Shift+Alt+J",
         "Ctrl+Shift+A",
         //"Q",
         //"Shift+J",
         //"Shift+K",
         //"Shift+Home",
         //"Shift+End",
         "Ctrl+[",
         "Ctrl+]",
         "1",
         "Shift+F5",
         "Shift+F6",
         "Shift+F7",
         "Shift+F8",
         "Ctrl+Shift+F5",
         "Ctrl+Shift+F7",
         "Ctrl+Shift+N",
         "Ctrl+Shift+M",
         "Ctrl+Home",
         "Ctrl+End",
         "Shift+C",
         "Alt+Shift+Up",
         "Alt+Shift+Down",
         "Shift+P",
         "Alt+Shift+Left",
         "Alt+Shift+Right",
         "Ctrl+Shift+T",
         //"Command+M",
         //"Option+Command+M",
         "Shift+H",
         "Shift+O",
         "Shift+I",
         "Shift+N",
         "D",
         "A",
         "Alt+Shift+F6",
         "Alt+F6",
      };

      std::vector<NormalizedKeyString> result(
         std::begin(strings), std::end(strings)
      );
      std::sort( result.begin(), result.end() );
      return result;
   }();
   return list;
}

// CommandManager needs to know which defaults are standard and which are in the
// full (max) list.
void CommandManager::SetMaxList()
{

   // This list is a DUPLICATE of the list in
   // KeyConfigPrefs::OnImportDefaults(wxCommandEvent & event)

   // TODO: At a later date get rid of the maxList entirely and
   // instead use flags in the menu entries to indicate whether the default
   // shortcut is standard or full.

   mMaxListOnly.clear();

   // if the full list, don't exclude any.
   bool bFull = gPrefs->ReadBool(wxT("/GUI/Shortcuts/FullDefaults"),false);
   if( bFull )
      return;

   mMaxListOnly = ExcludedList();
}


void CommandManager::PurgeData()
{
   // mCommandList contains pointers to CommandListEntrys
   // mMenuBarList contains MenuBarListEntrys.
   // mSubMenuList contains SubMenuListEntrys
   mCommandList.clear();
   mMenuBarList.clear();
   mSubMenuList.clear();

   mCommandNameHash.clear();
   mCommandKeyHash.clear();
   mCommandNumericIDHash.clear();

   mCurrentMenuText = COMMAND;
   mCurrentID = 17000;
}


///
/// Makes a NEW menubar for placement on the top of a project
/// Names it according to the passed-in string argument.
///
/// If the menubar already exists, that's unexpected.
/// Returns an owning handle and keeps only a weak reference
BasicMenu::BarHandle CommandManager::AddMenuBar(const wxString & sMenu)
{
   auto menuBar = GetMenuBar(sMenu);
   if (menuBar) {
      wxASSERT(false);
      return {};
   }

   PushMenuBar( sMenu );
   // Return an owning handle!
   return std::move( mMenuBarList.back().menubar );
}

void CommandManager::PushMenuBar(const wxString & sMenu)
{
   mMenuBarList.emplace_back(sMenu);
}


///
/// Retrieves the menubar based on the name given in PushMenuBar(name)
///
BasicMenu::BarHandle CommandManager::GetMenuBar(const wxString & sMenu) const
{
   for (const auto &entry : mMenuBarList)
   {
      if(entry.name == sMenu)
         return entry.menubar;
   }

   return {};
}


///
/// Retrieve the 'current' menubar; either NULL or the
/// last on in the mMenuBarList.
BasicMenu::BarHandle CommandManager::CurrentMenuBar() const
{
   if(mMenuBarList.empty())
      return {};

   return mMenuBarList.back().menubar;
}

///
/// Typically used to switch back and forth
/// between adding to a hidden menu bar and
/// adding to one that is visible
///
void CommandManager::PopMenuBar()
{
   auto iter = mMenuBarList.end();
   if ( iter != mMenuBarList.begin() )
      mMenuBarList.erase( --iter );
   else
      wxASSERT( false );
}


///
/// This starts a NEW menu
///
BasicMenu::Handle CommandManager::BeginMenu(const BasicMenu::Item::Text & text)
{
   if ( *uCurrentMenu )
      return BeginSubMenu( text );
   else
      return BeginMainMenu( text );
}


///
/// This attaches a menu, if it's main, to the menubar
//  and in all cases ends the menu
///
void CommandManager::EndMenu()
{
   if ( mSubMenuList.empty() )
      EndMainMenu();
   else
      EndSubMenu();
}


///
/// This starts a NEW menu
///
BasicMenu::Handle CommandManager::BeginMainMenu(const BasicMenu::Item::Text & text)
{
   *uCurrentMenu = BasicMenu::FreshMenu;
   mCurrentMenuText = text;
   return *uCurrentMenu;
}


///
/// This attaches a menu to the menubar and ends the menu
///
void CommandManager::EndMainMenu()
{
   // Add the menu to the menubar after all menu items have been
   // added to the menu to allow OSX to rearrange special menu
   // items like Preferences, About, and Quit.
   CurrentMenuBar().Append(
      std::move( *uCurrentMenu ), mCurrentMenuText.label.main );
   *uCurrentMenu = {};
   mCurrentMenuText = COMMAND;
}


///
/// This starts a NEW submenu, and names it according to
/// the function's argument.
BasicMenu::Handle CommandManager::BeginSubMenu(const BasicMenu::Item::Text & text)
{
   mSubMenuList.emplace_back( text );
   mbSeparatorAllowed = false;
   return mSubMenuList.back().menu;
}


///
/// This function is called after the final item of a SUBmenu is added.
/// Submenu items are added just like regular menu items; they just happen
/// after BeginSubMenu() is called but before EndSubMenu() is called.
void CommandManager::EndSubMenu()
{
   //Save the submenu's information
   SubMenuListEntry tmpSubMenu{ std::move( mSubMenuList.back() ) };

   //Pop off the NEW submenu so CurrentMenu returns the parent of the submenu
   mSubMenuList.pop_back();

   //Add the submenu to the current menu
   // PRL:  Use help for commands as well as submenus?
   CurrentMenu().AppendSubMenu(std::move( tmpSubMenu.menu ),
      tmpSubMenu.text );
   mbSeparatorAllowed = true;
}


///
/// This returns the 'Current' Submenu, which is the one at the
///  end of the mSubMenuList (or NULL, if it doesn't exist).
BasicMenu::Handle CommandManager::CurrentSubMenu() const
{
   if(mSubMenuList.empty())
      return {};

   return mSubMenuList.back().menu;
}

///
/// This returns the current menu that we're appending to - note that
/// it could be a submenu if BeginSubMenu was called and we haven't
/// reached EndSubMenu yet.
BasicMenu::Handle CommandManager::CurrentMenu() const
{
   if( ! *uCurrentMenu )
      return {};

   auto tmpCurrentSubMenu = CurrentSubMenu();

   if(!tmpCurrentSubMenu)
   {
      return *uCurrentMenu;
   }

   return tmpCurrentSubMenu;
}

void CommandManager::UpdateCheckmarks( AudacityProject &project )
{
   for ( const auto &entry : mCommandList ) {
      if ( entry->menu && entry->checkmarkFn && !entry->isOccult) {
         entry->menu.SetState( entry->id,
            { true, entry->checkmarkFn( project ) },
            // Mask to update check only not enabled state
            BasicMenu::Item::State::Check );
      }
   }
}

void CommandManager::MenuItemCallback(AudacityProject &project, int ID)
{
   auto flags = MenuManager::Get(project).GetUpdateFlags();
   HandleMenuID(project, ID, flags, false);
}

void CommandManager::AddItem(AudacityProject &project,
                             const CommandID &name,
                             BasicMenu::Item::Text text,
                             CommandHandlerFinder finder,
                             CommandFunctorPointer callback,
                             CommandFlag flags,
                             const Options &options)
{
   if (options.global) {
      //wxASSERT( flags == AlwaysEnabledFlag );
      AddGlobalCommand(
         name, text, finder, callback, options );
      return;
   }

   wxASSERT( flags != NoFlagsSpecified );

   CommandListEntry *entry =
      NewIdentifier(name,
         text,
         CurrentMenu(), finder, callback,
         {}, 0, 0,
         options);
   entry->useStrictFlags = options.useStrictFlags;
   int ID = entry->id;

   // Edit the given label, maybe overwriting the accelerator with the one
   // from preferences
   text.label = FormatLabelWithDisabledAccel(entry);

   SetCommandFlags(name, flags);


   auto &checker = options.checker;
   // PRL:  store a help string?
   const auto action = [this, &project, ID]{ MenuItemCallback(project, ID); };
   if (checker)
      CurrentMenu().AppendCheckItem( text, action,
         { true, checker( project ) }, ID);
   else
      CurrentMenu().Append(text, action, {}, ID);

   mbSeparatorAllowed = true;
}

auto CommandManager::Options::MakeCheckFn(
   const wxString key, bool defaultValue ) -> CheckFn
{
   return [=](AudacityProject&){ return gPrefs->ReadBool( key, defaultValue ); };
}

auto CommandManager::Options::MakeCheckFn(
   const BoolSetting &setting ) -> CheckFn
{
   return MakeCheckFn( setting.GetPath(), setting.GetDefault() );
}

///
/// Add a list of menu items to the current menu.  When the user selects any
/// one of these, the given functor will be called
/// with its position in the list as the index number.
/// When you call Enable on this command name, it will enable or disable
/// all of the items at once.
void CommandManager::AddItemList(AudacityProject &project,
                                 const CommandID & name,
                                 const ComponentInterfaceSymbol items[],
                                 size_t nItems,
                                 CommandHandlerFinder finder,
                                 CommandFunctorPointer callback,
                                 CommandFlag flags,
                                 bool bIsEffect)
{
   for (size_t i = 0, cnt = nItems; i < cnt; i++) {
      CommandListEntry *entry =
         NewIdentifier(name,
            items[i].Msgid(),
            CurrentMenu(),
            finder,
            callback,
            items[i].Internal(),
            i,
            cnt,
            Options{}
               .IsEffect(bIsEffect));
      entry->flags = flags;
      int ID = entry->id;
      const auto action = [this, &project, ID]{ MenuItemCallback(project, ID); };
      CurrentMenu().Append(FormatLabelForMenu(entry), action, {}, ID);
      mbSeparatorAllowed = true;
   }
}

void CommandManager::AddGlobalCommand(const CommandID &name,
                                      const BasicMenu::Item::Text &text,
                                      CommandHandlerFinder finder,
                                      CommandFunctorPointer callback,
                                      const Options &options)
{
   CommandListEntry *entry =
      NewIdentifier(name, text, {}, finder, callback,
                    {}, 0, 0, options);

   entry->enabled = false;
   entry->isGlobal = true;
   entry->flags = AlwaysEnabledFlag;
}

void CommandManager::AddSeparator()
{
   if( mbSeparatorAllowed )
      CurrentMenu().AppendSeparator();
   mbSeparatorAllowed = false; // boolean to prevent too many separators.
}

int CommandManager::NextIdentifier(int ID)
{
   ID++;

   //Skip the reserved identifiers used by wxWidgets
   if((ID >= wxID_LOWEST) && (ID <= wxID_HIGHEST))
      ID = wxID_HIGHEST+1;

   return ID;
}

///Given all of the information for a command, comes up with a NEW unique
///ID, adds it to a list, and returns the ID.
///WARNING: Does this conflict with the identifiers set for controls/windows?
///If it does, a workaround may be to keep controls below wxID_LOWEST
///and keep menus above wxID_HIGHEST
CommandListEntry *CommandManager::NewIdentifier(const CommandID & nameIn,
   const BasicMenu::Item::Text & text,
   BasicMenu::Handle menu,
   CommandHandlerFinder finder,
   CommandFunctorPointer callback,
   const CommandID &nameSuffix,
   int index,
   int count,
   const Options &options)
{
   const auto &label = text.label.main;

   bool excludeFromMacros =
      (options.allowInMacros == 0) ||
      ((options.allowInMacros == -1) && label.MSGID().GET().Contains("..."));

   const wxString & accel = options.accel;
   bool bIsEffect = options.bIsEffect;
   CommandID parameter = options.parameter == "" ? nameIn : options.parameter;

   // if empty, new identifier's long label will be same as label, below:
   const auto &longLabel = options.longName;

   const bool multi = !nameSuffix.empty();
   auto name = nameIn;

   // If we have the identifier already, reuse it.
   CommandListEntry *prev = mCommandNameHash[name];
   if (!prev);
   else if( prev->label != label );
   else if( multi );
   else
      return prev;

   {
      auto entry = std::make_unique<CommandListEntry>( menu );

      TranslatableString labelPrefix;
      if (!mSubMenuList.empty())
         labelPrefix = mSubMenuList.back().text.label.main.Stripped();

      // For key bindings for commands with a list, such as align,
      // the name in prefs is the category name plus the effect name.
      // This feature is not used for built-in effects.
      if (multi)
         name = CommandID{ { name, nameSuffix }, wxT('_') };

      // wxMac 2.5 and higher will do special things with the
      // Preferences, Exit (Quit), and About menu items,
      // if we give them the right IDs.
      // Otherwise we just pick increasing ID numbers for each NEW
      // command.  Note that the name string we are comparing
      // ("About", "Preferences") is the internal command name
      // (untranslated), not the label that actually appears in the
      // menu (which might be translated).

      mCurrentID = NextIdentifier(mCurrentID);
      entry->id = mCurrentID;
      entry->parameter = parameter;

#if defined(__WXMAC__)
      // See bug #2642 for some history as to why these items 
      // on Mac have their IDs set explicitly and not others.
      if (name == wxT("Preferences"))
         entry->id = wxID_PREFERENCES;
      else if (name == wxT("Exit"))
         entry->id = wxID_EXIT;
      else if (name == wxT("About"))
         entry->id = wxID_ABOUT;
#endif

      entry->name = name;
      entry->label = label;

      // long label is the same as label unless options specified otherwise:
      entry->longLabel = longLabel.empty() ? label : longLabel;

      entry->excludeFromMacros = excludeFromMacros;
      entry->key = NormalizedKeyString{ accel.BeforeFirst(wxT('\t')) };
      entry->defaultKey = entry->key;
      entry->labelPrefix = labelPrefix;
      entry->labelTop = mCurrentMenuText.label.main.Stripped();
      entry->finder = finder;
      entry->callback = callback;
      entry->isEffect = bIsEffect;
      entry->multi = multi;
      entry->index = index;
      entry->count = count;
      entry->flags = AlwaysEnabledFlag;
      entry->enabled = true;
      entry->skipKeydown = options.skipKeyDown;
      entry->wantKeyup = options.wantKeyUp || entry->skipKeydown;
      entry->allowDup = options.allowDup;
      entry->isGlobal = false;
      entry->isOccult = bMakingOccultCommands;
      entry->checkmarkFn = options.checker;

      // Exclude accelerators that are in the MaxList.
      // Note that the default is unaffected, intentionally so.
      // There are effectively two levels of default, the full (max) list
      // and the normal reduced list.
      if( std::binary_search( mMaxListOnly.begin(), mMaxListOnly.end(),
                              entry->key ) )
         entry->key = {};

      // Key from preferences overrides the default key given
      gPrefs->SetPath(wxT("/NewKeys"));
      // using GET to interpret CommandID as a config path component
      const auto &path = entry->name.GET();
      if (gPrefs->HasEntry(path)) {
         entry->key =
            NormalizedKeyString{ gPrefs->ReadObject(path, entry->key) };
      }
      gPrefs->SetPath(wxT("/"));

      mCommandList.push_back(std::move(entry));
      // Don't use the variable entry eny more!
   }

   // New variable
   CommandListEntry *entry = &*mCommandList.back();
   mCommandNumericIDHash[entry->id] = entry;

#if defined(_DEBUG)
   prev = mCommandNameHash[entry->name];
   if (prev) {
      // Under Linux it looks as if we may ask for a newID for the same command
      // more than once.  So it's only an error if two different commands
      // have the exact same name.
      if( prev->label != entry->label )
      {
         wxLogDebug(wxT("Command '%s' defined by '%s' and '%s'"),
                    // using GET in a log message for devs' eyes only
                    entry->name.GET(),
                    prev->label.Debug(),
                    entry->label.Debug());
         wxFAIL_MSG(wxString::Format(wxT("Command '%s' defined by '%s' and '%s'"),
                    // using GET in an assertion violation message for devs'
                    // eyes only
                    entry->name.GET(),
                    prev->label.Debug(),
                    entry->label.Debug()));
      }
   }
#endif
   mCommandNameHash[entry->name] = entry;

   if (!entry->key.empty()) {
      mCommandKeyHash[entry->key] = entry;
   }

   return entry;
}

BasicMenu::Item::Label
CommandManager::FormatLabelForMenu(
   const CommandID &id, const TranslatableString *pLabel) const
{
   NormalizedKeyString keyStr;
   if (auto iter = mCommandNameHash.find(id); iter != mCommandNameHash.end()) {
      if (auto pEntry = iter->second) {
         keyStr = pEntry->key;
         if (!pLabel)
            pLabel = &pEntry->label;
      }
   }
   if (pLabel)
      return { *pLabel, keyStr };
   return {};
}

BasicMenu::Item::Label
CommandManager::FormatLabelForMenu(const CommandListEntry *entry) const
{
   return { entry->label, entry->key };
}

// A label that may have its accelerator disabled.
// The problem is that as soon as we show accelerators in the menu, the menu might
// catch them in normal wxWidgets processing, rather than passing the key presses on
// to the controls that had the focus.  We would like all the menu accelerators to be
// disabled, in fact.
BasicMenu::Item::Label
CommandManager::FormatLabelWithDisabledAccel(const CommandListEntry *entry) const
{
   const auto &key = entry->key;
#ifdef __WXMSW__
   if (!key.empty()) {
      for (const auto prefix: {
         "Left",
         "Right",
         "Up",
         "Down",
         "Return",
         "Tab",
         "Shift+Tab",
         "0",
         "1",
         "2",
         "3",
         "4",
         "5",
         "6",
         "7",
         "8",
         "9",

         // "Space",

         // These ones appear to be illegal already and mess up accelerator processing.
         "NUMPAD_ENTER",
         "Backspace",
         "Delete",
      })
         if ( key.GET().StartsWith( prefix ) )
            // Dummy accelerator that looks Ok in menus but is non functional.
            // Note the space before the key.
            return { entry->label, " " + key.GET() };
   }
#endif
   return { entry->label, key };
}
///Enables or disables a menu item based on its name (not the
///label in the menu bar, but the name of the command.)
///If you give it the name of a multi-item (one that was
///added using AddItemList(), it will enable or disable all
///of them at once
void CommandManager::Enable(CommandListEntry *entry, bool enabled)
{
   if (!entry->menu) {
      entry->enabled = enabled;
      return;
   }

   // LL:  Refresh from real state as we can get out of sync on the
   //      Mac due to its reluctance to enable menus when in a modal
   //      state.
   auto state = entry->menu.GetState( entry->id );
   entry->enabled = state.enabled;

   // Only enabled if needed
   if (entry->enabled != enabled) {
      entry->menu.SetState(entry->id, enabled, BasicMenu::Item::State::Enable );
      entry->enabled = entry->menu.GetState(entry->id).enabled;
   }

   if (entry->multi) {
      int i;
      int ID = entry->id;

      for(i=1; i<entry->count; i++) {
         ID = NextIdentifier(ID);

         // This menu item is not necessarily in the same menu, because
         // multi-items can be spread across multiple sub menus
         CommandListEntry *multiEntry = mCommandNumericIDHash[ID];
         if (multiEntry) {
            if ( !multiEntry->menu.SetState(ID, enabled, BasicMenu::Item::State::Enable ) )
               // using GET in a log message for devs' eyes only
               wxLogDebug(L"Warning: Menu entry with id %i in %s not found",
                   ID, entry->name.GET());
         }
         else
            wxLogDebug(L"Warning: Menu entry with id %i not in hash", ID);
      }
   }
}

void CommandManager::Enable(const wxString &name, bool enabled)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry || !entry->menu) {
      wxLogDebug(wxT("Warning: Unknown command enabled: '%s'"),
                 (const wxChar*)name);
      return;
   }

   Enable(entry, enabled);
}

void CommandManager::EnableUsingFlags(
   CommandFlag flags, CommandFlag strictFlags)
{
   // strictFlags are a subset of flags.  strictFlags represent the real
   // conditions now, but flags are the conditions that could be made true.
   // Some commands use strict flags only, refusing the chance to fix
   // conditions
   wxASSERT( (strictFlags & ~flags).none() );

   for(const auto &entry : mCommandList) {
      if (entry->multi && entry->index != 0)
         continue;
      if( entry->isOccult )
         continue;

      auto useFlags = entry->useStrictFlags ? strictFlags : flags;

      if (entry->flags.any()) {
         bool enable = ((useFlags & entry->flags) == entry->flags);
         Enable(entry.get(), enable);
      }
   }
}

bool CommandManager::GetEnabled(const CommandID &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry || !entry->menu) {
      // using GET in a log message for devs' eyes only
      wxLogDebug(wxT("Warning: command doesn't exist: '%s'"),
                 name.GET());
      return false;
   }
   return entry->enabled;
}

int CommandManager::GetNumberOfKeysRead() const
{
   return mXMLKeysRead;
}

void CommandManager::Check(const CommandID &name, bool checked)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry || !entry->menu || entry->isOccult) {
      return;
   }
   entry->menu.SetState(entry->id, { true, checked },
      BasicMenu::Item::State::Check );
}

///Changes the label text of a menu item
void CommandManager::Modify(const wxString &name, const TranslatableString &newLabel)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry && entry->menu) {
      entry->label = newLabel;
      entry->menu.SetLabel(entry->id, FormatLabelForMenu(entry));
   }
}

void CommandManager::SetKeyFromName(const CommandID &name,
                                    const NormalizedKeyString &key)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry) {
      entry->key = key;
   }
}

void CommandManager::SetKeyFromIndex(int i, const NormalizedKeyString &key)
{
   const auto &entry = mCommandList[i];
   entry->key = key;
}

TranslatableString CommandManager::DescribeCommandsAndShortcuts(
   const ComponentInterfaceSymbol commands[], size_t nCommands) const
{
   wxString mark;
   // This depends on the language setting and may change in-session after
   // change of preferences:
   bool rtl = (wxLayout_RightToLeft == wxTheApp->GetLayoutDirection());
   if (rtl)
      mark = wxT("\u200f");

   static const wxString &separatorFormat = wxT("%s / %s");
   TranslatableString result;
   for (size_t ii = 0; ii < nCommands; ++ii) {
      const auto &pair = commands[ii];
      // If RTL, then the control character forces right-to-left sequencing of
      // "/" -separated command names, and puts any "(...)" shortcuts to the
      // left, consistently with accelerators in menus (assuming matching
      // operating system preferences for language), even if the command name
      // was missing from the translation file and defaulted to the English.

      // Note: not putting this and other short format strings in the
      // translation catalogs
      auto piece = Verbatim( wxT("%s%s") )
         .Format( mark, pair.Msgid().Stripped() );

      auto name = pair.Internal();
      if (!name.empty()) {
         auto keyStr = GetKeyFromName(name);
         if (!keyStr.empty()){
            auto keyString = keyStr.Display(true);
            auto format = wxT("%s %s(%s)");
#ifdef __WXMAC__
            // The unicode controls push and pop left-to-right embedding.
            // This keeps the directionally weak characters, such as uparrow
            // for Shift, left of the key name,
            // consistently with how menu accelerators appear, even when the
            // system language is RTL.
            format = wxT("%s %s(\u202a%s\u202c)");
#endif
            // The mark makes correctly placed parentheses for RTL, even
            // in the case that the piece is untranslated.
            piece = Verbatim( format ).Format( piece, mark, keyString );
         }
      }

      if (result.empty())
         result = piece;
      else
         result = Verbatim( separatorFormat ).Format( result, piece );
   }
   return result;
}

///
///
///
bool CommandManager::FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit)
{
   if (!project)
      return false;
   
   auto pWindow = FindProjectFrame( project );
   CommandListEntry *entry = mCommandKeyHash[KeyEventToKeyString(evt)];
   if (entry == NULL)
   {
      return false;
   }

   int type = evt.GetEventType();

   // Global commands aren't tied to any specific project
   if (entry->isGlobal && type == wxEVT_KEY_DOWN)
   {
      // Global commands are always disabled so they do not interfere with the
      // rest of the command handling.  But, to use the common handler, we
      // enable them temporarily and then disable them again after handling.
      // LL:  Why do they need to be disabled???
      entry->enabled = false;
      auto cleanup = valueRestorer( entry->enabled, true );
      return HandleCommandEntry(*project, entry, NoFlagsSpecified, false, &evt);
   }

   wxWindow * pFocus = wxWindow::FindFocus();
   wxWindow * pParent = wxGetTopLevelParent( pFocus );
   bool validTarget = pParent == pWindow;
   // Bug 1557.  MixerBoard should count as 'destined for project'
   // MixerBoard IS a TopLevelWindow, and its parent is the project.
   if( pParent && pParent->GetParent() == pWindow ){
      if(auto keystrokeHandlingWindow = dynamic_cast< TopLevelKeystrokeHandlingWindow* >( pParent ))
         validTarget = keystrokeHandlingWindow->HandleCommandKeystrokes();
   }
   validTarget = validTarget && wxEventLoop::GetActive()->IsMain();

   // Any other keypresses must be destined for this project window
   if (!permit && !validTarget )
   {
      return false;
   }

   auto flags = MenuManager::Get(*project).GetUpdateFlags();

   wxKeyEvent temp = evt;

   // Possibly let wxWidgets do its normal key handling IF it is one of
   // the standard navigation keys.
   if((type == wxEVT_KEY_DOWN) || (type == wxEVT_KEY_UP ))
   {
      wxWindow * pWnd = wxWindow::FindFocus();
      bool bIntercept =
         pWnd && !dynamic_cast< NonKeystrokeInterceptingWindow * >( pWnd );

      //wxLogDebug("Focus: %p TrackPanel: %p", pWnd, pTrackPanel );
      // We allow the keystrokes below to be handled by wxWidgets controls IF we are
      // in some sub window rather than in the TrackPanel itself.
      // Otherwise they will go to our command handler and if it handles them
      // they will NOT be available to wxWidgets.
      if( bIntercept ){
         switch( evt.GetKeyCode() ){
         case WXK_LEFT:
         case WXK_RIGHT:
         case WXK_UP:
         case WXK_DOWN:
         // Don't trap WXK_SPACE (Bug 1727 - SPACE not starting/stopping playback
         // when cursor is in a time control)
         // case WXK_SPACE:
         case WXK_TAB:
         case WXK_BACK:
         case WXK_HOME:
         case WXK_END:
         case WXK_RETURN:
         case WXK_NUMPAD_ENTER:
         case WXK_DELETE:
         case '0':
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
         case '8':
         case '9':
            return false;
         }
      }
   }

   if (type == wxEVT_KEY_DOWN)
   {
      if (entry->skipKeydown)
      {
         return true;
      }
      return HandleCommandEntry(*project, entry, flags, false, &temp);
   }

   if (type == wxEVT_KEY_UP && entry->wantKeyup)
   {
      return HandleCommandEntry(*project, entry, flags, false, &temp);
   }

   return false;
}

namespace {

constexpr auto JournalCode = wxT("CM");  // for CommandManager

// Register a callback for the journal
Journal::RegisteredCommand sCommand{ JournalCode,
[]( const wxArrayStringEx &fields )
{
   // Expect JournalCode and the command name.
   // To do, perhaps, is to include some parameters.
   bool handled = false;
   if ( fields.size() == 2 ) {
      if (auto project = GetActiveProject().lock()) {
         auto pManager = &CommandManager::Get( *project );
         auto flags = MenuManager::Get( *project ).GetUpdateFlags();
         const CommandContext context( *project );
         auto &command = fields[1];
         handled =
            pManager->HandleTextualCommand( command, context, flags, false );
      }
   }
   return handled;
}
};

}

/// HandleCommandEntry() takes a CommandListEntry and executes it
/// returning true iff successful.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleCommandEntry(AudacityProject &project,
   const CommandListEntry * entry,
   CommandFlag flags, bool alwaysEnabled, const wxEvent * evt,
   const CommandContext *pGivenContext)
{
   if (!entry )
      return false;

   if (flags != AlwaysEnabledFlag && !entry->enabled)
      return false;

   if (!alwaysEnabled && entry->flags.any()) {

      const auto NiceName = entry->label.Stripped(
         TranslatableString::Ellipses | TranslatableString::MenuCodes );
      // NB: The call may have the side effect of changing flags.
      bool allowed =
         MenuManager::Get(project).ReportIfActionNotAllowed(
            NiceName, flags, entry->flags );
      // If the function was disallowed, it STILL should count as having been
      // handled (by doing nothing or by telling the user of the problem).
      // Otherwise we may get other handlers having a go at obeying the command.
      if (!allowed)
         return true;
      mNiceName = NiceName;
   }
   else {
      mNiceName = {};
   }

   Journal::Output({ JournalCode, entry->name.GET() });

   CommandContext context{ project, evt, entry->index, entry->parameter };
   if (pGivenContext)
      context.temporarySelection = pGivenContext->temporarySelection;
   // Discriminate the union entry->callback by entry->finder
   if (auto &finder = entry->finder) {
      auto &handler = finder(project);
      (handler.*(entry->callback.memberFn))(context);
   }
   else
      (entry->callback.nonMemberFn)(context);
   mLastProcessId = 0;
   return true;
}

// Called by Contrast and Plot Spectrum Plug-ins to mark them as Last Analzers.
// Note that Repeat data has previously been collected
void CommandManager::RegisterLastAnalyzer(const CommandContext& context) {
   if (mLastProcessId != 0) {
      auto& menuManager = MenuManager::Get(context.project);
      menuManager.mLastAnalyzerRegistration = MenuCreator::repeattypeunique;
      menuManager.mLastAnalyzerRegisteredId = mLastProcessId;
      auto lastEffectDesc = XO("Repeat %s").Format(mNiceName);
      Modify(wxT("RepeatLastAnalyzer"), lastEffectDesc);
   }
   return;
}

// Called by Selected Tools to mark them as Last Tools.
// Note that Repeat data has previously been collected
void CommandManager::RegisterLastTool(const CommandContext& context) {
   if (mLastProcessId != 0) {
      auto& menuManager = MenuManager::Get(context.project);
      menuManager.mLastToolRegistration = MenuCreator::repeattypeunique;
      menuManager.mLastToolRegisteredId = mLastProcessId;
      auto lastEffectDesc = XO("Repeat %s").Format(mNiceName);
      Modify(wxT("RepeatLastTool"), lastEffectDesc);
   }
   return;
}

// Used to invoke Repeat Last Analyzer Process for built-in, non-nyquist plug-ins.
void CommandManager::DoRepeatProcess(const CommandContext& context, int id) {
   mLastProcessId = 0;  //Don't Process this as repeat
   CommandListEntry* entry = mCommandNumericIDHash[id];
   // Discriminate the union entry->callback by entry->finder
   if (auto &finder = entry->finder) {
      auto &handler = finder(context.project);
      (handler.*(entry->callback.memberFn))(context);
   }
   else
      (entry->callback.nonMemberFn)(context);
}


///Call this when a menu event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleMenuID(
   AudacityProject &project, int id, CommandFlag flags, bool alwaysEnabled)
{
   mLastProcessId = id;
   CommandListEntry *entry = mCommandNumericIDHash[id];

   if (GlobalMenuHook::Call(entry->name))
      return true;

   return HandleCommandEntry( project, entry, flags, alwaysEnabled );
}

/// HandleTextualCommand() allows us a limited version of script/batch
/// behavior, since we can get from a string command name to the actual
/// code to run.
CommandManager::TextualCommandResult
CommandManager::HandleTextualCommand(const CommandID & Str,
   const CommandContext & context, CommandFlag flags, bool alwaysEnabled)
{
   if( Str.empty() )
      return CommandFailure;
   // Linear search for now...
   for (const auto &entry : mCommandList)
   {
      if (!entry->multi)
      {
         // Testing against labelPrefix too allows us to call Nyquist functions by name.
         if( Str == entry->name ||
            // PRL:  uh oh, mixing internal string (Str) with user-visible
            // (labelPrefix, which was initialized from a user-visible
            // sub-menu name)
            Str == entry->labelPrefix.Translation() )
         {
            return HandleCommandEntry(
               context.project, entry.get(), flags, alwaysEnabled,
               nullptr, &context)
               ? CommandSuccess : CommandFailure;
         }
      }
      else
      {
         // Handle multis too...
         if( Str == entry->name )
         {
            return HandleCommandEntry(
               context.project, entry.get(), flags, alwaysEnabled,
               nullptr, &context)
               ? CommandSuccess : CommandFailure;
         }
      }
   }
   return CommandNotFound;
}

TranslatableStrings CommandManager::GetCategories( AudacityProject& )
{
   TranslatableStrings cats;

   for (const auto &entry : mCommandList) {
      auto &cat = entry->labelTop;
      if ( ! make_iterator_range( cats ).contains(cat) ) {
         cats.push_back(cat);
      }
   }
#if 0
   mCommandList.size(); i++) {
      if (includeMultis || !mCommandList[i]->multi)
         names.push_back(mCommandList[i]->name);
   }

   if (p == NULL) {
      return;
   }

   auto bar = BasicMenu::BarHandle( GetProjectFrame( *p ) );
   if ( bar )
      for( const auto &item : bar )
         cats.push_back( item.title.Stripped() );

   cats.push_back(COMMAND);
#endif

   return cats;
}

void CommandManager::GetAllCommandNames(CommandIDs &names,
                                        bool includeMultis) const
{
   for(const auto &entry : mCommandList) {
      if ( entry->isEffect )
         continue;
      if (!entry->multi)
         names.push_back(entry->name);
      else if( includeMultis )
         names.push_back(entry->name );// + wxT(":")/*+ mCommandList[i]->label*/);
   }
}

void CommandManager::GetAllCommandLabels(TranslatableStrings &names,
                                         std::vector<bool> &vExcludeFromMacros,
                                        bool includeMultis) const
{
   vExcludeFromMacros.clear();
   for(const auto &entry : mCommandList) {
      // This is fetching commands from the menus, for use as batch commands.
      // Until we have properly merged EffectManager and CommandManager
      // we explicitly exclude effects, as they are already handled by the
      // effects Manager.
      if ( entry->isEffect )
         continue;
      if (!entry->multi)
         names.push_back(entry->longLabel), vExcludeFromMacros.push_back(entry->excludeFromMacros);
      else if( includeMultis )
         names.push_back(entry->longLabel), vExcludeFromMacros.push_back(entry->excludeFromMacros);
   }
}

void CommandManager::GetAllCommandData(
   CommandIDs &names,
   std::vector<NormalizedKeyString> &keys,
   std::vector<NormalizedKeyString> &default_keys,
   TranslatableStrings &labels,
   TranslatableStrings &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
   TranslatableStrings &prefixes,
#endif
   bool includeMultis)
{
   for(const auto &entry : mCommandList) {
      // GetAllCommandData is used by KeyConfigPrefs.
      // It does need the effects.
      //if ( entry->isEffect )
      //   continue;
      if ( !entry->multi || includeMultis )
      {
         names.push_back(entry->name);
         keys.push_back(entry->key);
         default_keys.push_back(entry->defaultKey);
         labels.push_back(entry->label);
         categories.push_back(entry->labelTop);
#if defined(EXPERIMENTAL_KEY_VIEW)
         prefixes.push_back(entry->labelPrefix);
#endif
      }
   }
}

CommandID CommandManager::GetNameFromNumericID(int id)
{
   CommandListEntry *entry = mCommandNumericIDHash[id];
   if (!entry)
      return {};
   return entry->name;
}

TranslatableString CommandManager::GetLabelFromName(const CommandID &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return {};

   return entry->longLabel;
}

TranslatableString CommandManager::GetPrefixedLabelFromName(const CommandID &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return {};

   if (!entry->labelPrefix.empty())
      return Verbatim( wxT("%s - %s") )
         .Format(entry->labelPrefix, entry->label)
            .Stripped();
   else
      return entry->label.Stripped();
}

TranslatableString CommandManager::GetCategoryFromName(const CommandID &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return {};

   return entry->labelTop;
}

NormalizedKeyString CommandManager::GetKeyFromName(const CommandID &name) const
{
   CommandListEntry *entry =
      // May create a NULL entry
      const_cast<CommandManager*>(this)->mCommandNameHash[name];
   if (!entry)
      return {};

   return entry->key;
}

NormalizedKeyString CommandManager::GetDefaultKeyFromName(const CommandID &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return {};

   return entry->defaultKey;
}

bool CommandManager::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == "audacitykeyboard") {
      mXMLKeysRead = 0;
   }

   if (tag == "command") {
      wxString name;
      NormalizedKeyString key;

      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (value.IsStringView())
         {
            const wxString strValue = value.ToWString();

            if (attr == "name")
               name = strValue;
            else if (attr == "key")
               key = NormalizedKeyString{ strValue };
         }
      }

      if (mCommandNameHash[name]) {
         mCommandNameHash[name]->key = key;
         mXMLKeysRead++;
      }
   }

   return true;
}

// This message is displayed now in KeyConfigPrefs::OnImport()
void CommandManager::HandleXMLEndTag(const std::string_view& tag)
{
   /*
   if (tag == "audacitykeyboard") {
      AudacityMessageBox(
         XO("Loaded %d keyboard shortcuts\n")
            .Format( mXMLKeysRead ),
         XO("Loading Keyboard Shortcuts"),
         wxOK | wxCENTRE);
   }
   */
}

XMLTagHandler *CommandManager::HandleXMLChild(const std::string_view&  WXUNUSED(tag))
{
   return this;
}

void CommandManager::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   xmlFile.StartTag(wxT("audacitykeyboard"));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   for(const auto &entry : mCommandList) {

      xmlFile.StartTag(wxT("command"));
      xmlFile.WriteAttr(wxT("name"), entry->name);
      xmlFile.WriteAttr(wxT("key"), entry->key);
      xmlFile.EndTag(wxT("command"));
   }

   xmlFile.EndTag(wxT("audacitykeyboard"));
}

void CommandManager::BeginOccultCommands()
{
   // To do:  perhaps allow occult item switching at lower levels of the
   // menu tree.
   wxASSERT( !CurrentMenu() );

   // Make a temporary menu bar collecting items added after.
   // This bar will be discarded but other side effects on the command
   // manager persist.
   PushMenuBar( L"ext-menu" );
   bMakingOccultCommands = true;
}

void CommandManager::EndOccultCommands()
{
   PopMenuBar();
   bMakingOccultCommands = false;
}

void CommandManager::SetCommandFlags(const CommandID &name,
                                     CommandFlag flags)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry)
      entry->flags = flags;
}

#if defined(_DEBUG)
void CommandManager::CheckDups()
{
   int cnt = mCommandList.size();
   for (size_t j = 0;  (int)j < cnt; j++) {
      if (mCommandList[j]->key.empty()) {
         continue;
      }
      
      if (mCommandList[j]->allowDup)
         continue;

      for (size_t i = 0; (int)i < cnt; i++) {
         if (i == j) {
            continue;
         }

         if (mCommandList[i]->key == mCommandList[j]->key) {
            wxString msg;
            msg.Printf(wxT("key combo '%s' assigned to '%s' and '%s'"),
                       // using GET to form debug message
                       mCommandList[i]->key.GET(),
                       mCommandList[i]->label.Debug(),
                       mCommandList[j]->label.Debug());
            wxASSERT_MSG(mCommandList[i]->key != mCommandList[j]->key, msg);
         }
      }
   }
}

#endif

// If a default shortcut of a command is introduced or changed, then this
// shortcut may be the same shortcut a user has previously assigned to another
// command. This function removes such duplicates by removing the shortcut
// from the command whose default has changed.
// Note that two commands may have the same shortcut if their default shortcuts
// are the same. However, in this function default shortcuts are checked against
// user assigned shortcuts. Two such commands with the same shortcut
// must both be in either the first or the second group, so there is no need
// to test for this case.
// Note that if a user is using the full set of default shortcuts, and one
// of these is changed, then if /GUI/Shortcuts/FullDefaults is not set in audacity.cfg,
// because the defaults appear as user assigned shortcuts in audacity.cfg,
// the previous default overrides the changed default, and no duplicate can
// be introduced.
void CommandManager::RemoveDuplicateShortcuts()
{
   TranslatableString disabledShortcuts;

   for (auto& entry : mCommandList) {
      if (!entry->key.empty() && entry->key != entry->defaultKey) {  // user assigned
         for (auto& entry2 : mCommandList) {
            if (!entry2->key.empty() && entry2->key == entry2->defaultKey) { // default
               if (entry2->key == entry->key) {
                  auto name = wxT("/NewKeys/") + entry2->name.GET();
                  gPrefs->Write(name, NormalizedKeyString{});

                  disabledShortcuts +=
                     XO("\n* %s, because you have assigned the shortcut %s to %s")
                     .Format(entry2->label.Strip(), entry->key.GET(), entry->label.Strip());
               }
            }
         }
      }
   }

   if (!disabledShortcuts.Translation().empty()) {
      TranslatableString message = XO("The following commands have had their shortcuts removed,"
      " because their default shortcut is new or changed, and is the same shortcut"
      " that you have assigned to another command.")
         + disabledShortcuts;
      AudacityMessageBox(message, XO("Shortcuts have been removed"), wxOK | wxCENTRE);

      gPrefs->Flush();
      MenuCreator::RebuildAllMenuBars();
   }
}

#include "../KeyboardCapture.h"

static KeyboardCapture::PreFilter::Scope scope1{
[]( wxKeyEvent & ) {
   // We must have a project since we will be working with the
   // CommandManager, which is tied to individual projects.
   auto project = GetActiveProject().lock();
   return project && GetProjectFrame( *project ).IsEnabled();
} };
static KeyboardCapture::PostFilter::Scope scope2{
[]( wxKeyEvent &key ) {
   // Capture handler window didn't want it, so ask the CommandManager.
   if (auto project = GetActiveProject().lock()) {
      auto &manager = CommandManager::Get( *project );
      return manager.FilterKeyEvent(project.get(), key);
   }
   else
      return false;
} };

