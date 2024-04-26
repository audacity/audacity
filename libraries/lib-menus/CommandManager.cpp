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

\class CommandListEntry
\brief CommandListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class CommandList
\brief List of CommandListEntry.

*//******************************************************************/
#include "CommandManager.h"
#include "CommandContext.h"

#include <wx/log.h>

#include "BasicUI.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "UndoManager.h"

#include <cassert>

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

const TranslatableString CommandManager::COMMAND = XO("Command");

CommandManager::CommandListEntry::~CommandListEntry() = default;

CommandManager::Populator::Populator(AudacityProject &project,
   LeafVisitor leafVisitor,
   std::function<void()> doSeparator
)  : Visitor{
      std::tuple {
         [this](auto &item, auto &){ DoBeginGroup(item); },
         move(leafVisitor),
         [this](auto &item, auto &){ DoEndGroup(item); },
      },
      move(doSeparator)
   }
   , mProject{ project }
{
   // The list of defaults to exclude depends on
   // preference wxT("/GUI/Shortcuts/FullDefaults"), which may have changed.
   SetMaxList();
}

CommandManager::Populator::~Populator() = default;

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  [](AudacityProject &project){
     return CommandManager::Factory::Call(project); }
};

CommandManager &CommandManager::Get(AudacityProject &project)
{
   return project.AttachedObjects::Get<CommandManager>(key);
}

const CommandManager &CommandManager::Get(const AudacityProject &project)
{
   return Get(const_cast<AudacityProject &>(project));
}

///
///  Standard Constructor
///
CommandManager::CommandManager(AudacityProject &project)
   : mProject{ project }
   , mUndoSubscription{ UndoManager::Get(project)
      .Subscribe(*this, &CommandManager::OnUndoRedo) }
{
   mLastProcessId = 0;

   UpdatePrefs();
}

void CommandManager::UpdatePrefs()
{
   bool bSelectAllIfNone;
   gPrefs->Read(wxT("/GUI/SelectAllOnNone"), &bSelectAllIfNone, false);
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   // Audacity autoselects or warns.
   mWhatIfNoSelection = bSelectAllIfNone ? 1 : 2;
}

///
///  Class Destructor.  Includes PurgeData, which removes
///  menubars
CommandManager::~CommandManager()
{
   PurgeData();
}

const std::vector<NormalizedKeyString> &CommandManager::ExcludedList()
{
   static const auto list = [] {
      // These short cuts are for the max list only....
      const char *const strings[] = {
         // "Ctrl+I",
         "Ctrl+Alt+I",
         //"Ctrl+J",
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
void CommandManager::Populator::SetMaxList()
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
   // mCommandList contains unique pointers to CommandListEntrys
   mCommandList.clear();
 
   // Then clear the three hashes of dangling pointers
   mCommandNameHash.clear();
   mCommandKeyHash.clear();
   mCommandNumericIDHash.clear();
}

void CommandManager::Populator::DoBeginGroup(
   const MenuRegistry::GroupItem<MenuRegistry::Traits> &item)
{
   using namespace MenuRegistry;
   auto pItem = &item;
   if (const auto pMenu = dynamic_cast<const MenuItem*>( pItem )) {
      const auto &title = pMenu->GetTitle();
      mMenuNames.emplace_back(title);
      BeginMenu(title);
   }
   else if (const auto pConditionalGroup =
      dynamic_cast<const ConditionalGroupItem*>( pItem )
   ) {
      const auto flag = (*pConditionalGroup)();
      if (!flag) {
         bMakingOccultCommands = true;
         BeginOccultCommands();
      }
      // to avoid repeated call of condition predicate in EndGroup():
      mFlags.push_back(flag);
   }
   else
      assert(IsSection(item));
}

void CommandManager::Populator::DoVisit(const Registry::SingleItem &item)
{
   using namespace MenuRegistry;
   auto pItem = &item;
   if (const auto pCommand = dynamic_cast<const CommandItem*>(pItem)) {
      auto &options = pCommand->options;
      AddItem(
         pCommand->name, pCommand->label_in,
         pCommand->finder, pCommand->callback,
         pCommand->flags, options);
   }
   else
   if (const auto pCommandList = dynamic_cast<const CommandGroupItem*>(pItem)) {
      AddItemList(pCommandList->name,
         pCommandList->items.data(), pCommandList->items.size(),
         pCommandList->finder, pCommandList->callback,
         pCommandList->flags, pCommandList->isEffect);
   }
   else
      wxASSERT( false );
}

void CommandManager::Populator::BeginMenu(const TranslatableString &)
{
}

void CommandManager::Populator::DoEndGroup(
   const MenuRegistry::GroupItem<MenuRegistry::Traits> &item)
{
   using namespace MenuRegistry;
   auto pItem = &item;
   if (const auto pMenu = dynamic_cast<const MenuItem*>(pItem)) {
      EndMenu();
      mMenuNames.pop_back();
   }
   else
   if (const auto pConditionalGroup =
      dynamic_cast<const ConditionalGroupItem*>(pItem)
   ) {
      const bool flag = mFlags.back();
      if (!flag) {
         EndOccultCommands();
         bMakingOccultCommands = false;
      }
      mFlags.pop_back();
   }
   else
      assert(IsSection(item));
}

void CommandManager::Populator::EndMenu()
{
}

auto CommandManager::Populator::AllocateEntry(const MenuRegistry::Options &)
   -> std::unique_ptr<CommandListEntry>
{
   return std::make_unique<CommandListEntry>();
}

void CommandManager::Populator::VisitEntry(CommandListEntry &,
   const MenuRegistry::Options *)
{
}

void CommandManager::UpdateCheckmarks()
{
   for (const auto &entry : mCommandList)
      entry->UpdateCheckmark(mProject);
}

void CommandManager::CommandListEntry::UpdateCheckmark(AudacityProject &)
{
}

void CommandManager::Populator::AddItem(const CommandID &name,
                             const TranslatableString &label_in,
                             CommandHandlerFinder finder,
                             CommandFunctorPointer callback,
                             CommandFlag flags,
                             const MenuRegistry::Options &options)
{
   if (options.global) {
      //wxASSERT( flags == AlwaysEnabledFlag );
      AddGlobalCommand(
         name, label_in, finder, callback, options );
      return;
   }

   wxASSERT( flags != NoFlagsSpecified );

   CommandListEntry *entry =
      NewIdentifier(name,
         label_in,
         finder, callback,
         {}, 0, 0,
         options);
   entry->useStrictFlags = options.useStrictFlags;
   Get(mProject).SetCommandFlags(name, flags);
   mbSeparatorAllowed = true;
   VisitEntry(*entry, &options);
}

///
/// Add a list of menu items to the current menu.  When the user selects any
/// one of these, the given functor will be called
/// with its position in the list as the index number.
/// When you call Enable on this command name, it will enable or disable
/// all of the items at once.
void CommandManager::Populator::AddItemList(const CommandID & name,
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
            finder,
            callback,
            items[i].Internal(),
            i,
            cnt,
            MenuRegistry::Options{}
               .IsEffect(bIsEffect));
      entry->flags = flags;
      mbSeparatorAllowed = true;
      VisitEntry(*entry, nullptr);
   }
}

void CommandManager::Populator::AddGlobalCommand(const CommandID &name,
                                      const TranslatableString &label_in,
                                      CommandHandlerFinder finder,
                                      CommandFunctorPointer callback,
                                      const MenuRegistry::Options &options)
{
   CommandListEntry *entry =
      NewIdentifier(name, label_in, finder, callback,
                    {}, 0, 0, options);

   entry->enabled = false;
   entry->isGlobal = true;
   entry->flags = AlwaysEnabledFlag;
   VisitEntry(*entry, &options);
}

void CommandManager::Populator::DoSeparator()
{
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
auto CommandManager::Populator::NewIdentifier(const CommandID & nameIn,
   const TranslatableString & label,
   CommandHandlerFinder finder,
   CommandFunctorPointer callback,
   const CommandID &nameSuffix,
   int index,
   int count,
   const MenuRegistry::Options &options)
   -> CommandListEntry*
{
   auto &cm = Get(mProject);

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
   CommandListEntry *prev = cm.mCommandNameHash[name];
   if (prev && prev->label == label && !multi)
      return prev;

   {
      auto entry = AllocateEntry(options);
      assert(entry);

      TranslatableString labelPrefix;
      if (MenuNames().size() > 1)
         // submenus only, not main
         labelPrefix = MenuNames().back().Stripped();

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
      entry->labelTop = MenuNames()[0].Stripped();
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
      {
         entry->key = {};
      }
      auto newKeysGroup = gPrefs->BeginGroup("/NewKeys");
      // using GET to interpret CommandID as a config path component
      const auto &path = entry->name.GET();
      if (gPrefs->HasEntry(path)) {
         // Key from preferences overrides the default key given
         entry->key =
            NormalizedKeyString{ gPrefs->Read(path, entry->key) };
      }

      cm.mCommandList.push_back(std::move(entry));
      // Don't use the variable entry eny more!
   }

   // New variable
   CommandListEntry *entry = &*cm.mCommandList.back();
   cm.mCommandNumericIDHash[entry->id] = entry;

#if defined(_DEBUG)
   prev = cm.mCommandNameHash[entry->name];
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
   cm.mCommandNameHash[entry->name] = entry;

   if (!entry->key.empty()) {
      cm.mCommandKeyHash[entry->key] = entry;
   }

   return entry;
}

wxString CommandManager::FormatLabelForMenu(
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
      return CommandListEntry::FormatLabelForMenu(*pLabel, keyStr);
   return {};
}

wxString CommandManager::CommandListEntry::FormatLabelForMenu(
   const TranslatableString &translatableLabel,
   const NormalizedKeyString &keyStr)
{
   auto label = translatableLabel.Translation();
   auto key = keyStr.GET();
   if (!key.empty())
   {
      // using GET to compose menu item name for wxWidgets
      label += wxT("\t") + key;
   }

   return label;
}

///Enables or disables a menu item based on its name (not the
///label in the menu bar, but the name of the command.)
///If you give it the name of a multi-item (one that was
///added using AddItemList(), it will enable or disable all
///of them at once
void CommandManager::Enable(CommandListEntry &entry, bool enabled)
{
   entry.Enable(enabled);
   if (entry.multi) {
      for (int i = 1, ID = entry.id;
         i < entry.count;
         ++i, ID = NextIdentifier(ID)
      ) {
         // This menu item is not necessarily in the same menu, because
         // multi-items can be spread across multiple sub menus
         if (auto iter = mCommandNumericIDHash.find(ID);
             iter != mCommandNumericIDHash.end())
            iter->second->EnableMultiItem(enabled);
         else
            wxLogDebug(wxT("Warning: Menu entry with id %i not in hash"), ID);
      }
   }
}

void CommandManager::CommandListEntry::Enable(bool b)
{
   enabled = b;
}

void CommandManager::CommandListEntry::EnableMultiItem(bool b)
{
   enabled = b;
}

void CommandManager::Enable(const wxString &name, bool enabled)
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      Enable(*iter->second, enabled);
   else
      wxLogDebug(wxT("Warning: Unknown command enabled: '%s'"),
                 (const wxChar*)name);
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
         Enable(*entry, enable);
      }
   }
}

bool CommandManager::GetEnabled(const CommandID &name) const
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      return iter->second->GetEnabled();
   else {
      // using GET in a log message for devs' eyes only
      wxLogDebug(wxT("Warning: command doesn't exist: '%s'"),
                 name.GET());
      return false;
   }
}

bool CommandManager::CommandListEntry::GetEnabled() const
{
   return enabled;
}

int CommandManager::GetNumberOfKeysRead() const
{
   return mXMLKeysRead;
}

void CommandManager::Check(const CommandID &name, bool checked)
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      iter->second->Check(checked);
}

void CommandManager::CommandListEntry::Check(bool)
{
}

///Changes the label text of a menu item
void CommandManager::Modify(const wxString &name, const TranslatableString &newLabel)
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      iter->second->Modify(newLabel);
}

void CommandManager::CommandListEntry::Modify(const TranslatableString &newLabel)
{
   label = newLabel;
}


void CommandManager::SetKeyFromName(const CommandID &name,
                                    const NormalizedKeyString &key)
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      iter->second->key = key;
}

void CommandManager::SetKeyFromIndex(int i, const NormalizedKeyString &key)
{
   if (!(0 <= i && i < NCommands())) {
      assert(false);
      return;
   }
   const auto &entry = mCommandList[i];
   entry->key = key;
}

TranslatableString CommandManager::DescribeCommandsAndShortcuts(
   const ComponentInterfaceSymbol commands[], size_t nCommands) const
{
   wxString mark;
   // This depends on the language setting and may change in-session after
   // change of preferences:
   if (BasicUI::IsUsingRtlLayout())
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

/// HandleCommandEntry() takes a CommandListEntry and executes it
/// returning true iff successful.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleCommandEntry(const CommandListEntry * entry,
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
      bool allowed = ReportIfActionNotAllowed(NiceName, flags, entry->flags);
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

   CommandContext context{ mProject, evt, entry->index, entry->parameter };
   if (pGivenContext)
      context.temporarySelection = pGivenContext->temporarySelection;
   ExecuteCommand(context, evt, *entry);
   return true;
}

void CommandManager::ExecuteCommand(const CommandContext &context,
   const wxEvent *evt, const CommandListEntry &entry)
{
   // Discriminate the union entry->callback by entry->finder
   if (auto &finder = entry.finder) {
      auto &handler = finder(mProject);
      (handler.*(entry.callback.memberFn))(context);
   }
   else
      (entry.callback.nonMemberFn)(context);
   mLastProcessId = 0;
}

// Called by Contrast and Plot Spectrum Plug-ins to mark them as Last Analzers.
// Note that Repeat data has previously been collected
void CommandManager::RegisterLastAnalyzer(const CommandContext& context) {
   if (mLastProcessId != 0) {
      mLastAnalyzerRegistration = repeattypeunique;
      mLastAnalyzerRegisteredId = mLastProcessId;
      auto lastEffectDesc = XO("Repeat %s").Format(mNiceName);
      Modify(wxT("RepeatLastAnalyzer"), lastEffectDesc);
   }
   return;
}

// Called by Selected Tools to mark them as Last Tools.
// Note that Repeat data has previously been collected
void CommandManager::RegisterLastTool(const CommandContext& context) {
   if (mLastProcessId != 0) {
      mLastToolRegistration = repeattypeunique;
      mLastToolRegisteredId = mLastProcessId;
      auto lastEffectDesc = XO("Repeat %s").Format(mNiceName);
      Modify(wxT("RepeatLastTool"), lastEffectDesc);
   }
   return;
}

// Used to invoke Repeat Last Analyzer Process for built-in, non-nyquist plug-ins.
void CommandManager::DoRepeatProcess(const CommandContext& context, int id)
{
   mLastProcessId = 0;  //Don't Process this as repeat
   if (auto iter = mCommandNumericIDHash.find(id);
      iter != mCommandNumericIDHash.end()
   ) {
      const auto entry = iter->second;
      // Discriminate the union entry->callback by entry->finder
      if (auto &finder = entry->finder) {
         auto &handler = finder(context.project);
         (handler.*(entry->callback.memberFn))(context);
      }
      else
         (entry->callback.nonMemberFn)(context);
   }
}


///Call this when a menu event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleMenuID(
   int id, CommandFlag flags, bool alwaysEnabled)
{
   mLastProcessId = id;
   if (auto iter = mCommandNumericIDHash.find(id);
      iter != mCommandNumericIDHash.end()
   ) {
      const auto entry = iter->second;
      if (GlobalMenuHook::Call(entry->name))
         return true;

      return HandleCommandEntry(entry, flags, alwaysEnabled);
   }
   return false;
}

/// HandleTextualCommand() allows us a limited version of script/batch
/// behavior, since we can get from a string command name to the actual
/// code to run.
CommandManager::TextualCommandResult
CommandManager::HandleTextualCommand(const CommandID & Str,
   const CommandContext & context, CommandFlag flags, bool alwaysEnabled)
{
   assert(&context.project == &GetProject());
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
               entry.get(), flags, alwaysEnabled,
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
               entry.get(), flags, alwaysEnabled,
               nullptr, &context)
               ? CommandSuccess : CommandFailure;
         }
      }
   }
   return CommandNotFound;
}

TranslatableStrings CommandManager::GetCategories()
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

   wxMenuBar *bar = p->GetMenuBar();
   size_t cnt = bar->GetMenuCount();
   for (size_t i = 0; i < cnt; i++) {
      cats.push_back(bar->GetMenuLabelText(i));
   }

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
   TranslatableStrings &prefixes,
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
         prefixes.push_back(entry->labelPrefix);
      }
   }
}

CommandID CommandManager::GetNameFromNumericID(int id) const
{
   if (auto iter = mCommandNumericIDHash.find(id);
      iter != mCommandNumericIDHash.end())
      return iter->second->name;
   return {};
}

TranslatableString CommandManager::GetLabelFromName(const CommandID &name) const
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      return iter->second->longLabel;
   return {};
}

TranslatableString
CommandManager::GetPrefixedLabelFromName(const CommandID &name) const
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end()
   ) {
      const auto entry = iter->second;
      if (!entry->labelPrefix.empty())
         return Verbatim( wxT("%s - %s") )
            .Format(entry->labelPrefix, entry->label)
               .Stripped();
      else
         return entry->label.Stripped();
   }
   return {};
}

TranslatableString
CommandManager::GetCategoryFromName(const CommandID &name) const
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      return iter->second->labelTop;
   return {};
}

NormalizedKeyString CommandManager::GetKeyFromName(const CommandID &name) const
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      return iter->second->key;
   return {};
}

NormalizedKeyString CommandManager::GetDefaultKeyFromName(const CommandID &name)
const
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      return iter->second->defaultKey;
   return {};
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

      if (auto iter = mCommandNameHash.find(name);
         iter != mCommandNameHash.end()
      ) {
         iter->second->key = key;
         ++mXMLKeysRead;
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

void CommandManager::Populator::BeginOccultCommands()
{
}

void CommandManager::Populator::EndOccultCommands()
{
}

void CommandManager::SetCommandFlags(const CommandID &name,
                                     CommandFlag flags)
{
   if (auto iter = mCommandNameHash.find(name);
      iter != mCommandNameHash.end())
      iter->second->flags = flags;
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
TranslatableString CommandManager::ReportDuplicateShortcuts()
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

   return disabledShortcuts;
}

CommandFlag CommandManager::GetUpdateFlags(bool quick) const
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

   if (quick)
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

bool CommandManager::ReportIfActionNotAllowed(
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
bool CommandManager::TryToMakeActionAllowed(
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

void CommandManager::TellUserWhyDisallowed(
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

void CommandManager::ModifyUndoMenuItems()
{
   auto &project = mProject;
   TranslatableString desc;
   auto &undoManager = UndoManager::Get( project );
   int cur = undoManager.GetCurrentState();

   if (undoManager.UndoAvailable()) {
      undoManager.GetShortDescription(cur, &desc);
      Modify(wxT("Undo"), XXO("&Undo %s").Format(desc));
      Enable(wxT("Undo"), ProjectHistory::Get(project).UndoAvailable());
   }
   else {
      Modify(wxT("Undo"), XXO("&Undo"));
   }

   if (undoManager.RedoAvailable()) {
      undoManager.GetShortDescription(cur+1, &desc);
      Modify(wxT("Redo"), XXO("&Redo %s").Format( desc ));
      Enable(wxT("Redo"), ProjectHistory::Get(project).RedoAvailable());
   }
   else {
      Modify(wxT("Redo"), XXO("&Redo"));
      Enable(wxT("Redo"), false);
   }
}

void CommandManager::OnUndoRedo(UndoRedoMessage message)
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
   ModifyUndoMenuItems();
   UpdateMenus();
}

// checkActive is a temporary hack that should be removed as soon as we
// get multiple effect preview working
void CommandManager::UpdateMenus(bool checkActive)
{
   auto &project = mProject;

   bool quick = checkActive && ReallyDoQuickCheck();
   auto flags = GetUpdateFlags(quick);
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

   for ( const auto &enabler : RegisteredMenuItemEnabler::Enablers() ) {
      auto actual = enabler.actualFlags();
      if (
         enabler.applicable( project ) && (flags & actual) == actual
      )
         flags2 |= enabler.possibleFlags();
   }

   // With select-all-on-none, some items that we don't want enabled may have
   // been enabled, since we changed the flags.  Here we manually disable them.
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   EnableUsingFlags(
      flags2, // the "lax" flags
      (mWhatIfNoSelection == 0 ? flags2 : flags) // the "strict" flags
   );

   Publish({});
}

bool CommandManager::ReallyDoQuickCheck()
{
   return true;
}
