/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include "Identifier.h"

#include "Callable.h"
#include "ClientData.h"
#include "CommandFunctors.h"
#include "CommandFlag.h"
#include "GlobalVariable.h"

#include "Keyboard.h"
#include "MenuRegistry.h"

#include "Observer.h"
#include "Prefs.h"
#include "Project.h"

#include <functional>
#include <vector>

#include "XMLTagHandler.h"

#include <unordered_map>

class wxEvent;
class wxMenu;
class wxMenuBar;

class BoolSetting;

struct MenuBarListEntry;
using PluginID = wxString;
struct SubMenuListEntry;

using MenuBarList = std::vector < MenuBarListEntry >;
using SubMenuList = std::vector < SubMenuListEntry >;

class AudacityProject;
class CommandContext;

//! Sent when menus update (such as for changing enablement of items)
struct MenuUpdateMessage {};

class AUDACITY_DLL_API CommandManager /* not final */
   : public XMLTagHandler
   , public ClientData::Base
   , public Observer::Publisher<MenuUpdateMessage>
   , private PrefsListener
{
public:
   struct Factory : DefaultedGlobalHook<Factory,
      Callable::SharedPtrFactory<CommandManager, AudacityProject &>::Function
   >{};

   static CommandManager &Get( AudacityProject &project );
   static const CommandManager &Get( const AudacityProject &project );

   // Interception of menu item handling.
   // If it returns true, bypass the usual dispatch of commands.
   struct AUDACITY_DLL_API GlobalMenuHook : GlobalHook<GlobalMenuHook,
      bool(const CommandID&)
   >{};

   explicit CommandManager(AudacityProject &project);
   CommandManager(const CommandManager &) = delete;
   CommandManager &operator=(const CommandManager &) = delete;
   ~CommandManager() override;

   // If quick, skip some needlessly expensive checks.
   CommandFlag GetUpdateFlags(bool quick = false) const;
   void UpdatePrefs() override;

   // Command Handling
   bool ReportIfActionNotAllowed(
      const TranslatableString & Name, CommandFlag & flags, CommandFlag flagsRqd );
   bool TryToMakeActionAllowed(
      CommandFlag & flags, CommandFlag flagsRqd );

   CommandFlag mLastFlags = AlwaysEnabledFlag;
   
   // Last effect applied to this project
   PluginID mLastGenerator{};
   PluginID mLastEffect{};
   PluginID mLastAnalyzer{};
   int mLastAnalyzerRegistration = repeattypenone;
   int mLastAnalyzerRegisteredId = -1;
   PluginID mLastTool{};
   int mLastToolRegistration = repeattypenone;
   int mLastToolRegisteredId = -1;
   enum {
      repeattypenone = 0,
      repeattypeplugin = 1,
      repeattypeunique = 2,
      repeattypeapplymacro = 3
   };
   unsigned mRepeatGeneratorFlags = 0;
   unsigned mRepeatEffectFlags = 0;
   unsigned mRepeatAnalyzerFlags = 0;
   unsigned mRepeatToolFlags = 0;

   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   int  mWhatIfNoSelection;

   // not configurable for now, but could be later.
   bool mStopIfWasPaused{ true };

   AudacityProject &GetProject() { return mProject; }

   void SetMaxList();
   void PurgeData();

   //
   // Creating menus and adding commands
   //

   std::unique_ptr<wxMenuBar> AddMenuBar(const wxString & sMenu);

   wxMenu *BeginMenu(const TranslatableString & tName);
   void EndMenu();

public:
   void AddItemList(const CommandID & name,
                    const ComponentInterfaceSymbol items[],
                    size_t nItems,
                    CommandHandlerFinder finder,
                    CommandFunctorPointer callback,
                    CommandFlag flags,
                    bool bIsEffect = false);

   void AddItem(const CommandID & name,
                const TranslatableString &label_in,
                CommandHandlerFinder finder,
                CommandFunctorPointer callback,
                CommandFlag flags,
                const MenuRegistry::Options &options = {});

   void AddSeparator();

   void PopMenuBar();
   void BeginOccultCommands();
   void EndOccultCommands();


   void SetCommandFlags(const CommandID &name, CommandFlag flags);

   //
   // Modifying menus
   //

   void EnableUsingFlags(
      CommandFlag flags, CommandFlag strictFlags);
   void Enable(const wxString &name, bool enabled);
   void Check(const CommandID &name, bool checked);
   void Modify(const wxString &name, const TranslatableString &newLabel);

   //
   // Modifying accelerators
   //

   void SetKeyFromName(const CommandID &name, const NormalizedKeyString &key);
   void SetKeyFromIndex(int i, const NormalizedKeyString &key);

   //
   // Executing commands
   //

   bool HandleMenuID(int id, CommandFlag flags, bool alwaysEnabled);
   void RegisterLastAnalyzer(const CommandContext& context);
   void RegisterLastTool(const CommandContext& context);
   void DoRepeatProcess(const CommandContext& context, int);

   enum TextualCommandResult {
      CommandFailure,
      CommandSuccess,
      CommandNotFound
   };

   /*!
    @pre `&context.project == &GetProject()`
    */
   TextualCommandResult
   HandleTextualCommand(const CommandID & Str,
      const CommandContext & context, CommandFlag flags, bool alwaysEnabled);

   //
   // Accessing
   //

   TranslatableStrings GetCategories();
   void GetAllCommandNames(CommandIDs &names, bool includeMultis) const;
   void GetAllCommandLabels(
      TranslatableStrings &labels, std::vector<bool> &vExcludeFromMacros,
      bool includeMultis) const;
   void GetAllCommandData(
      CommandIDs &names,
      std::vector<NormalizedKeyString> &keys,
      std::vector<NormalizedKeyString> &default_keys,
      TranslatableStrings &labels, TranslatableStrings &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
      TranslatableStrings &prefixes,
#endif
      bool includeMultis);

   // Each command is assigned a numerical ID for use in wxMenu and wxEvent,
   // which need not be the same across platforms or sessions
   CommandID GetNameFromNumericID( int id );

   TranslatableString GetLabelFromName(const CommandID &name);
   TranslatableString GetPrefixedLabelFromName(const CommandID &name);
   TranslatableString GetCategoryFromName(const CommandID &name);
   NormalizedKeyString GetKeyFromName(const CommandID &name) const;
   NormalizedKeyString GetDefaultKeyFromName(const CommandID &name);

   bool GetEnabled(const CommandID &name);
   int GetNumberOfKeysRead() const;

#if defined(_DEBUG)
   void CheckDups();
#endif
   // May have side-effect on the config file
   TranslatableString ReportDuplicateShortcuts();

   //
   // Loading/Saving
   //

   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   ///
   /// Formatting summaries that include shortcut keys
   ///
   TranslatableString DescribeCommandsAndShortcuts(
       // If a shortcut key is defined for the command, then it is appended,
       // parenthesized, after the translated name.
       const ComponentInterfaceSymbol commands[], size_t nCommands) const;

   // Sorted list of the shortcut keys to be excluded from the standard defaults
   static const std::vector<NormalizedKeyString> &ExcludedList();

protected:
   struct CommandListEntry;

private:

   //
   // Creating menus and adding commands
   //

   int NextIdentifier(int ID);
   CommandListEntry *NewIdentifier(const CommandID & name,
                                   const TranslatableString & label,
                                   wxMenu *menu,
                                   CommandHandlerFinder finder,
                                   CommandFunctorPointer callback,
                                   const CommandID &nameSuffix,
                                   int index,
                                   int count,
                                   const MenuRegistry::Options &options);
   
   void AddGlobalCommand(const CommandID &name,
                         const TranslatableString &label,
                         CommandHandlerFinder finder,
                         CommandFunctorPointer callback,
                         const MenuRegistry::Options &options = {});

   //
   // Executing commands
   //

protected:
   bool HandleCommandEntry(
      const CommandListEntry * entry, CommandFlag flags,
      bool alwaysEnabled, const wxEvent * evt = nullptr,
      const CommandContext *pGivenContext = nullptr );

   virtual void ExecuteCommand(const CommandContext &context,
      const wxEvent *evt, const CommandListEntry &entry);

   //
   // Modifying
   //

private:
   void Enable(CommandListEntry *entry, bool enabled);
   wxMenu *BeginMainMenu(const TranslatableString & tName);
   void EndMainMenu();
   wxMenu* BeginSubMenu(const TranslatableString & tName);
   void EndSubMenu();

   //
   // Accessing
   //

   wxMenuBar * CurrentMenuBar() const;
   wxMenuBar * GetMenuBar(const wxString & sMenu) const;
   wxMenu * CurrentSubMenu() const;
public:
   wxMenu * CurrentMenu() const;

   void UpdateCheckmarks();

   //! Format a string appropriate for insertion in a menu
   /*!
    @param pLabel if not null, use this instead of the manager's
    stored label
    */
   wxString FormatLabelForMenu(
      const CommandID &id, const TranslatableString *pLabel) const;

private:
   wxString FormatLabelForMenu(const CommandListEntry *entry) const;
   wxString FormatLabelForMenu(
      const TranslatableString &translatableLabel,
      const NormalizedKeyString &keyStr) const;
   wxString FormatLabelWithDisabledAccel(const CommandListEntry *entry) const;

   //
   // Loading/Saving
   //

   bool HandleXMLTag(const std::string_view& tag, const AttributesList &attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   XMLTagHandler *HandleXMLChild(const std::string_view& tag) override;

   void TellUserWhyDisallowed(const TranslatableString & Name, CommandFlag flagsGot,
      CommandFlag flagsRequired);

protected:
   AudacityProject &mProject;

   struct CommandListEntry
   {
      int id;
      CommandID name;
      TranslatableString longLabel;
      NormalizedKeyString key;
      NormalizedKeyString defaultKey;
      TranslatableString label;
      TranslatableString labelPrefix;
      TranslatableString labelTop;
      wxMenu *menu;
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
   using CommandKeyHash =
      std::unordered_map<NormalizedKeyString, CommandListEntry*>;
   CommandKeyHash mCommandKeyHash;

private:
   // mMaxList only holds shortcuts that should not be added (by default)
   // and is sorted.
   std::vector<NormalizedKeyString> mMaxListOnly;

   MenuBarList  mMenuBarList;
   SubMenuList  mSubMenuList;

   // This is an array of pointers, not structures, because the hash maps also
   // point to them,
   // so we don't want the structures to relocate with vector operations.
   using CommandList = std::vector<std::unique_ptr<CommandListEntry>>;
   CommandList  mCommandList;

   using CommandNameHash = std::unordered_map<CommandID, CommandListEntry*>;
   CommandNameHash  mCommandNameHash;

   using CommandNumericIDHash = std::unordered_map<int, CommandListEntry*>;
   CommandNumericIDHash  mCommandNumericIDHash;
   int mCurrentID;
   int mXMLKeysRead;

   bool mbSeparatorAllowed; // false at the start of a menu and immediately after a separator.

   TranslatableString mCurrentMenuName;
   TranslatableString mNiceName;
   int mLastProcessId;
   std::unique_ptr<wxMenu> uCurrentMenu;
   wxMenu *mCurrentMenu {};

   bool bMakingOccultCommands;
   std::unique_ptr< wxMenuBar > mTempMenuBar;
};
#endif
