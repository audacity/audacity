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

#include "Observer.h"
#include "Prefs.h"
#include "Project.h"
#include "Registry.h"

#include <vector>

#include "XMLTagHandler.h"

#include <unordered_map>

class wxEvent;
class wxMenu;
class wxMenuBar;
using CommandParameter = CommandID;

class BoolSetting;

struct MenuBarListEntry;
struct SubMenuListEntry;
struct CommandListEntry;

using MenuBarList = std::vector < MenuBarListEntry >;
using SubMenuList = std::vector < SubMenuListEntry >;

// This is an array of pointers, not structures, because the hash maps also point to them,
// so we don't want the structures to relocate with vector operations.
using CommandList = std::vector<std::unique_ptr<CommandListEntry>>;

using CommandKeyHash = std::unordered_map<NormalizedKeyString, CommandListEntry*>;
using CommandNameHash = std::unordered_map<CommandID, CommandListEntry*>;
using CommandNumericIDHash = std::unordered_map<int, CommandListEntry*>;

class AudacityProject;
class CommandContext;

namespace MenuTable {
   struct Traits;
   template<typename MenuTraits> struct Visitor;
}

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

   static void Visit(MenuTable::Visitor<MenuTable::Traits> &visitor,
      AudacityProject &project);

   // If checkActive, do not do complete flags testing on an
   // inactive project as it is needlessly expensive.
   CommandFlag GetUpdateFlags( bool checkActive = false ) const;
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

   void SetMaxList();
   void PurgeData();

   //
   // Creating menus and adding commands
   //

   std::unique_ptr<wxMenuBar> AddMenuBar(const wxString & sMenu);

   wxMenu *BeginMenu(const TranslatableString & tName);
   void EndMenu();

   // type of a function that determines checkmark state
   using CheckFn = std::function< bool(AudacityProject&) >;

   // For specifying unusual arguments in AddItem
   struct AUDACITY_DLL_API Options
   {
      Options() {}
      // Allow implicit construction from an accelerator string, which is
      // a very common case
      Options( const wxChar *accel_ ) : accel{ accel_ } {}
      // A two-argument constructor for another common case
      Options(
         const wxChar *accel_,
         const TranslatableString &longName_ )
      : accel{ accel_ }, longName{ longName_ } {}

      Options &&Accel (const wxChar *value) &&
         { accel = value; return std::move(*this); }
      Options &&IsEffect (bool value = true) &&
         { bIsEffect = value; return std::move(*this); }
      Options &&Parameter (const CommandParameter &value) &&
         { parameter = value; return std::move(*this); }
      Options &&LongName (const TranslatableString &value ) &&
         { longName = value; return std::move(*this); }
      Options &&IsGlobal () &&
         { global = true; return std::move(*this); }
      Options &&UseStrictFlags () &&
         { useStrictFlags = true; return std::move(*this); }
      Options &&WantKeyUp () &&
         { wantKeyUp = true; return std::move(*this); }
      Options &&SkipKeyDown () &&
         { skipKeyDown = true; return std::move(*this); }

      // This option affects debugging only:
      Options &&AllowDup () &&
         { allowDup = true; return std::move(*this); }

      Options &&AllowInMacros ( int value = 1 ) &&
         { allowInMacros = value; return std::move(*this); }

      // CheckTest is overloaded
      // Take arbitrary predicate
      Options &&CheckTest (const CheckFn &fn) &&
         { checker = fn; return std::move(*this); }
      // Take a preference path
      Options &&CheckTest (const wxChar *key, bool defaultValue) && {
         checker = MakeCheckFn( key, defaultValue );
         return std::move(*this);
      }
      // Take a BoolSetting
      Options &&CheckTest ( const BoolSetting &setting ) && {
         checker = MakeCheckFn( setting );
         return std::move(*this);
      }

      const wxChar *accel{ wxT("") };
      CheckFn checker; // default value means it's not a check item
      bool bIsEffect{ false };
      CommandParameter parameter{};
      TranslatableString longName{};
      bool global{ false };
      bool useStrictFlags{ false };
      bool wantKeyUp{ false };
      bool skipKeyDown{ false };
      bool allowDup{ false };
      int allowInMacros{ -1 }; // 0 = never, 1 = always, -1 = deduce from label

   private:
      static CheckFn
         MakeCheckFn( const wxString key, bool defaultValue );
      static CheckFn
         MakeCheckFn( const BoolSetting &setting );
   };

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
                const Options &options = {});

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

   // "permit" allows filtering even if the active window isn't a child of the project.
   // Lyrics and MixerTrackCluster classes use it.
   static bool FilterKeyEvent(AudacityProject &project, const wxKeyEvent & evt, bool permit = false);
   bool HandleMenuID(int id, CommandFlag flags, bool alwaysEnabled);
   void RegisterLastAnalyzer(const CommandContext& context);
   void RegisterLastTool(const CommandContext& context);
   void DoRepeatProcess(const CommandContext& context, int);

   enum TextualCommandResult {
      CommandFailure,
      CommandSuccess,
      CommandNotFound
   };

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
                                   const Options &options);
   
   void AddGlobalCommand(const CommandID &name,
                         const TranslatableString &label,
                         CommandHandlerFinder finder,
                         CommandFunctorPointer callback,
                         const Options &options = {});

   //
   // Executing commands
   //

   bool HandleCommandEntry(
      const CommandListEntry * entry, CommandFlag flags,
      bool alwaysEnabled, const wxEvent * evt = nullptr,
      const CommandContext *pGivenContext = nullptr );

   //
   // Modifying
   //

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

private:
   // mMaxList only holds shortcuts that should not be added (by default)
   // and is sorted.
   std::vector<NormalizedKeyString> mMaxListOnly;

   MenuBarList  mMenuBarList;
   SubMenuList  mSubMenuList;
   CommandList  mCommandList;
   CommandNameHash  mCommandNameHash;
   CommandKeyHash mCommandKeyHash;
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

// Define items that populate tables that specifically describe menu trees
namespace MenuTable {
using namespace Registry;

   //! A mix-in discovered by dynamic_cast; independent of the Traits
   struct MenuItemProperties {
      enum Properties {
         None,
         Inline,
         Section,
         Whole,
         Extension,
      };
      virtual ~MenuItemProperties();
      virtual Properties GetProperties() const = 0;
   };

namespace detail {
   struct VisitorBase {
      std::pair<bool, bool>
         ShouldBeginGroup(const MenuItemProperties *pProperties);
      void AfterBeginGroup(const MenuItemProperties *pProperties);
      bool ShouldEndGroup(const MenuItemProperties *pProperties);
      bool ShouldDoSeparator();

      std::vector<bool> firstItem;
      std::vector<bool> needSeparator;
   };
}

   //! Wraps the behavior of another VisitorFuntions<MenuTraits>, and also
   //! needs a callback for what to do at separator lines
   template<typename MenuTraits> struct Visitor
      : VisitorFunctions<MenuTraits>
      , detail::VisitorBase
   {
      Visitor(VisitorFunctions<MenuTraits> functions,
         std::function<void()> doSeparator
      )  : VisitorFunctions<MenuTraits>{ std::tuple{

      [this](const GroupItem<MenuTraits> &item, const Path &path)
      {
         using namespace MenuTable;
         const auto pProperties = dynamic_cast<const MenuItemProperties*>(&item);
         auto [begin, separate] = ShouldBeginGroup(pProperties);
         if (separate)
            mDoSeparator();
         if (begin)
            mWrapped.BeginGroup(item, path);
         AfterBeginGroup(pProperties);
      },

      [this](const Registry::SingleItem &item, const Path &path)
      {
         if (ShouldDoSeparator())
            mDoSeparator();
         mWrapped.Visit(item, path);
      },

      [this](const GroupItem<MenuTraits> &item, const Path &path)
      {
         using namespace MenuTable;
         const auto pProperties = dynamic_cast<const MenuItemProperties*>(&item);
         if (ShouldEndGroup(pProperties))
            mWrapped.EndGroup(item, path);
      }

      }}
      , mWrapped{ move(functions) }
      , mDoSeparator{ move(doSeparator) }
      {}

   private:
      const VisitorFunctions<MenuTraits> mWrapped;
      const std::function<void()> mDoSeparator;
   };

   struct CommandItem;
   struct CommandGroupItem;
   struct ConditionalGroupItem;
   struct MenuItem;
   struct MenuItems;
   struct SpecialItem;
   struct MenuPart;

   struct Traits : Registry::DefaultTraits {
      using ComputedItemContextType = AudacityProject;
      using LeafTypes = List<
         CommandItem, CommandGroupItem, SpecialItem>;
      using NodeTypes = List<
         ConditionalGroupItem, MenuItem, MenuItems, MenuPart>;
   };

   template<typename RegistryTraits>
   static inline bool IsSection(const GroupItem<RegistryTraits> &item) {
      auto pProperties = dynamic_cast<const MenuItemProperties *>(&item);
      return pProperties && pProperties->GetProperties() ==
         MenuItemProperties::Section;
   };

   struct MenuItemData {
      MenuItemData(TranslatableString title) : mTitle{ std::move(title) } {}
      const TranslatableString mTitle;
   };

   // Describes a main menu in the toolbar, or a sub-menu
   struct AUDACITY_DLL_API MenuItem final
      : Composite::Extension<
         GroupItem<Traits>, MenuItemData, const Identifier&
      >
      , MenuItemProperties
   {
      using Extension::Extension;
      ~MenuItem() override;
      const auto &GetTitle() const { return mTitle; }
      Properties GetProperties() const override;
   };

   using Condition = std::function<bool()>;

   // Collects other items that are conditionally shown or hidden, but are
   // always available to macro programming
   struct ConditionalGroupItem final
      : Composite::Extension<
         GroupItem<Traits>, Condition, const Identifier &
      >
   {
      using Extension::Extension;
      ~ConditionalGroupItem() override;
      using Condition::operator();
   };

   // usage:
   //   auto scope = FinderScope( findCommandHandler );
   //   return Items( ... );
   //
   // or:
   //   return ( FinderScope( findCommandHandler ), Items( ... ) );
   //
   // where findCommandHandler names a function.
   // This is used before a sequence of many calls to Command() and
   // CommandGroup(), so that the finder argument need not be specified
   // in each call.
   class AUDACITY_DLL_API FinderScope : ValueRestorer< CommandHandlerFinder >
   {
      static CommandHandlerFinder sFinder;

   public:
      //! @post result: `result != nullptr`
      static CommandHandlerFinder DefaultFinder() { return sFinder; }

      //! @pre `finder != nullptr`
      explicit
      FinderScope( CommandHandlerFinder finder )
         : ValueRestorer( sFinder, finder )
      { assert(finder); }
   };

   // Describes one command in a menu
   struct AUDACITY_DLL_API CommandItem final : SingleItem {
      CommandItem(const CommandID &name_,
               const TranslatableString &label_in_,
               CommandFunctorPointer callback_,
               CommandFlag flags_,
               const CommandManager::Options &options_,
               CommandHandlerFinder finder_);

      // Takes a pointer to member function directly, and delegates to the
      // previous constructor; useful within the lifetime of a FinderScope
      /*!
       @pre `finder != nullptr`
       */
      template< typename Handler >
      CommandItem(const CommandID &name,
               const TranslatableString &label_in,
               void (Handler::*pmf)(const CommandContext&),
               CommandFlag flags,
               const CommandManager::Options &options = {},
               CommandHandlerFinder finder = FinderScope::DefaultFinder())
         : CommandItem(name, label_in,
            CommandFunctorPointer{
               static_cast<CommandFunctorPointer::MemberFn>(pmf) },
            flags, options, finder)
      { assert(finder); }

      // Takes a pointer to nonmember function and delegates to the first
      // constructor
      CommandItem(const CommandID &name,
               const TranslatableString &label_in,
               CommandFunctorPointer::NonMemberFn callback,
               CommandFlag flags,
               const CommandManager::Options &options = {})
         : CommandItem(name, label_in,
            CommandFunctorPointer{ callback },
            flags, options, nullptr)
      {}
   
      ~CommandItem() override;

      const TranslatableString label_in;
      CommandHandlerFinder finder;
      CommandFunctorPointer callback;
      CommandFlag flags;
      CommandManager::Options options;
   };

   // Describes several successive commands in a menu that are closely related
   // and dispatch to one common callback, which will be passed a number
   // in the CommandContext identifying the command
   struct AUDACITY_DLL_API CommandGroupItem final : SingleItem {
      CommandGroupItem(const Identifier &name_,
               std::vector<ComponentInterfaceSymbol> items_,
               CommandFunctorPointer callback_,
               CommandFlag flags_,
               bool isEffect_,
               CommandHandlerFinder finder_);

      // Takes a pointer to member function directly, and delegates to the
      // previous constructor; useful within the lifetime of a FinderScope
      /*!
       @pre `finder != nullptr`
       */
      template< typename Handler >
      CommandGroupItem(const Identifier &name_,
               std::vector<ComponentInterfaceSymbol> items_,
               void (Handler::*pmf)(const CommandContext&),
               CommandFlag flags_,
               bool isEffect_,
               CommandHandlerFinder finder = FinderScope::DefaultFinder())
         : CommandGroupItem(name_, move(items_),
            CommandFunctorPointer{
               static_cast<CommandFunctorPointer::MemberFn>(pmf) },
            flags_, isEffect_, finder)
      { assert(finder); }

      // Takes a pointer to nonmember function and delegates to the first
      // constructor
      CommandGroupItem(const CommandID &name,
               std::vector< ComponentInterfaceSymbol > items,
               CommandFunctorPointer::NonMemberFn fn,
               CommandFlag flags,
               bool isEffect = false)
         : CommandGroupItem(name, move(items),
            CommandFunctorPointer{ fn },
            flags, isEffect, nullptr)
      {}

      ~CommandGroupItem() override;

      const std::vector<ComponentInterfaceSymbol> items;
      CommandHandlerFinder finder;
      CommandFunctorPointer callback;
      CommandFlag flags;
      bool isEffect;
   };

   // For manipulating the enclosing menu or sub-menu directly,
   // adding any number of items, not using the CommandManager
   struct SpecialItem final : SingleItem
   {
      using Appender = std::function< void( AudacityProject&, wxMenu& ) >;

      explicit SpecialItem( const Identifier &internalName, const Appender &fn_ )
      : SingleItem{ internalName }
      , fn{ fn_ }
      {}
      ~SpecialItem() override;

      Appender fn;
   };

   //! Groups of this type are inlined in the menu tree organization.  They
   //! (but not their contained items) are excluded from visitations using
   //! MenuVisitor
   struct MenuItems
      : Composite::Extension<
         GroupItem<Traits>, void, const Identifier &
      >
      , MenuItemProperties
   {
      using Extension::Extension;
      ~MenuItems() override;
      //! Anonymous if its name is empty, else weakly ordered
      Ordering GetOrdering() const override;
      Properties GetProperties() const override;
   };

   struct MenuPart
      : Composite::Extension<
         GroupItem<Traits>, void, const Identifier &
      >
      , MenuItemProperties
   {
      using Extension::Extension;
      ~MenuPart() override;
      Properties GetProperties() const override;
   };

   /*! @name Factories
      The following, and Registry::Indirect(), are the functions to use directly
      to specify elements of menu groupings.
    */
   //! @{

   //! Variadic constructor from pointers to subordinate items, which are moved
   //! into the result.
   /*!
    Null pointers are permitted, and ignored when building the menu.
    Items are spliced into the enclosing menu.
    The name is untranslated and may be empty, to make the group transparent
    in identification of items by path.  Otherwise try to keep the name
    stable across Audacity versions.
    */
   constexpr auto Items = Callable::UniqueMaker<MenuItems>();

   //! Like Items, but insert a menu separator between the menu section and
   //! any other items or sections before or after it in the same (innermost,
   //! enclosing) menu.
   /*!
    It's not necessary that the sisters of sections be other sections, but it
    might clarify the logical groupings.
    */
   constexpr auto Section = Callable::UniqueMaker<MenuPart>();
   
   //! Items will appear in a main toolbar menu or in a sub-menu.
   /*!
    The name is untranslated.  Try to keep the name stable across Audacity
    versions.
    If the name of a menu is empty, then subordinate items cannot be located
    by path.
    */
   constexpr auto Menu = Callable::UniqueMaker<MenuItem>();

   //! These items register in the CommandManager but are not shown in menus
   //! if the condition evaluates false.
   /*!
    The name is untranslated.  Try to keep the name stable across Audacity
    versions.
    Name for conditional group must be non-empty.
    */
   constexpr auto ConditionalItems = Callable::UniqueMaker<ConditionalGroupItem>();

   constexpr auto Command = Callable::UniqueMaker<CommandItem>();

   constexpr auto CommandGroup = Callable::UniqueMaker<CommandGroupItem,
      const Identifier &, std::vector<ComponentInterfaceSymbol>>();

   constexpr auto Special = Callable::UniqueMaker<SpecialItem>();

   //! @}

   struct ItemRegistry {
      static GroupItem<Traits> &Registry();
   };

   // Typically you make a static object of this type in the .cpp file that
   // also defines the added menu actions.
   // pItem can be specified by an expression using the inline functions above.
   using AttachedItem = RegisteredItem<ItemRegistry>;
}

#endif
