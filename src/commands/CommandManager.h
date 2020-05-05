/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include "Identifier.h"

#include "ClientData.h"
#include "CommandFunctors.h"
#include "CommandFlag.h"
#include "GlobalVariable.h"

#include "Keyboard.h"

#include "Prefs.h"
#include "Registry.h"

#include <vector>

#include "../widgets/BasicMenu.h"
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

class AUDACITY_DLL_API CommandManager final
   : public XMLTagHandler
   , public ClientData::Base
{
 public:
   static CommandManager &Get( AudacityProject &project );
   static const CommandManager &Get( const AudacityProject &project );

   // Interception of menu item handling.
   // If it returns true, bypass the usual dispatch of commands.
   struct AUDACITY_DLL_API GlobalMenuHook : GlobalHook<GlobalMenuHook,
      bool(const CommandID&)
   >{};

   //
   // Constructor / Destructor
   //

   CommandManager();
   virtual ~CommandManager();

   CommandManager(const CommandManager&) PROHIBITED;
   CommandManager &operator= (const CommandManager&) PROHIBITED;

   void SetMaxList();
   void PurgeData();

   //
   // Creating menus and adding commands
   //

   std::unique_ptr<wxMenuBar> AddMenuBar(const wxString & sMenu);

   wxMenu *BeginMenu(const BasicMenu::Item::Text & text);
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

   void AddItem(AudacityProject &project,
                const CommandID & name,
                const BasicMenu::Item::Text &text,
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
   bool FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit = false);
   bool HandleMenuID(AudacityProject &project, int id, CommandFlag flags, bool alwaysEnabled);
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

   TranslatableStrings GetCategories( AudacityProject& );
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
   void RemoveDuplicateShortcuts();

   //
   // Loading/Saving
   //

   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   ///
   /// Formatting summaries that include shortcut keys
   ///
   TranslatableString DescribeCommandsAndShortcuts
   (
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
                                   const BasicMenu::Item::Text & text,
                                   wxMenu *menu,
                                   CommandHandlerFinder finder,
                                   CommandFunctorPointer callback,
                                   const CommandID &nameSuffix,
                                   int index,
                                   int count,
                                   const Options &options);
   
   void AddGlobalCommand(const CommandID &name,
                         const BasicMenu::Item::Text &text,
                         CommandHandlerFinder finder,
                         CommandFunctorPointer callback,
                         const Options &options = {});

   //
   // Executing commands
   //

   bool HandleCommandEntry(AudacityProject &project,
      const CommandListEntry * entry, CommandFlag flags,
      bool alwaysEnabled, const wxEvent * evt = nullptr,
      const CommandContext *pGivenContext = nullptr );

   //
   // Modifying
   //

   void Enable(CommandListEntry *entry, bool enabled);
   wxMenu *BeginMainMenu(const BasicMenu::Item::Text & text);
   void EndMainMenu();
   wxMenu* BeginSubMenu(const BasicMenu::Item::Text & text);
   void EndSubMenu();

   //
   // Accessing
   //

   wxMenuBar * CurrentMenuBar() const;
   wxMenuBar * GetMenuBar(const wxString & sMenu) const;
   wxMenu * CurrentSubMenu() const;
public:
   wxMenu * CurrentMenu() const;

   void UpdateCheckmarks( AudacityProject &project );

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

   TranslatableString mNiceName;
   int mLastProcessId;
   BasicMenu::Item::Text mCurrentMenuText;
   std::unique_ptr<wxMenu> uCurrentMenu;
   wxMenu *mCurrentMenu {};

   bool bMakingOccultCommands;
   std::unique_ptr< wxMenuBar > mTempMenuBar;
};

struct AUDACITY_DLL_API MenuVisitor : Registry::Visitor
{
   // final overrides
   void BeginGroup( Registry::GroupItem &item, const Path &path ) final;
   void EndGroup( Registry::GroupItem &item, const Path& ) final;
   void Visit( Registry::SingleItem &item, const Path &path ) final;

   // added virtuals
   virtual void DoBeginGroup( Registry::GroupItem &item, const Path &path );
   virtual void DoEndGroup( Registry::GroupItem &item, const Path &path );
   virtual void DoVisit( Registry::SingleItem &item, const Path &path );
   virtual void DoSeparator();

private:
   void MaybeDoSeparator();
   std::vector<bool> firstItem;
   std::vector<bool> needSeparator;
};

struct ToolbarMenuVisitor : MenuVisitor
{
   explicit ToolbarMenuVisitor( AudacityProject &p ) : project{ p } {}
   operator AudacityProject & () const { return project; }
   AudacityProject &project;
};

// Define items that populate tables that specifically describe menu trees
namespace MenuTable {
   using namespace Registry;

   // These are found by dynamic_cast
   struct AUDACITY_DLL_API MenuSection {
      virtual ~MenuSection();
   };
   struct AUDACITY_DLL_API WholeMenu {
      WholeMenu( bool extend = false ) : extension{ extend }  {}
      virtual ~WholeMenu();
      bool extension;
   };

   // Describes a main menu in the toolbar, or a sub-menu
   struct AUDACITY_DLL_API MenuItem final
      : ConcreteGroupItem< false, ToolbarMenuVisitor >
      , WholeMenu {
      // Construction from an internal name and a previously built-up
      // vector of pointers
      MenuItem( const Identifier &internalName,
         const BasicMenu::Item::Text &text, BaseItemPtrs &&items_ );
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         MenuItem( const Identifier &internalName,
            const BasicMenu::Item::Text &text, Args&&... args )
            : ConcreteGroupItem< false, ToolbarMenuVisitor >{
               internalName, std::forward<Args>(args)... }
            , text{ text }
         {}
      ~MenuItem() override;

      BasicMenu::Item::Text text;
   };

   // Collects other items that are conditionally shown or hidden, but are
   // always available to macro programming
   struct ConditionalGroupItem final
      : ConcreteGroupItem< false, ToolbarMenuVisitor > {
      using Condition = std::function< bool() >;

      // Construction from an internal name and a previously built-up
      // vector of pointers
      ConditionalGroupItem( const Identifier &internalName,
         Condition condition_, BaseItemPtrs &&items_ );
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         ConditionalGroupItem( const Identifier &internalName,
            Condition condition_, Args&&... args )
            : ConcreteGroupItem< false, ToolbarMenuVisitor >{
               internalName, std::forward<Args>(args)... }
            , condition{ condition_ }
         {}
      ~ConditionalGroupItem() override;

      Condition condition;
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
               const BasicMenu::Item::Text &text,
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
      CommandItem(const CommandID &name_,
               const BasicMenu::Item::Text &text,
               void (Handler::*pmf)(const CommandContext&),
               CommandFlag flags_,
               const CommandManager::Options &options_,
               CommandHandlerFinder finder = FinderScope::DefaultFinder())
         : CommandItem(name_, text,
            CommandFunctorPointer{
               static_cast<CommandFunctorPointer::MemberFn>(pmf) },
            flags_, options_, finder)
      { assert(finder); }

      // Takes a pointer to nonmember function and delegates to the first
      // constructor
      CommandItem(const CommandID &name_,
               const TranslatableString &label_in_,
               CommandFunctorPointer::NonMemberFn callback_,
               CommandFlag flags_,
               const CommandManager::Options &options_)
         : CommandItem(name_, label_in_,
            CommandFunctorPointer{ callback_ },
            flags_, options_, nullptr)
      {}
   
      ~CommandItem() override;

      const BasicMenu::Item::Text text;
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
               std::vector< ComponentInterfaceSymbol > items_,
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
               std::vector< ComponentInterfaceSymbol > items_,
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
      CommandGroupItem(const CommandID &name_,
               std::vector< ComponentInterfaceSymbol > items_,
               CommandFunctorPointer::NonMemberFn fn_,
               CommandFlag flags_,
               bool isEffect_)
         : CommandGroupItem(name_, move(items_),
            CommandFunctorPointer{ fn_ },
            flags_, isEffect_, nullptr)
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

   struct MenuPart : ConcreteGroupItem< false, ToolbarMenuVisitor >, MenuSection
   {
      template< typename... Args >
      explicit
      MenuPart( const Identifier &internalName, Args&&... args )
         : ConcreteGroupItem< false, ToolbarMenuVisitor >{
            internalName, std::forward< Args >( args )... }
      {}
   };
   using MenuItems = ConcreteGroupItem< true, ToolbarMenuVisitor >;

   // The following, and Shared(), are the functions to use directly
   // in writing table definitions.

   // Group items can be constructed two ways.
   // Pointers to subordinate items are moved into the result.
   // Null pointers are permitted, and ignored when building the menu.
   // Items are spliced into the enclosing menu.
   // The name is untranslated and may be empty, to make the group transparent
   // in identification of items by path.  Otherwise try to keep the name
   // stable across Audacity versions.
   template< typename... Args >
   inline std::unique_ptr< MenuItems > Items(
      const Identifier &internalName, Args&&... args )
         { return std::make_unique< MenuItems >(
            internalName, std::forward<Args>(args)... ); }

   // Like Items, but insert a menu separator between the menu section and
   // any other items or sections before or after it in the same (innermost,
   // enclosing) menu.
   // It's not necessary that the sisters of sections be other sections, but it
   // might clarify the logical groupings.
   template< typename... Args >
   inline std::unique_ptr< MenuPart > Section(
      const Identifier &internalName, Args&&... args )
         { return std::make_unique< MenuPart >(
            internalName, std::forward<Args>(args)... ); }
   
   // Menu items can be constructed two ways, as for group items
   // Items will appear in a main toolbar menu or in a sub-menu.
   // The name is untranslated.  Try to keep the name stable across Audacity
   // versions.
   // If the name of a menu is empty, then subordinate items cannot be located
   // by path.
   template< typename... Args >
   inline std::unique_ptr<MenuItem> Menu(
      const Identifier &internalName,
      const BasicMenu::Item::Text &text, Args&&... args )
         { return std::make_unique<MenuItem>(
            internalName, text, std::forward<Args>(args)... ); }
   inline std::unique_ptr<MenuItem> Menu(
      const Identifier &internalName,
      const BasicMenu::Item::Text &text, BaseItemPtrs &&items )
         { return std::make_unique<MenuItem>(
            internalName, text, std::move( items ) ); }

   // Conditional group items can be constructed two ways, as for group items
   // These items register in the CommandManager but are not shown in menus
   // if the condition evaluates false.
   // The name is untranslated.  Try to keep the name stable across Audacity
   // versions.
   // Name for conditional group must be non-empty.
   template< typename... Args >
   inline std::unique_ptr<ConditionalGroupItem> ConditionalItems(
      const Identifier &internalName,
      ConditionalGroupItem::Condition condition, Args&&... args )
         { return std::make_unique<ConditionalGroupItem>(
            internalName, condition, std::forward<Args>(args)... ); }
   inline std::unique_ptr<ConditionalGroupItem> ConditionalItems(
      const Identifier &internalName, ConditionalGroupItem::Condition condition,
      BaseItemPtrs &&items )
         { return std::make_unique<ConditionalGroupItem>(
            internalName, condition, std::move( items ) ); }

   // Make either a menu item or just a group, depending on the nonemptiness
   // of the title.
   // The name is untranslated and may be empty, to make the group transparent
   // in identification of items by path.  Otherwise try to keep the name
   // stable across Audacity versions.
   // If the name of a menu is empty, then subordinate items cannot be located
   // by path.
   template< typename... Args >
   inline BaseItemPtr MenuOrItems(
      const Identifier &internalName, const TranslatableString &title, Args&&... args )
         {  if ( title.empty() )
               return Items( internalName, std::forward<Args>(args)... );
            else
               return std::make_unique<MenuItem>(
                  internalName, title, std::forward<Args>(args)... ); }
   inline BaseItemPtr MenuOrItems(
      const Identifier &internalName,
      const TranslatableString &title, BaseItemPtrs &&items )
         {  if ( title.empty() )
               return Items( internalName, std::move( items ) );
            else
               return std::make_unique<MenuItem>(
                  internalName, title, std::move( items ) ); }

   /*!
    @pre `finder != nullptr`
    */
   template< typename Handler >
   inline std::unique_ptr<CommandItem> Command(
      const CommandID &name,
      const BasicMenu::Item::Text &text,
      void (Handler::*pmf)(const CommandContext&),
      CommandFlag flags, const CommandManager::Options &options = {},
      CommandHandlerFinder finder = FinderScope::DefaultFinder())
   {
      assert(finder);
      return std::make_unique<CommandItem>(
         name, text, pmf, flags, options, finder
      );
   }

   inline std::unique_ptr<CommandItem> Command(
      const CommandID &name,
      const TranslatableString &label_in,
      void (*fn)(const CommandContext&),
      CommandFlag flags, const CommandManager::Options &options = {})
   {
      return std::make_unique<CommandItem>(
         name, label_in, fn, flags, options
      );
   }

   /*!
    @pre `finder != nullptr`
    */
   template< typename Handler >
   inline std::unique_ptr<CommandGroupItem> CommandGroup(
      const Identifier &name,
      std::vector< ComponentInterfaceSymbol > items,
      void (Handler::*pmf)(const CommandContext&),
      CommandFlag flags, bool isEffect = false,
      CommandHandlerFinder finder = FinderScope::DefaultFinder())
   {
      assert(finder);
      return std::make_unique<CommandGroupItem>(
         name, move(items), pmf, flags, isEffect, finder
      );
   }

   inline std::unique_ptr<CommandGroupItem> CommandGroup(
      const Identifier &name,
      std::vector< ComponentInterfaceSymbol > items,
      void (*fn)(const CommandContext&),
      CommandFlag flags, bool isEffect = false)
   {
      return std::make_unique<CommandGroupItem>(
         name, move(items), fn, flags, isEffect
      );
   }

   inline std::unique_ptr<SpecialItem> Special(
      const Identifier &name, const SpecialItem::Appender &fn )
         { return std::make_unique<SpecialItem>( name, fn ); }

   // Typically you make a static object of this type in the .cpp file that
   // also defines the added menu actions.
   // pItem can be specified by an expression using the inline functions above.
   struct AUDACITY_DLL_API AttachedItem final
   {
      AttachedItem( const Placement &placement, BaseItemPtr pItem );

      AttachedItem( const wxString &path, BaseItemPtr pItem )
         // Delegating constructor
         : AttachedItem( Placement{ path }, std::move( pItem ) )
      {}
   };

   void DestroyRegistry();

}

#endif
