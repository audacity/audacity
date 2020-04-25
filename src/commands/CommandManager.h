/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include "../Experimental.h"

#include "audacity/Types.h"

#include "../ClientData.h"
#include "CommandFunctors.h"
#include "CommandFlag.h"

#include "Keyboard.h"
#include <vector>

#include "../xml/XMLTagHandler.h"

#include "audacity/Types.h"

#include <unordered_map>

class wxMenu;
class wxMenuBar;
class wxArrayString;
class wxMenu;
class wxMenuBar;
using CommandParameter = CommandID;

struct MenuBarListEntry
{
   MenuBarListEntry(const wxString &name_, wxMenuBar *menubar_);
   ~MenuBarListEntry();

   wxString name;
   wxWeakRef<wxMenuBar> menubar; // This structure does not assume memory ownership!
};

struct SubMenuListEntry
{
   SubMenuListEntry(const TranslatableString &name_, std::unique_ptr<wxMenu> menu_);
   SubMenuListEntry( SubMenuListEntry&& ) = default;
   ~SubMenuListEntry();

   TranslatableString name;
   std::unique_ptr<wxMenu> menu;
};

struct CommandListEntry
{
   int id;
   CommandID name;
   TranslatableString longLabel;
   NormalizedKeyString key;
   NormalizedKeyString defaultKey;
   TranslatableString label;
   TranslatableString labelPrefix;
   wxString labelTop;
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
   bool isGlobal;
   bool isOccult;
   bool isEffect;
   bool excludeFromMacros;
   CommandFlag flags;
   bool useStrictFlags{ false };
};

using MenuBarList = std::vector < MenuBarListEntry >;

// to do: remove the extra indirection when Mac compiler moves to newer version
using SubMenuList = std::vector < std::unique_ptr<SubMenuListEntry> >;

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

   // Type of a function that can intercept menu item handling.
   // If it returns true, bypass the usual dipatch of commands.
   using MenuHook = std::function< bool(const CommandID&) >;

   // install a menu hook, returning the previously installed one
   static MenuHook SetMenuHook( const MenuHook &hook );

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

   wxMenu *BeginMenu(const TranslatableString & tName);
   void EndMenu();

   // For specifying unusual arguments in AddItem
   struct Options
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
      Options &&AllowInMacros ( int value = 1 ) &&
         { allowInMacros = value; return std::move(*this); }

      // CheckTest is overloaded
      // Take arbitrary predicate
      Options &&CheckTest (const CommandListEntry::CheckFn &fn) &&
         { checker = fn; return std::move(*this); }
      // Take a preference path
      Options &&CheckTest (const wxChar *key, bool defaultValue) && {
         checker = MakeCheckFn( key, defaultValue );
         return std::move(*this);
      }

      const wxChar *accel{ wxT("") };
      CommandListEntry::CheckFn checker; // default value means it's not a check item
      bool bIsEffect{ false };
      CommandParameter parameter{};
      TranslatableString longName{};
      bool global{ false };
      bool useStrictFlags{ false };
      int allowInMacros{ -1 }; // 0 = never, 1 = always, -1 = deduce from label

   private:
      static CommandListEntry::CheckFn
         MakeCheckFn( const wxString key, bool defaultValue );
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
   bool FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit = false);
   bool HandleMenuID(AudacityProject &project, int id, CommandFlag flags, bool alwaysEnabled);

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

   void GetCategories(wxArrayString &cats, AudacityProject *);
   void GetAllCommandNames(CommandIDs &names, bool includeMultis) const;
   void GetAllCommandLabels(
      TranslatableStrings &labels, std::vector<bool> &vExcludeFromMacros,
      bool includeMultis) const;
   void GetAllCommandData(
      CommandIDs &names,
      std::vector<NormalizedKeyString> &keys,
      std::vector<NormalizedKeyString> &default_keys,
      TranslatableStrings &labels, wxArrayString &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
      TranslatableStrings &prefixes,
#endif
      bool includeMultis);

   // Each command is assigned a numerical ID for use in wxMenu and wxEvent,
   // which need not be the same across platforms or sessions
   CommandID GetNameFromNumericID( int id );

   TranslatableString GetLabelFromName(const CommandID &name);
   TranslatableString GetPrefixedLabelFromName(const CommandID &name);
   wxString GetCategoryFromName(const CommandID &name);
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

   // Sorted list of the shortcut keys to be exluded from the standard defaults
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

   bool HandleCommandEntry(AudacityProject &project,
      const CommandListEntry * entry, CommandFlag flags,
      bool alwaysEnabled, const wxEvent * evt = NULL);

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

   void UpdateCheckmarks( AudacityProject &project );
private:
   wxString FormatLabelForMenu(const CommandListEntry *entry) const;
   wxString FormatLabelWithDisabledAccel(const CommandListEntry *entry) const;

   //
   // Loading/Saving
   //

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;

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
   std::unique_ptr<wxMenu> uCurrentMenu;
   wxMenu *mCurrentMenu {};

   bool bMakingOccultCommands;
   std::unique_ptr< wxMenuBar > mTempMenuBar;
};

// Define classes and functions that associate parts of the user interface
// with path names
namespace Registry {
   // Items in the registry form an unordered tree, but each may also describe a
   // desired insertion point among its peers.  The request might not be honored
   // (as when the other name is not found, or when more than one item requests
   // the same ordering), but this is not treated as an error.
   struct OrderingHint
   {
      // The default Unspecified hint is just like End, except that in case the
      // item is delegated to (by a SharedItem, ComputedItem, or nameless
      // transparent group), the delegating item's hint will be used instead
      enum Type : int {
         Before, After,
         Begin, End,
         Unspecified // keep this last
      } type{ Unspecified };

      // name of some other BaseItem; significant only when type is Before or
      // After:
      Identifier name;

      OrderingHint() {}
      OrderingHint( Type type_, const wxString &name_ = {} )
         : type(type_), name(name_) {}

      bool operator == ( const OrderingHint &other ) const
      { return name == other.name && type == other.type; }

      bool operator < ( const OrderingHint &other ) const
      {
         // This sorts unspecified placements later
         return std::make_pair( type, name ) <
            std::make_pair( other.type, other.name );
      }
   };

   // TODO C++17: maybe use std::variant (discriminated unions) to achieve
   // polymorphism by other means, not needing unique_ptr and dynamic_cast
   // and using less heap.
   // Most items in the table will be the large ones describing commands, so the
   // waste of space in unions for separators and sub-menus should not be
   // large.
   struct BaseItem {
      // declare at least one virtual function so dynamic_cast will work
      explicit
      BaseItem( const Identifier &internalName )
         : name{ internalName }
      {}
      virtual ~BaseItem();

      const Identifier name;

      OrderingHint orderingHint;
   };
   using BaseItemPtr = std::unique_ptr<BaseItem>;
   using BaseItemSharedPtr = std::shared_ptr<BaseItem>;
   using BaseItemPtrs = std::vector<BaseItemPtr>;

   class Visitor;
   

   // An item that delegates to another held in a shared pointer; this allows
   // static tables of items to be computed once and reused
   // The name of the delegate is significant for path calculations, but the
   // SharedItem's ordering hint is used if the delegate has none
   struct SharedItem final : BaseItem {
      explicit SharedItem( const BaseItemSharedPtr &ptr_ )
         : BaseItem{ wxEmptyString }
         , ptr{ ptr_ }
      {}
      ~SharedItem() override;

      BaseItemSharedPtr ptr;
   };

   // A convenience function
   inline std::unique_ptr<SharedItem> Shared( const BaseItemSharedPtr &ptr )
      { return std::make_unique<SharedItem>( ptr ); }

   // An item that computes some other item to substitute for it, each time
   // the ComputedItem is visited
   // The name of the substitute is significant for path calculations, but the
   // ComputedItem's ordering hint is used if the substitute has none
   struct ComputedItem final : BaseItem {
      // The type of functions that generate descriptions of items.
      // Return type is a shared_ptr to let the function decide whether to
      // recycle the object or rebuild it on demand each time.
      // Return value from the factory may be null
      template< typename VisitorType >
      using Factory = std::function< BaseItemSharedPtr( VisitorType & ) >;

      using DefaultVisitor = Visitor;

      explicit ComputedItem( const Factory< DefaultVisitor > &factory_ )
         : BaseItem( wxEmptyString )
         , factory{ factory_ }
      {}
      ~ComputedItem() override;

      Factory< DefaultVisitor > factory;
   };

   // Common abstract base class for items that are not groups
   struct SingleItem : BaseItem {
      using BaseItem::BaseItem;
      ~SingleItem() override = 0;
   };

   // Common abstract base class for items that group other items
   struct GroupItem : BaseItem {
      using BaseItem::BaseItem;

      // Construction from an internal name and a previously built-up
      // vector of pointers
      GroupItem( const Identifier &internalName, BaseItemPtrs &&items_ )
         : BaseItem{ internalName }, items{ std::move( items_ ) }
      {}
      ~GroupItem() override = 0;

      // Whether the item is non-significant for path naming
      // when it also has an empty name
      virtual bool Transparent() const = 0;

      BaseItemPtrs items;
   };
   
   // GroupItem adding variadic constructor conveniences
   template< typename VisitorType = ComputedItem::DefaultVisitor >
   struct InlineGroupItem : GroupItem {
      using GroupItem::GroupItem;
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         InlineGroupItem( const Identifier &internalName, Args&&... args )
         : GroupItem( internalName )
         { Append( std::forward< Args >( args )... ); }

   private:
      // nullary overload grounds the recursion
      void Append() {}
      // recursive overload
      template< typename Arg, typename... Args >
         void Append( Arg &&arg, Args&&... moreArgs )
         {
            // Dispatch one argument to the proper overload of AppendOne.
            // std::forward preserves rvalue/lvalue distinction of the actual
            // argument of the constructor call; that is, it inserts a
            // std::move() if and only if the original argument is rvalue
            AppendOne( std::forward<Arg>( arg ) );
            // recur with the rest of the arguments
            Append( std::forward<Args>(moreArgs)... );
         };

      // Move one unique_ptr to an item into our array
      void AppendOne( BaseItemPtr&& ptr )
      {
         items.push_back( std::move( ptr ) );
      }
      // This overload allows a lambda or function pointer in the variadic
      // argument lists without any other syntactic wrapping, and also
      // allows implicit conversions to type Factory.
      // (Thus, a lambda can return a unique_ptr<BaseItem> rvalue even though
      // Factory's return type is shared_ptr, and the needed conversion is
      // applied implicitly.)
      void AppendOne( const ComputedItem::Factory<VisitorType> &factory )
      {
         auto adaptedFactory = [factory]( Registry::Visitor &visitor ){
            return factory( dynamic_cast< VisitorType& >( visitor ) );
         };
         AppendOne( std::make_unique<ComputedItem>( adaptedFactory ) );
      }
      // This overload lets you supply a shared pointer to an item, directly
      template<typename Subtype>
      void AppendOne( const std::shared_ptr<Subtype> &ptr )
      { AppendOne( std::make_unique<SharedItem>(ptr) ); }
   };

   // Inline group item also specifying transparency
   template< bool transparent,
      typename VisitorType = ComputedItem::DefaultVisitor >
   struct ConcreteGroupItem : InlineGroupItem< VisitorType >
   {
      using InlineGroupItem< VisitorType >::InlineGroupItem;
      ~ConcreteGroupItem() {}
      bool Transparent() const override { return transparent; }
   };

   // Concrete subclass of GroupItem that adds nothing else
   // TransparentGroupItem with an empty name is transparent to item path calculations
   // and propagates its ordering hint if subordinates don't specify hints
   // and it does specify one
   template< typename VisitorType = ComputedItem::DefaultVisitor >
   struct TransparentGroupItem final : ConcreteGroupItem< true, VisitorType >
   {
      using ConcreteGroupItem< true, VisitorType >::ConcreteGroupItem;
      ~TransparentGroupItem() override {}
   };

   // The /-separated path is relative to the GroupItem supplied to
   // RegisterItem.
   // For instance, wxT("Transport/Cursor") to locate an item under a sub-menu
   // of a main menu
   struct Placement {
      wxString path;
      OrderingHint hint;

      Placement( const wxString &path_, const OrderingHint &hint_ = {} )
         : path( path_ ), hint( hint_ )
      {}
   };

   // registry collects items, before consulting preferences and ordering
   // hints, and applying the merge procedure to them.
   // This function puts one more item into the registry.
   // The sequence of calls to RegisterItem has no significance for
   // determining the visitation ordering.  When sequence is important, register
   // a GroupItem.
   void RegisterItem( GroupItem &registry, const Placement &placement,
      BaseItemPtr pItem );
   
   // Define actions to be done in Visit.
   // Default implementations do nothing
   // The supplied path does not include the name of the item
   class Visitor
   {
   public:
      virtual ~Visitor();
      using Path = std::vector< Identifier >;
      virtual void BeginGroup( GroupItem &item, const Path &path );
      virtual void EndGroup( GroupItem &item, const Path &path );
      virtual void Visit( SingleItem &item, const Path &path );
   };

   // Top-down visitation of all items and groups in a tree rooted in
   // pTopItem, as merged with pRegistry.
   // The merger of the trees is recomputed in each call, not saved.
   // So neither given tree is modified.
   // But there may be a side effect on preferences to remember the ordering
   // imposed on each node of the unordered tree of registered items; each item
   // seen in the registry for the first time is placed somehere, and that
   // ordering should be kept the same thereafter in later runs (which may add
   // yet other previously unknown items).
   void Visit(
      Visitor &visitor,
      BaseItem *pTopItem,
      const GroupItem *pRegistry = nullptr );

   // Typically a static object.  Constructor initializes certain preferences
   // if they are not present.  These preferences determine an extrinsic
   // visitation ordering for registered items.  This is needed in some
   // places that have migrated from a system of exhaustive listings, to a
   // registry of plug-ins, and something must be done to preserve old
   // behavior.  It can be done in the central place using string literal
   // identifiers only, not requiring static compilation or linkage dependency.
   struct OrderingPreferenceInitializer {
      using Literal = const wxChar *;
      using Pair = std::pair< Literal, Literal >;
      using Pairs = std::vector< Pair >;
      OrderingPreferenceInitializer(
         // Specifies the topmost preference section:
         Literal root,
         // Specifies /-separated Registry paths relative to root
         // (these should be blank or start with / and not end with /),
         // each with a ,-separated sequence of identifiers, which specify a
         // desired ordering at one node of the tree:
         const Pairs &pairs );
   };
}

struct MenuVisitor : Registry::Visitor
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
   struct MenuSection {
      virtual ~MenuSection();
   };
   struct WholeMenu {
      WholeMenu( bool extend = false ) : extension{ extend }  {}
      virtual ~WholeMenu();
      bool extension;
   };

   // Describes a main menu in the toolbar, or a sub-menu
   struct MenuItem final : ConcreteGroupItem< false, ToolbarMenuVisitor >
      , WholeMenu {
      // Construction from an internal name and a previously built-up
      // vector of pointers
      MenuItem( const Identifier &internalName,
         const TranslatableString &title_, BaseItemPtrs &&items_ );
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         MenuItem( const Identifier &internalName,
            const TranslatableString &title_, Args&&... args )
            : ConcreteGroupItem< false, ToolbarMenuVisitor >{
               internalName, std::forward<Args>(args)... }
            , title{ title_ }
         {}
      ~MenuItem() override;

      TranslatableString title;
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
   class FinderScope : ValueRestorer< CommandHandlerFinder >
   {
      static CommandHandlerFinder sFinder;

   public:
      static CommandHandlerFinder DefaultFinder() { return sFinder; }

      explicit
      FinderScope( CommandHandlerFinder finder )
         : ValueRestorer( sFinder, finder )
      {}
   };

   // Describes one command in a menu
   struct CommandItem final : SingleItem {
      CommandItem(const CommandID &name_,
               const TranslatableString &label_in_,
               CommandFunctorPointer callback_,
               CommandFlag flags_,
               const CommandManager::Options &options_,
               CommandHandlerFinder finder_);

      // Takes a pointer to member function directly, and delegates to the
      // previous constructor; useful within the lifetime of a FinderScope
      template< typename Handler >
      CommandItem(const CommandID &name_,
               const TranslatableString &label_in_,
               void (Handler::*pmf)(const CommandContext&),
               CommandFlag flags_,
               const CommandManager::Options &options_,
               CommandHandlerFinder finder = FinderScope::DefaultFinder())
         : CommandItem(name_, label_in_,
            static_cast<CommandFunctorPointer>(pmf),
            flags_, options_, finder)
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
   struct CommandGroupItem final : SingleItem {
      CommandGroupItem(const Identifier &name_,
               std::vector< ComponentInterfaceSymbol > items_,
               CommandFunctorPointer callback_,
               CommandFlag flags_,
               bool isEffect_,
               CommandHandlerFinder finder_);

      // Takes a pointer to member function directly, and delegates to the
      // previous constructor; useful within the lifetime of a FinderScope
      template< typename Handler >
      CommandGroupItem(const Identifier &name_,
               std::vector< ComponentInterfaceSymbol > items_,
               void (Handler::*pmf)(const CommandContext&),
               CommandFlag flags_,
               bool isEffect_,
               CommandHandlerFinder finder = FinderScope::DefaultFinder())
         : CommandGroupItem(name_, std::move(items_),
            static_cast<CommandFunctorPointer>(pmf),
            flags_, isEffect_, finder)
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
      const Identifier &internalName, const TranslatableString &title, Args&&... args )
         { return std::make_unique<MenuItem>(
            internalName, title, std::forward<Args>(args)... ); }
   inline std::unique_ptr<MenuItem> Menu(
      const Identifier &internalName, const TranslatableString &title, BaseItemPtrs &&items )
         { return std::make_unique<MenuItem>(
            internalName, title, std::move( items ) ); }

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

   template< typename Handler >
   inline std::unique_ptr<CommandItem> Command(
      const CommandID &name,
      const TranslatableString &label_in,
      void (Handler::*pmf)(const CommandContext&),
      CommandFlag flags, const CommandManager::Options &options = {},
      CommandHandlerFinder finder = FinderScope::DefaultFinder())
   {
      return std::make_unique<CommandItem>(
         name, label_in, pmf, flags, options, finder
      );
   }

   template< typename Handler >
   inline std::unique_ptr<CommandGroupItem> CommandGroup(
      const Identifier &name,
      std::vector< ComponentInterfaceSymbol > items,
      void (Handler::*pmf)(const CommandContext&),
      CommandFlag flags, bool isEffect = false,
      CommandHandlerFinder finder = FinderScope::DefaultFinder())
   {
      return std::make_unique<CommandGroupItem>(
         name, std::move(items), pmf, flags, isEffect, finder
      );
   }

   inline std::unique_ptr<SpecialItem> Special(
      const Identifier &name, const SpecialItem::Appender &fn )
         { return std::make_unique<SpecialItem>( name, fn ); }

   // Typically you make a static object of this type in the .cpp file that
   // also defines the added menu actions.
   // pItem can be specified by an expression using the inline functions above.
   struct AttachedItem final
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
