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

#include "CommandFunctors.h"
#include "CommandFlag.h"

#include "../MemoryX.h"
#include "Keyboard.h"
#include <vector>
#include <wx/hashmap.h>

#include "../xml/XMLTagHandler.h"

#include "audacity/Types.h"

#include <unordered_map>

class wxMenu;
class wxMenuBar;
class wxArrayString;
class TranslatedInternalString;
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
   SubMenuListEntry(const wxString &name_, std::unique_ptr<wxMenu> &&menu_);
   SubMenuListEntry(SubMenuListEntry &&that);
   ~SubMenuListEntry();

   wxString name;
   std::unique_ptr<wxMenu> menu;
};

struct CommandListEntry
{
   int id;
   CommandID name;
   wxString longLabel;
   NormalizedKeyString key;
   NormalizedKeyString defaultKey;
   wxString label;
   wxString labelPrefix;
   wxString labelTop;
   wxMenu *menu;
   CommandHandlerFinder finder;
   CommandFunctorPointer callback;
   CommandParameter parameter;
   bool multi;
   int index;
   int count;
   bool enabled;
   bool skipKeydown;
   bool wantKeyup;
   bool isGlobal;
   bool isOccult;
   bool isEffect;
   bool hasDialog;
   CommandFlag flags;
   CommandMask mask;
};

using MenuBarList = std::vector < MenuBarListEntry >;

// to do: remove the extra indirection when Mac compiler moves to newer version
using SubMenuList = std::vector < std::unique_ptr<SubMenuListEntry> >;

// This is an array of pointers, not structures, because the hash maps also point to them,
// so we don't want the structures to relocate with vector operations.
using CommandList = std::vector<std::unique_ptr<CommandListEntry>>;

namespace std
{
   template<> struct hash< NormalizedKeyString > {
      size_t operator () (const NormalizedKeyString &str) const // noexcept
      {
         auto &stdstr = str.Raw(); // no allocations, a cheap fetch
         using Hasher = std::hash< wxString >;
         return Hasher{}( stdstr );
      }
   };
}

using CommandKeyHash = std::unordered_map<NormalizedKeyString, CommandListEntry*>;
using CommandNameHash = std::unordered_map<wxString, CommandListEntry*>;
using CommandIDHash = std::unordered_map<int, CommandListEntry*>;

class AudacityProject;
class CommandContext;

class AUDACITY_DLL_API CommandManager final : public XMLTagHandler
{
 public:

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

   // You may either called SetCurrentMenu later followed by ClearCurrentMenu,
   // or else BeginMenu followed by EndMenu.  Don't mix them.
   wxMenu *BeginMenu(const wxString & tName);
   void EndMenu();

   // For specifying unusual arguments in AddItem
   struct Options
   {
      Options() {}
      // Allow implicit construction from an accelerator string, which is
      // a very common case
      Options( const wxChar *accel_ ) : accel{ accel_ } {}
      // A two-argument constructor for another common case
      Options( const wxChar *accel_, const wxString &longName_ )
      : accel{ accel_ }, longName{ longName_ } {}

      Options &&Accel (const wxChar *value) &&
         { accel = value; return std::move(*this); }
      Options &&CheckState (bool value) &&
         { check = value ? 1 : 0; return std::move(*this); }
      Options &&IsEffect () &&
         { bIsEffect = true; return std::move(*this); }
      Options &&Parameter (const CommandParameter &value) &&
         { parameter = value; return std::move(*this); }
      Options &&Mask (CommandMask value) &&
         { mask = value; return std::move(*this); }
      Options &&LongName (const wxString &value) &&
         { longName = value; return std::move(*this); }
      Options &&IsGlobal () &&
         { global = true; return std::move(*this); }

      const wxChar *accel{ wxT("") };
      int check{ -1 }; // default value means it's not a check item
      bool bIsEffect{ false };
      CommandParameter parameter{};
      CommandMask mask{ NoFlagsSpecified };
      wxString longName{}; // translated
      bool global{ false };
   };

   void AddItemList(const CommandID & name,
                    const ComponentInterfaceSymbol items[],
                    size_t nItems,
                    CommandHandlerFinder finder,
                    CommandFunctorPointer callback,
                    CommandFlag flags,
                    bool bIsEffect = false);

   void AddItem(const CommandID &name,
                const wxChar *label_in,
                bool hasDialog,
                CommandHandlerFinder finder,
                CommandFunctorPointer callback,
                CommandFlag flags,
                const Options &options = {});

   void AddSeparator();

   // A command doesn't actually appear in a menu but might have a
   // keyboard shortcut.
   void AddCommand(const CommandID &name,
                   const wxChar *label,
                   CommandHandlerFinder finder,
                   CommandFunctorPointer callback,
                   CommandFlag flags);

   void AddCommand(const CommandID &name,
                   const wxChar *label,
                   CommandHandlerFinder finder,
                   CommandFunctorPointer callback,
                   const wxChar *accel,
                   CommandFlag flags);

   void PopMenuBar();
   void BeginOccultCommands();
   void EndOccultCommands();


   void SetCommandFlags(const CommandID &name, CommandFlag flags, CommandMask mask);

   //
   // Modifying menus
   //

   void EnableUsingFlags(CommandFlag flags, CommandMask mask);
   void Enable(const wxString &name, bool enabled);
   void Check(const CommandID &name, bool checked);
   void Modify(const wxString &name, const wxString &newLabel);

   // You may either called SetCurrentMenu later followed by ClearCurrentMenu,
   // or else BeginMenu followed by EndMenu.  Don't mix them.
   void SetCurrentMenu(wxMenu * menu);
   void ClearCurrentMenu();

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
   bool HandleMenuID(int id, CommandFlag flags, CommandMask mask);
   bool HandleTextualCommand(const CommandID & Str, const CommandContext & context, CommandFlag flags, CommandMask mask);

   //
   // Accessing
   //

   void GetCategories(wxArrayString &cats);
   void GetAllCommandNames(CommandIDs &names, bool includeMultis) const;
   void GetAllCommandLabels(
      wxArrayString &labels, std::vector<bool> &vHasDialog,
      bool includeMultis) const;
   void GetAllCommandData(
      CommandIDs &names,
      std::vector<NormalizedKeyString> &keys,
      std::vector<NormalizedKeyString> &default_keys,
      wxArrayString &labels, wxArrayString &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
      wxArrayString &prefixes,
#endif
      bool includeMultis);

   CommandID GetNameFromID( int id );

   wxString GetLabelFromName(const CommandID &name);
   wxString GetPrefixedLabelFromName(const CommandID &name);
   wxString GetCategoryFromName(const CommandID &name);
   NormalizedKeyString GetKeyFromName(const CommandID &name) const;
   NormalizedKeyString GetDefaultKeyFromName(const CommandID &name);

   bool GetEnabled(const CommandID &name);

#if defined(__WXDEBUG__)
   void CheckDups();
#endif

   //
   // Loading/Saving
   //

   void WriteXML(XMLWriter &xmlFile) const /* not override */;
   void TellUserWhyDisallowed(const wxString & Name, CommandFlag flagsGot, CommandFlag flagsRequired);

   ///
   /// Formatting summaries that include shortcut keys
   ///
   wxString DescribeCommandsAndShortcuts
   (
       // If a shortcut key is defined for the command, then it is appended,
       // parenthesized, after the translated name.
       const TranslatedInternalString commands[], size_t nCommands) const;

   // Sorted list of the shortcut keys to be exluded from the standard defaults
   static const std::vector<NormalizedKeyString> &ExcludedList();

private:

   //
   // Creating menus and adding commands
   //

   int NextIdentifier(int ID);
   CommandListEntry *NewIdentifier(const CommandID & name,
                                   const wxString & label,
                                   const wxString & longLabel,
                                   bool hasDialog,
                                   wxMenu *menu,
                                   CommandHandlerFinder finder,
                                   CommandFunctorPointer callback,
                                   const CommandID &nameSuffix,
                                   int index,
                                   int count,
                                   bool bIsEffect);
   CommandListEntry *NewIdentifier(const CommandID & name,
                                   const wxString & label,
                                   const wxString & longLabel,
                                   bool hasDialog,
                                   const wxString & accel,
                                   wxMenu *menu,
                                   CommandHandlerFinder finder,
                                   CommandFunctorPointer callback,
                                   const CommandID &nameSuffix,
                                   int index,
                                   int count,
                                   bool bIsEffect,
                                   const CommandParameter &parameter);
   
   void AddGlobalCommand(const CommandID &name,
                         const wxChar *label,
                         bool hasDialog,
                         CommandHandlerFinder finder,
                         CommandFunctorPointer callback,
                         const wxChar *accel);

   //
   // Executing commands
   //

   bool HandleCommandEntry(const CommandListEntry * entry, CommandFlag flags, CommandMask mask, const wxEvent * evt = NULL);

   //
   // Modifying
   //

   void Enable(CommandListEntry *entry, bool enabled);
   wxMenu *BeginMainMenu(const wxString & tName);
   void EndMainMenu();
   wxMenu* BeginSubMenu(const wxString & tName);
   void EndSubMenu();

   //
   // Accessing
   //

   wxMenuBar * CurrentMenuBar() const;
   wxMenuBar * GetMenuBar(const wxString & sMenu) const;
   wxMenu * CurrentSubMenu() const;
public:
   wxMenu * CurrentMenu() const;
private:
   wxString GetLabel(const CommandListEntry *entry) const;
   wxString GetLabelWithDisabledAccel(const CommandListEntry *entry) const;

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
   CommandIDHash  mCommandIDHash;
   int mCurrentID;
   int mXMLKeysRead;

   bool mbSeparatorAllowed; // false at the start of a menu and immediately after a separator.

   wxString mCurrentMenuName;
   std::unique_ptr<wxMenu> uCurrentMenu;
   wxMenu *mCurrentMenu {};

   bool bMakingOccultCommands;
   std::unique_ptr< wxMenuBar > mTempMenuBar;
};

// Define items that populate tables that describe menu trees
namespace MenuTable {
   // TODO C++17: maybe use std::variant (discriminated unions) to achieve
   // polymorphism by other means, not needing unique_ptr and dynamic_cast
   // and using less heap.
   // Most items in the table will be the large ones describing commands, so the
   // waste of space in unions for separators and sub-menus should not be
   // large.
   struct BaseItem {
      // declare at least one virtual function so dynamic_cast will work
      virtual ~BaseItem();
   };
   using BaseItemPtr = std::unique_ptr<BaseItem>;
   using BaseItemPtrs = std::vector<BaseItemPtr>;
   

   // The type of functions that generate menu table descriptions.
   // Return type is a shared_ptr to let the function decide whether to recycle
   // the object or rebuild it on demand each time.
   // Return value from the factory may be null.
   using Factory = std::function<
      std::shared_ptr< MenuTable::BaseItem >( AudacityProject & )
   >;

   struct ComputedItem : BaseItem {
      explicit ComputedItem( const Factory &factory_ )
         : factory{ factory_ }
      {}
      ~ComputedItem() override;

      Factory factory;
   };

   struct GroupItem : BaseItem {
      // Construction from a previously built-up vector of pointers
      GroupItem( BaseItemPtrs &&items_ );
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         GroupItem( Args&&... args )
         { Append( std::forward< Args >( args )... ); }
      ~GroupItem() override;

      BaseItemPtrs items;

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
      void AppendOne( BaseItemPtr&& ptr );
      // This overload allows a lambda or function pointer in the variadic
      // argument lists without any other syntactic wrapping, and also
      // allows implicit conversions to type Factory.
      // (Thus, a lambda can return a unique_ptr<BaseItem> rvalue even though
      // Factory's return type is shared_ptr, and the needed conversion is
      // appled implicitly.)
      void AppendOne( const Factory &factory )
      { AppendOne( std::make_unique<ComputedItem>( factory ) ); }
   };

   struct MenuItem final : GroupItem {
      // Construction from a previously built-up vector of pointers
      MenuItem( const wxString &title_, BaseItemPtrs &&items_ );
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         MenuItem( const wxString &title_, Args&&... args )
            : GroupItem{ std::forward<Args>(args)... }
            , title{ title_ }
         {}
      ~MenuItem() override;

      wxString title; // translated
   };

   struct ConditionalGroupItem final : GroupItem {
      using Condition = std::function< bool() >;

      // Construction from a previously built-up vector of pointers
      ConditionalGroupItem( Condition condition_, BaseItemPtrs &&items_ );
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         ConditionalGroupItem( Condition condition_, Args&&... args )
            : GroupItem{ std::forward<Args>(args)... }
            , condition{ condition_ }
         {}
      ~ConditionalGroupItem() override;

      Condition condition;
   };

   struct SeparatorItem final : BaseItem
   {
      ~SeparatorItem() override;
   };

   struct CommandItem final : BaseItem {
      CommandItem(const CommandID &name_,
               const wxString &label_in_,
               bool hasDialog_,
               CommandHandlerFinder finder_,
               CommandFunctorPointer callback_,
               CommandFlag flags_,
               const CommandManager::Options &options_);
      ~CommandItem() override;

      const CommandID name;
      const wxString label_in;
      bool hasDialog;
      CommandHandlerFinder finder;
      CommandFunctorPointer callback;
      CommandFlag flags;
      CommandManager::Options options;
   };

   struct CommandGroupItem final : BaseItem {
      CommandGroupItem(const wxString &name_,
               std::initializer_list< ComponentInterfaceSymbol > items_,
               CommandHandlerFinder finder_,
               CommandFunctorPointer callback_,
               CommandFlag flags_,
               bool isEffect_);
      ~CommandGroupItem() override;

      const wxString name;
      const std::vector<ComponentInterfaceSymbol> items;
      CommandHandlerFinder finder;
      CommandFunctorPointer callback;
      CommandFlag flags;
      bool isEffect;
   };

   // For manipulating the enclosing menu or sub-menu directly,
   // adding any number of items, not using the CommandManager
   struct SpecialItem final : BaseItem
   {
      using Appender = std::function< void( AudacityProject&, wxMenu& ) >;

      explicit SpecialItem( const Appender &fn_ )
      : fn{ fn_ }
      {}
      ~SpecialItem() override;

      Appender fn;
   };

   // Following are the functions to use directly in writing table definitions.

   // Group items can be constructed two ways.
   // Pointers to subordinate items are moved into the result.
   // Null pointers are permitted, and ignored when building the menu.
   // Items are spliced into the enclosing menu
   template< typename... Args >
   inline BaseItemPtr Items( Args&&... args )
         { return std::make_unique<GroupItem>(
            std::forward<Args>(args)... ); }

   // Menu items can be constructed two ways, as for group items
   // Items will appear in a main toolbar menu or in a sub-menu
   template< typename... Args >
   inline BaseItemPtr Menu(
      const wxString &title, Args&&... args )
         { return std::make_unique<MenuItem>(
            title, std::forward<Args>(args)... ); }
   inline BaseItemPtr Menu(
      const wxString &title, BaseItemPtrs &&items )
         { return std::make_unique<MenuItem>( title, std::move( items ) ); }

   // Conditional group items can be constructed two ways, as for group items
   // These items register in the CommandManager but are not shown in menus
   template< typename... Args >
      inline BaseItemPtr ConditionalItems(
         ConditionalGroupItem::Condition condition, Args&&... args )
         { return std::make_unique<ConditionalGroupItem>(
            condition, std::forward<Args>(args)... ); }
   inline BaseItemPtr ConditionalItems(
      ConditionalGroupItem::Condition condition, BaseItemPtrs &&items )
         { return std::make_unique<ConditionalGroupItem>(
            condition, std::move( items ) ); }

   // Make either a menu item or just a group, depending on the nonemptiness
   // of the title
   template< typename... Args >
   inline BaseItemPtr MenuOrItems(
      const wxString &title, Args&&... args )
         {  if ( title.empty() ) return Items( std::forward<Args>(args)... );
            else return std::make_unique<MenuItem>(
               title, std::forward<Args>(args)... ); }
   inline BaseItemPtr MenuOrItems(
      const wxString &title, BaseItemPtrs &&items )
         {  if ( title.empty() ) return Items( std::move( items ) );
            else return std::make_unique<MenuItem>( title, std::move( items ) ); }

   inline std::unique_ptr<SeparatorItem> Separator()
      { return std::make_unique<SeparatorItem>(); }

   inline std::unique_ptr<CommandItem> Command(
      const CommandID &name, const wxString &label_in, bool hasDialog,
      CommandHandlerFinder finder, CommandFunctorPointer callback,
      CommandFlag flags, const CommandManager::Options &options = {})
   {
      return std::make_unique<CommandItem>(
         name, label_in, hasDialog, finder, callback, flags, options
      );
   }

   inline std::unique_ptr<CommandGroupItem> CommandGroup(
      const wxString &name,
      std::initializer_list< ComponentInterfaceSymbol > items,
      CommandHandlerFinder finder, CommandFunctorPointer callback,
      CommandFlag flags, bool isEffect = false)
   {
      return std::make_unique<CommandGroupItem>(
         name, items, finder, callback, flags, isEffect
      );
   }

   inline std::unique_ptr<SpecialItem> Special(
      const SpecialItem::Appender &fn )
         { return std::make_unique<SpecialItem>( fn ); }
}

#endif
