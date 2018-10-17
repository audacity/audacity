/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include "../Experimental.h"

#include "CommandFunctors.h"
#include "CommandFlag.h"

#include "../MemoryX.h"
#include "Keyboard.h"
#include <vector>
#include <wx/string.h>
#include <wx/menu.h>
#include <wx/hashmap.h>

#include "../xml/XMLTagHandler.h"

#include "audacity/Types.h"

#include <unordered_map>

using CommandParameter = wxString;
class TranslatedInternalString;

struct MenuBarListEntry
{
   MenuBarListEntry(const wxString &name_, wxMenuBar *menubar_)
      : name(name_), menubar(menubar_)
   {}

   wxString name;
   wxMenuBar *menubar; // This structure does not assume memory ownership!
};

struct SubMenuListEntry
{
   SubMenuListEntry(const wxString &name_, std::unique_ptr<wxMenu> &&menu_)
      : name(name_), menu( std::move(menu_) )
   {}

   SubMenuListEntry(SubMenuListEntry &&that)
      : name(std::move(that.name))
      , menu(std::move(that.menu))
   {
   }

   wxString name;
   std::unique_ptr<wxMenu> menu;
};

struct CommandListEntry
{
   int id;
   wxString name;
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
   void BeginMenu(const wxString & tName);
   void EndMenu();

   wxMenu* BeginSubMenu(const wxString & tName);
   void EndSubMenu();

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

      const wxChar *accel{ wxT("") };
      int check{ -1 }; // default value means it's not a check item
      bool bIsEffect{ false };
      CommandParameter parameter{};
      CommandMask mask{ NoFlagsSpecified };
      wxString longName{}; // translated
   };

   void AddItemList(const wxString & name,
                    const TranslatedInternalString items[],
                    size_t nItems,
                    CommandHandlerFinder finder,
                    CommandFunctorPointer callback,
                    CommandFlag flags,
                    bool bIsEffect = false);

   void AddItem(const wxChar *name,
                const wxChar *label_in,
                bool hasDialog,
                CommandHandlerFinder finder,
                CommandFunctorPointer callback,
                CommandFlag flags,
                const Options &options = {});

   void AddSeparator();

   // A command doesn't actually appear in a menu but might have a
   // keyboard shortcut.
   void AddCommand(const wxChar *name,
                   const wxChar *label,
                   CommandHandlerFinder finder,
                   CommandFunctorPointer callback,
                   CommandFlag flags);

   void AddCommand(const wxChar *name,
                   const wxChar *label,
                   CommandHandlerFinder finder,
                   CommandFunctorPointer callback,
                   const wxChar *accel,
                   CommandFlag flags);

   void AddGlobalCommand(const wxChar *name,
                         const wxChar *label,
                         bool hasDialog,
                         CommandHandlerFinder finder,
                         CommandFunctorPointer callback,
                         const wxChar *accel);
   //
   // Command masks
   //

   // For NEW items/commands
   void SetDefaultFlags(CommandFlag flags, CommandMask mask = NoFlagsSpecified);
   CommandFlag GetDefaultFlags() const { return mDefaultFlags; }
   CommandMask GetDefaultMask() const { return mDefaultMask; }

   void SwapMenuBars();
   void SetOccultCommands( bool bOccult);


   void SetCommandFlags(const wxString &name, CommandFlag flags, CommandMask mask);

   //
   // Modifying menus
   //

   void EnableUsingFlags(CommandFlag flags, CommandMask mask);
   void Enable(const wxString &name, bool enabled);
   void Check(const wxString &name, bool checked);
   void Modify(const wxString &name, const wxString &newLabel);

   // You may either called SetCurrentMenu later followed by ClearCurrentMenu,
   // or else BeginMenu followed by EndMenu.  Don't mix them.
   void SetCurrentMenu(wxMenu * menu);
   void ClearCurrentMenu();

   //
   // Modifying accelerators
   //

   void SetKeyFromName(const wxString &name, const NormalizedKeyString &key);
   void SetKeyFromIndex(int i, const NormalizedKeyString &key);

   //
   // Executing commands
   //

   // "permit" allows filtering even if the active window isn't a child of the project.
   // Lyrics and MixerTrackCluster classes use it.
   bool FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit = false);
   bool HandleMenuID(int id, CommandFlag flags, CommandMask mask);
   bool HandleTextualCommand(const wxString & Str, const CommandContext & context, CommandFlag flags, CommandMask mask);

   //
   // Accessing
   //

   void GetCategories(wxArrayString &cats);
   void GetAllCommandNames(wxArrayString &names, bool includeMultis) const;
   void GetAllCommandLabels(
      wxArrayString &labels, std::vector<bool> &vHasDialog,
      bool includeMultis) const;
   void GetAllCommandData(
      wxArrayString &names,
      std::vector<NormalizedKeyString> &keys,
      std::vector<NormalizedKeyString> &default_keys,
      wxArrayString &labels, wxArrayString &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
      wxArrayString &prefixes,
#endif
      bool includeMultis);

   wxString GetNameFromID( int id );
   wxString GetLabelFromName(const wxString &name);
   wxString GetPrefixedLabelFromName(const wxString &name);
   wxString GetCategoryFromName(const wxString &name);
   NormalizedKeyString GetKeyFromName(const wxString &name) const;
   NormalizedKeyString GetDefaultKeyFromName(const wxString &name);

   bool GetEnabled(const wxString &name);

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

protected:

   //
   // Creating menus and adding commands
   //

   int NextIdentifier(int ID);
   CommandListEntry *NewIdentifier(const wxString & name,
                                   const wxString & label,
                                   const wxString & longLabel,
                                   bool hasDialog,
                                   wxMenu *menu,
                                   CommandHandlerFinder finder,
                                   CommandFunctorPointer callback,
                                   const wxString &nameSuffix,
                                   int index,
                                   int count,
                                   bool bIsEffect);
   CommandListEntry *NewIdentifier(const wxString & name,
                                   const wxString & label,
                                   const wxString & longLabel,
                                   bool hasDialog,
                                   const wxString & accel,
                                   wxMenu *menu,
                                   CommandHandlerFinder finder,
                                   CommandFunctorPointer callback,
                                   const wxString &nameSuffix,
                                   int index,
                                   int count,
                                   bool bIsEffect,
                                   const CommandParameter &parameter);

   //
   // Executing commands
   //

   bool HandleCommandEntry(const CommandListEntry * entry, CommandFlag flags, CommandMask mask, const wxEvent * evt = NULL);

   //
   // Modifying
   //

   void Enable(CommandListEntry *entry, bool enabled);

   //
   // Accessing
   //

   wxMenuBar * CurrentMenuBar() const;
   wxMenuBar * GetMenuBar(const wxString & sMenu) const;
   wxMenu * CurrentSubMenu() const;
   wxMenu * CurrentMenu() const;
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

   CommandFlag mDefaultFlags;
   CommandMask mDefaultMask;
   bool bMakingOccultCommands;
};

#endif
