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

#include "../MemoryX.h"
#include <vector>
#include <wx/string.h>
#include <wx/dynarray.h>
#include <wx/menu.h>
#include <wx/hashmap.h>

#include "../AudacityApp.h"
#include "../xml/XMLTagHandler.h"

#include "audacity/Types.h"

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
   wxString key;
   wxString defaultKey;
   wxString label;
   wxString labelPrefix;
   wxString labelTop;
   wxMenu *menu;
   CommandFunctorPointer callback;
   bool multi;
   int index;
   int count;
   bool enabled;
   bool skipKeydown;
   bool wantKeyup;
   bool isGlobal;
   CommandFlag flags;
   CommandMask mask;
};

using MenuBarList = std::vector < MenuBarListEntry >;

// to do: remove the extra indirection when Mac compiler moves to newer version
using SubMenuList = std::vector < movable_ptr<SubMenuListEntry> >;

// This is an array of pointers, not structures, because the hash maps also point to them,
// so we don't want the structures to relocate with vector operations.
using CommandList = std::vector<movable_ptr<CommandListEntry>>;

WX_DECLARE_STRING_HASH_MAP_WITH_DECL(CommandListEntry *, CommandNameHash, class AUDACITY_DLL_API);
WX_DECLARE_HASH_MAP_WITH_DECL(int, CommandListEntry *, wxIntegerHash, wxIntegerEqual, CommandIDHash, class AUDACITY_DLL_API);

class AudacityProject;

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

   void PurgeData();

   //
   // Creating menus and adding commands
   //

   std::unique_ptr<wxMenuBar> AddMenuBar(const wxString & sMenu);

   void BeginMenu(const wxString & tName);
   void EndMenu();

   wxMenu* BeginSubMenu(const wxString & tName);
   void EndSubMenu();

   void InsertItem(const wxString & name,
                   const wxString & label,
                   const CommandFunctorPointer &callback,
                   const wxString & after,
                   int checkmark = -1);

   void AddItemList(const wxString & name,
                    const wxArrayString & labels,
                    const CommandFunctorPointer &callback);

   void AddCheck(const wxChar *name,
                 const wxChar *label,
                 const CommandFunctorPointer &callback,
                 int checkmark = 0);

   void AddCheck(const wxChar *name,
                 const wxChar *label,
                 const CommandFunctorPointer &callback,
                 int checkmark,
                 CommandFlag flags,
                 CommandMask mask);

   void AddItem(const wxChar *name,
                const wxChar *label,
                const CommandFunctorPointer &callback,
                CommandFlag flags = NoFlagsSpecifed,
                CommandMask mask   = NoFlagsSpecifed);

   void AddItem(const wxChar *name,
                const wxChar *label_in,
                const CommandFunctorPointer &callback,
                const wxChar *accel,
                CommandFlag flags = NoFlagsSpecifed,
                CommandMask mask   = NoFlagsSpecifed,
                int checkmark = -1);

   void AddSeparator();

   // A command doesn't actually appear in a menu but might have a
   // keyboard shortcut.
   void AddCommand(const wxChar *name,
                   const wxChar *label,
                   const CommandFunctorPointer &callback,
                   CommandFlag flags = NoFlagsSpecifed,
                   CommandMask mask   = NoFlagsSpecifed);

   void AddCommand(const wxChar *name,
                   const wxChar *label,
                   const CommandFunctorPointer &callback,
                   const wxChar *accel,
                   CommandFlag flags = NoFlagsSpecifed,
                   CommandMask mask   = NoFlagsSpecifed);

   void AddGlobalCommand(const wxChar *name,
                         const wxChar *label,
                         const CommandFunctorPointer &callback,
                         const wxChar *accel);
   //
   // Command masks
   //

   // For NEW items/commands
   void SetDefaultFlags(CommandFlag flags, CommandMask mask);
   CommandFlag GetDefaultFlags() const { return mDefaultFlags; }
   CommandMask GetDefaultMask() const { return mDefaultMask; }

   void SetCommandFlags(const wxString &name, CommandFlag flags, CommandMask mask);
   void SetCommandFlags(const wxChar **names,
                        CommandFlag flags, CommandMask mask);
   // Pass multiple command names as const wxChar *, terminated by NULL
   void SetCommandFlags(CommandFlag flags, CommandMask mask, ...);

   //
   // Modifying menus
   //

   void EnableUsingFlags(CommandFlag flags, CommandMask mask);
   void Enable(const wxString &name, bool enabled);
   void Check(const wxString &name, bool checked);
   void Modify(const wxString &name, const wxString &newLabel);

   //
   // Modifying accelerators
   //

   void SetKeyFromName(const wxString &name, const wxString &key);
   void SetKeyFromIndex(int i, const wxString &key);

   //
   // Executing commands
   //

   // "permit" allows filtering even if the active window isn't a child of the project.
   // Lyrics and MixerTrackCluster classes use it.
   bool FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit = false);
   bool HandleMenuID(int id, CommandFlag flags, CommandMask mask);
   bool HandleTextualCommand(wxString & Str, CommandFlag flags, CommandMask mask);

   //
   // Accessing
   //

   void GetCategories(wxArrayString &cats);
   void GetAllCommandNames(wxArrayString &names, bool includeMultis);
   void GetAllCommandLabels(wxArrayString &labels, bool includeMultis);
   void GetAllCommandData(
      wxArrayString &names, wxArrayString &keys, wxArrayString &default_keys,
      wxArrayString &labels, wxArrayString &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
      wxArrayString &prefixes,
#endif
      bool includeMultis);

   wxString GetLabelFromName(const wxString &name);
   wxString GetPrefixedLabelFromName(const wxString &name);
   wxString GetCategoryFromName(const wxString &name);
   wxString GetKeyFromName(const wxString &name);
   wxString GetDefaultKeyFromName(const wxString &name);

   bool GetEnabled(const wxString &name);

#if defined(__WXDEBUG__)
   void CheckDups();
#endif

   //
   // Loading/Saving
   //

   void WriteXML(XMLWriter &xmlFile) /* not override */;

protected:

   //
   // Creating menus and adding commands
   //

   int NextIdentifier(int ID);
   CommandListEntry *NewIdentifier(const wxString & name,
                                   const wxString & label,
                                   wxMenu *menu,
                                   const CommandFunctorPointer &callback,
                                   bool multi,
                                   int index,
                                   int count);
   CommandListEntry *NewIdentifier(const wxString & name,
                                   const wxString & label,
                                   const wxString & accel,
                                   wxMenu *menu,
                                   const CommandFunctorPointer &callback,
                                   bool multi,
                                   int index,
                                   int count);

   //
   // Executing commands
   //

   bool HandleCommandEntry(const CommandListEntry * entry, CommandFlag flags, CommandMask mask, const wxEvent * evt = NULL);
   void TellUserWhyDisallowed(CommandFlag flagsGot, CommandFlag flagsRequired);

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

   //
   // Loading/Saving
   //

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;

private:
   MenuBarList  mMenuBarList;
   SubMenuList  mSubMenuList;
   CommandList  mCommandList;
   CommandNameHash  mCommandNameHash;
   CommandNameHash  mCommandKeyHash;
   CommandIDHash  mCommandIDHash;
   int mCurrentID;
   int mXMLKeysRead;

   bool mbSeparatorAllowed; // false at the start of a menu and immediately after a separator.

   wxString mCurrentMenuName;
   std::unique_ptr<wxMenu> mCurrentMenu;

   CommandFlag mDefaultFlags;
   CommandMask mDefaultMask;
};

#endif
