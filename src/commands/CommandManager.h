/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include "../Experimental.h"

#include <wx/string.h>
#include <wx/dynarray.h>
#include <wx/menu.h>
#include <wx/hashmap.h>

#include "../AudacityApp.h"
#include "../xml/XMLTagHandler.h"

#include "audacity/Types.h"

class AUDACITY_DLL_API CommandFunctor
{
public:
   CommandFunctor(){};
   virtual ~CommandFunctor(){};
   virtual void operator()(int index = 0, const wxEvent *e = NULL) = 0;
};

struct MenuBarListEntry
{
   wxString name;
   wxMenuBar *menubar;
};

struct SubMenuListEntry
{
   wxString name;
   wxMenu *menu;
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
   CommandFunctor *callback;
   bool multi;
   int index;
   int count;
   bool enabled;
   bool skipKeydown;
   bool wantKeyup;
   bool isGlobal;
   wxUint32 flags;
   wxUint32 mask;
};

WX_DEFINE_USER_EXPORTED_ARRAY(MenuBarListEntry *, MenuBarList, class AUDACITY_DLL_API);
WX_DEFINE_USER_EXPORTED_ARRAY(SubMenuListEntry *, SubMenuList, class AUDACITY_DLL_API);
WX_DEFINE_USER_EXPORTED_ARRAY(CommandListEntry *, CommandList, class AUDACITY_DLL_API);

WX_DECLARE_STRING_HASH_MAP_WITH_DECL(CommandListEntry *, CommandNameHash, class AUDACITY_DLL_API);
WX_DECLARE_HASH_MAP_WITH_DECL(int, CommandListEntry *, wxIntegerHash, wxIntegerEqual, CommandIDHash, class AUDACITY_DLL_API);

class AudacityProject;

class AUDACITY_DLL_API CommandManager: public XMLTagHandler
{
 public:

   //
   // Constructor / Destructor
   //

   CommandManager();
   virtual ~CommandManager();

   void PurgeData();

   //
   // Creating menus and adding commands
   //

   wxMenuBar *AddMenuBar(const wxString & sMenu);

   void BeginMenu(const wxString & tName);
   void EndMenu();

   wxMenu* BeginSubMenu(const wxString & tName);
   void EndSubMenu();
   void SetToMenu( wxMenu * menu ){
      mCurrentMenu = menu;
   };

   void InsertItem(const wxString & name,
                   const wxString & label,
                   CommandFunctor *callback,
                   const wxString & after,
                   int checkmark = -1);

   void AddItemList(const wxString & name,
                    const wxArrayString & labels,
                    CommandFunctor *callback);

   void AddCheck(const wxChar *name,
                 const wxChar *label,
                 CommandFunctor *callback,
                 int checkmark = 0);

   void AddCheck(const wxChar *name,
                 const wxChar *label,
                 CommandFunctor *callback,
                 int checkmark,
                 unsigned int flags,
                 unsigned int mask);

   void AddItem(const wxChar *name,
                const wxChar *label,
                CommandFunctor *callback,
                unsigned int flags = NoFlagsSpecifed,
                unsigned int mask = NoFlagsSpecifed);

   void AddItem(const wxChar *name,
                const wxChar *label_in,
                CommandFunctor *callback,
                const wxChar *accel,
                unsigned int flags = NoFlagsSpecifed,
                unsigned int mask = NoFlagsSpecifed,
                int checkmark = -1);

   void AddSeparator();

   // A command doesn't actually appear in a menu but might have a
   // keyboard shortcut.
   void AddCommand(const wxChar *name,
                   const wxChar *label,
                   CommandFunctor *callback,
                   unsigned int flags = NoFlagsSpecifed,
                   unsigned int mask = NoFlagsSpecifed);

   void AddCommand(const wxChar *name,
                   const wxChar *label,
                   CommandFunctor *callback,
                   const wxChar *accel,
                   unsigned int flags = NoFlagsSpecifed,
                   unsigned int mask = NoFlagsSpecifed);

   void AddGlobalCommand(const wxChar *name,
                         const wxChar *label,
                         CommandFunctor *callback,
                         const wxChar *accel);
   //
   // Command masks
   //

   // For new items/commands
   void SetDefaultFlags(wxUint32 flags, wxUint32 mask);

   void SetCommandFlags(wxString name, wxUint32 flags, wxUint32 mask);
   void SetCommandFlags(const wxChar **names,
                        wxUint32 flags, wxUint32 mask);
   // Pass multiple command names as const wxChar *, terminated by NULL
   void SetCommandFlags(wxUint32 flags, wxUint32 mask, ...);

   //
   // Modifying menus
   //

   void EnableUsingFlags(wxUint32 flags, wxUint32 mask);
   void Enable(wxString name, bool enabled);
   void Check(wxString name, bool checked);
   void Modify(wxString name, wxString newLabel);

   //
   // Modifying accelerators
   //

   void SetKeyFromName(wxString name, wxString key);
   void SetKeyFromIndex(int i, wxString key);

   //
   // Executing commands
   //

   // "permit" allows filtering even if the active window isn't a child of the project.
   // Lyrics and MixerTrackCluster classes use it.
   bool FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit = false);
   bool HandleMenuID(int id, wxUint32 flags, wxUint32 mask);
   bool HandleTextualCommand(wxString & Str, wxUint32 flags, wxUint32 mask);

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

   wxString GetLabelFromName(wxString name);
   wxString GetPrefixedLabelFromName(wxString name);
   wxString GetCategoryFromName(wxString name);
   wxString GetKeyFromName(wxString name);
   wxString GetDefaultKeyFromName(wxString name);

   bool GetEnabled(const wxString &name);

#if defined(__WXDEBUG__)
   void CheckDups();
#endif

   //
   // Loading/Saving
   //

   virtual void WriteXML(XMLWriter &xmlFile);

protected:

   //
   // Creating menus and adding commands
   //

   int NextIdentifier(int ID);
   CommandListEntry *NewIdentifier(const wxString & name,
                                   const wxString & label,
                                   wxMenu *menu,
                                   CommandFunctor *callback,
                                   bool multi,
                                   int index,
                                   int count);
   CommandListEntry *NewIdentifier(const wxString & name,
                                   const wxString & label,
                                   const wxString & accel,
                                   wxMenu *menu,
                                   CommandFunctor *callback,
                                   bool multi,
                                   int index,
                                   int count);

   //
   // Executing commands
   //

   bool HandleCommandEntry(const CommandListEntry * entry, wxUint32 flags, wxUint32 mask, const wxEvent * evt = NULL);
   void TellUserWhyDisallowed(wxUint32 flagsGot, wxUint32 flagsRequired);

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

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);

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
   wxMenu * mCurrentMenu;

   wxUint32 mDefaultFlags;
   wxUint32 mDefaultMask;
};

#endif
