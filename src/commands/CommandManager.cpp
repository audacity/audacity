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

#include "../Audacity.h"
#include "CommandManager.h"

#include <wx/defs.h>
#include <wx/hash.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/tokenzr.h>

#include "../AudacityApp.h"
#include "../Prefs.h"
#include "../Project.h"

#include "Keyboard.h"
#include "../PluginManager.h"
#include "../effects/EffectManager.h"

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

#define COMMAND _("Command")

#if defined(__WXMAC__)
#include <AppKit/AppKit.h>
#include <wx/osx/private.h>
#elif defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#include "../Experimental.h"

// Shared by all projects
static class CommandManagerEventMonitor final : public wxEventFilter
{
public:
   CommandManagerEventMonitor()
   :  wxEventFilter()
   {
#if defined(__WXMAC__)
      // In wx3, the menu accelerators take precendence over key event processing
      // so we won't get wxEVT_CHAR_HOOK events for combinations assigned to menus.
      // Since we only support OS X 10.6 or greater, we can use an event monitor
      // to capture the key event before it gets to the normal wx3 processing.

      // The documentation for addLocalMonitorForEventsMatchingMask implies that
      // NSKeyUpMask can't be used in 10.6, but testing shows that it can.
      NSEventMask mask = NSKeyDownMask | NSKeyUpMask;

      mHandler =
      [
         NSEvent addLocalMonitorForEventsMatchingMask:mask handler:^NSEvent *(NSEvent *event)
         {
            WXWidget widget = (WXWidget) [[event window] firstResponder];
            if (widget)
            {
               wxWidgetCocoaImpl *impl = (wxWidgetCocoaImpl *)
                  wxWidgetImpl::FindFromWXWidget(widget);
               if (impl)
               {
                  mEvent = event;

                  wxKeyEvent wxevent([event type] == NSKeyDown ? wxEVT_KEY_DOWN : wxEVT_KEY_UP);
                  impl->SetupKeyEvent(wxevent, event);

                  NSEvent *result;
                  if ([event type] == NSKeyDown)
                  {
                     wxKeyEvent eventHook(wxEVT_CHAR_HOOK, wxevent);
                     result = FilterEvent(eventHook) == Event_Processed ? nil : event;
                  }
                  else
                  {
                     result = FilterEvent(wxevent) == Event_Processed ? nil : event;
                  }

                  mEvent = nullptr;
                  return result;
               }
            }

            return event;
         }
      ];

      // Bug1252: must also install this filter with wxWidgets, else
      // we don't intercept command keys when focus is in a combo box.
      wxEvtHandler::AddFilter(this);
#else

      wxEvtHandler::AddFilter(this);

#endif
   }

   virtual ~CommandManagerEventMonitor()
   {
#if defined(__WXMAC__)
      [NSEvent removeMonitor:mHandler];
#else
      wxEvtHandler::RemoveFilter(this);
#endif
   }

   int FilterEvent(wxEvent& event) override
   {
      // Quickly bail if this isn't something we want.
      wxEventType type = event.GetEventType();
      if (type != wxEVT_CHAR_HOOK && type != wxEVT_KEY_UP)
      {
         return Event_Skip;
      }

      // We must have a project since we will be working with the Command Manager
      // and capture handler, both of which are (currently) tied to individual projects.
      //
      // Shouldn't they be tied to the application instead???
      AudacityProject *project = GetActiveProject();
      if (!project || !project->IsEnabled())
      {
         return Event_Skip;
      }

      // Make a copy of the event and (possibly) make it look like a key down
      // event.
      wxKeyEvent key = (wxKeyEvent &) event;
      if (type == wxEVT_CHAR_HOOK)
      {
         key.SetEventType(wxEVT_KEY_DOWN);
      }

      // Give the capture handler first dibs at the event.
      wxWindow *handler = project->GetKeyboardCaptureHandler();
      if (handler && HandleCapture(handler, key))
      {
         return Event_Processed;
      }

      // Capture handler didn't want it, so ask the Command Manager.
      CommandManager *manager = project->GetCommandManager();
      if (manager && manager->FilterKeyEvent(project, key))
      {
         return Event_Processed;
      }

      // Give it back to WX for normal processing.
      return Event_Skip;
   }

private:

   // Returns true if the event was captured and processed
   bool HandleCapture(wxWindow *target, const wxKeyEvent & event)
   {
      if (wxGetTopLevelParent(target) != wxGetTopLevelParent(wxWindow::FindFocus()))
      {
         return false;
      }
      wxEvtHandler *handler = target->GetEventHandler();

      // We make a copy of the event because the capture handler may modify it.
      wxKeyEvent temp = event;

#if defined(__WXGTK__)
      // wxGTK uses the control and alt modifiers to represent ALTGR,
      // so remove it as it might confuse the capture handlers.
      if (temp.GetModifiers() == (wxMOD_CONTROL | wxMOD_ALT))
      {
         temp.SetControlDown(false);
         temp.SetAltDown(false);
      }
#endif

      // Ask the capture handler if the key down/up event is something they it
      // might be interested in handling.
      wxCommandEvent e(EVT_CAPTURE_KEY);
      e.SetEventObject(&temp);
      e.StopPropagation();
      if (!handler->ProcessEvent(e))
      {
         return false;
      }

      // Now, let the handler process the normal key event.
      bool keyDown = temp.GetEventType() == wxEVT_KEY_DOWN;
      temp.WasProcessed();
      temp.StopPropagation();
      wxEventProcessInHandlerOnly onlyDown(temp, handler);
      bool processed = handler->ProcessEvent(temp);

      // Don't go any further if the capture handler didn't process
      // the key down event.
      if (!processed && keyDown)
      {
         return false;
      }

      // At this point the capture handler has either processed a key down event
      // or we're dealing with a key up event.
      //
      // So, only generate the char events for key down events.
      if (keyDown)
      {
         wxString chars = GetUnicodeString(temp);
         for (size_t i = 0, cnt = chars.Length(); i < cnt; i++)
         {
            temp = event;
            temp.SetEventType(wxEVT_CHAR);
            temp.WasProcessed();
            temp.StopPropagation();
            temp.m_uniChar = chars[i];
            wxEventProcessInHandlerOnly onlyChar(temp, handler);
            handler->ProcessEvent(temp);
         }
      }

      // We get here for processed key down events or for key up events, whether
      // processed or not.
      return true;
   }

   // Convert the key down event to a unicode string.
   wxString GetUnicodeString(const wxKeyEvent & event)
   {
      wxString chars;

#if defined(__WXMSW__)

      BYTE ks[256];
      GetKeyboardState(ks);
      WCHAR ucode[256];
      int res = ToUnicode(event.GetRawKeyCode(), 0, ks, ucode, 256, 0);
      if (res >= 1)
      {
         chars.Append(ucode, res);
      }

#elif defined(__WXGTK__)

      chars.Append((wxChar) gdk_keyval_to_unicode(event.GetRawKeyCode()));
                                     
#elif defined(__WXMAC__)

      if (!mEvent) {
         // TODO:  we got here without getting the NSEvent pointer,
         // as in the combo box case of bug 1252.  We can't compute it!
         // This makes a difference only when there is a capture handler.
         // It's never the case yet that there is one.
         wxASSERT(false);
         return chars;
      }

      NSString *c = [mEvent charactersIgnoringModifiers];
      if ([c length] == 1)
      {
         unichar codepoint = [c characterAtIndex:0];
         if ((codepoint >= 0xF700 && codepoint <= 0xF8FF) || codepoint == 0x7F)
         {
            return chars;
         }
      }

      c = [mEvent characters];
      chars = [c UTF8String];

      TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardInputSource();
      CFDataRef uchr = (CFDataRef)TISGetInputSourceProperty(currentKeyboard, kTISPropertyUnicodeKeyLayoutData);
      CFRelease(currentKeyboard);
      if (uchr == NULL)
      {
         return chars;
      }

      const UCKeyboardLayout *keyboardLayout = (const UCKeyboardLayout*)CFDataGetBytePtr(uchr);
      if (keyboardLayout == NULL)
      {
         return chars;
      }
      
      const UniCharCount maxStringLength = 255;
      UniCharCount actualStringLength = 0;
      UniChar unicodeString[maxStringLength];
      UInt32 nsflags = [mEvent modifierFlags];
      UInt16 modifiers = (nsflags & NSAlphaShiftKeyMask ? alphaLock : 0) |
                         (nsflags & NSShiftKeyMask ? shiftKey : 0) |
                         (nsflags & NSControlKeyMask ? controlKey : 0) |
                         (nsflags & NSAlternateKeyMask ? optionKey : 0) |
                         (nsflags & NSCommandKeyMask ? cmdKey : 0);
         
      OSStatus status = UCKeyTranslate(keyboardLayout,
                                       [mEvent keyCode],
                                       kUCKeyActionDown,
                                       (modifiers >> 8) & 0xff,
                                       LMGetKbdType(),
                                       0,
                                       &mDeadKeyState,
                                       maxStringLength,
                                       &actualStringLength,
                                       unicodeString);
     
      if (status != noErr)
      {
         return chars;
      }
      
      chars = [[NSString stringWithCharacters:unicodeString
                                       length:(NSInteger)actualStringLength] UTF8String];

#endif

      return chars;
   }

private:

#if defined(__WXMAC__)   
   id mHandler;
   NSEvent *mEvent {};
   UInt32 mDeadKeyState;
#endif

} monitor;

///
///  Standard Constructor
///
CommandManager::CommandManager():
   mCurrentID(17000),
   mCurrentMenuName(COMMAND),
   mDefaultFlags(AlwaysEnabledFlag),
   mDefaultMask(AlwaysEnabledFlag)
{
   mbSeparatorAllowed = false;
}

///
///  Class Destructor.  Includes PurgeData, which removes
///  menubars
CommandManager::~CommandManager()
{
   //WARNING: This removes menubars that could still be assigned to windows!
   PurgeData();
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
   mCommandIDHash.clear();

   mCurrentMenuName = COMMAND;
   mCurrentID = 0;
}


///
/// Makes a NEW menubar for placement on the top of a project
/// Names it according to the passed-in string argument.
///
/// If the menubar already exists, that's unexpected.
std::unique_ptr<wxMenuBar> CommandManager::AddMenuBar(const wxString & sMenu)
{
   wxMenuBar *menuBar = GetMenuBar(sMenu);
   if (menuBar) {
      wxASSERT(false);
      return {};
   }

   auto result = std::make_unique<wxMenuBar>();
#ifdef __AUDACITY_OLD_STD__
   mMenuBarList.push_back(MenuBarListEntry{sMenu, result.get()});
#else
   mMenuBarList.emplace_back(sMenu, result.get());
#endif

   return result;
}


///
/// Retrieves the menubar based on the name given in AddMenuBar(name)
///
wxMenuBar * CommandManager::GetMenuBar(const wxString & sMenu) const
{
   for (const auto &entry : mMenuBarList)
   {
      if(!entry.name.Cmp(sMenu))
         return entry.menubar;
   }

   return NULL;
}


///
/// Retrieve the 'current' menubar; either NULL or the
/// last on in the mMenuBarList.
wxMenuBar * CommandManager::CurrentMenuBar() const
{
   if(mMenuBarList.empty())
      return NULL;

   return mMenuBarList.back().menubar;
}


///
/// This starts a NEW menu
///
void CommandManager::BeginMenu(const wxString & tName)
{
   mCurrentMenu = std::make_unique<wxMenu>();
   mCurrentMenuName = tName;
}


///
/// This attaches a menu to the menubar and ends the menu
///
void CommandManager::EndMenu()
{
   // Add the menu to the menubar after all menu items have been
   // added to the menu to allow OSX to rearrange special menu
   // items like Preferences, About, and Quit.
   CurrentMenuBar()->Append(mCurrentMenu.release(), mCurrentMenuName);
   mCurrentMenuName = COMMAND;
}


///
/// This starts a NEW submenu, and names it according to
/// the function's argument.
wxMenu* CommandManager::BeginSubMenu(const wxString & tName)
{
   mSubMenuList.push_back
      (make_movable< SubMenuListEntry > ( tName, std::make_unique<wxMenu>() ));
   mbSeparatorAllowed = false;
   return mSubMenuList.back()->menu.get();
}


///
/// This function is called after the final item of a SUBmenu is added.
/// Submenu items are added just like regular menu items; they just happen
/// after BeginSubMenu() is called but before EndSubMenu() is called.
void CommandManager::EndSubMenu()
{
   //Save the submenu's information
   SubMenuListEntry tmpSubMenu { std::move( *mSubMenuList.back() ) };

   //Pop off the NEW submenu so CurrentMenu returns the parent of the submenu
   mSubMenuList.pop_back();

   //Add the submenu to the current menu
   CurrentMenu()->Append
      (0, tmpSubMenu.name, tmpSubMenu.menu.release(), tmpSubMenu.name);
   mbSeparatorAllowed = true;
}


///
/// This returns the 'Current' Submenu, which is the one at the
///  end of the mSubMenuList (or NULL, if it doesn't exist).
wxMenu * CommandManager::CurrentSubMenu() const
{
   if(mSubMenuList.empty())
      return NULL;

   return mSubMenuList.back()->menu.get();
}

///
/// This returns the current menu that we're appending to - note that
/// it could be a submenu if BeginSubMenu was called and we haven't
/// reached EndSubMenu yet.
wxMenu * CommandManager::CurrentMenu() const
{
   if(!mCurrentMenu)
      return NULL;

   wxMenu * tmpCurrentSubMenu = CurrentSubMenu();

   if(!tmpCurrentSubMenu)
   {
      return mCurrentMenu.get();
   }

   return tmpCurrentSubMenu;
}

///
/// Add a menu item to the current menu.  When the user selects it, the
/// given functor will be called
void CommandManager::InsertItem(const wxString & name,
                                const wxString & label_in,
                                const CommandFunctorPointer &callback,
                                const wxString & after,
                                int checkmark)
{
   wxMenuBar *bar = GetActiveProject()->GetMenuBar();
   wxArrayString names = ::wxStringTokenize(after, wxT(":"));
   size_t cnt = names.GetCount();

   if (cnt < 2) {
      return;
   }

   int pos = bar->FindMenu(names[0]);
   if (pos == wxNOT_FOUND) {
      return;
   }

   wxMenu *menu = bar->GetMenu(pos);
   wxMenuItem *item = NULL;
   pos = 0;

   for (size_t ndx = 1; ndx < cnt; ndx++) {
      wxMenuItemList list = menu->GetMenuItems();
      size_t lcnt = list.GetCount();
      wxString label = wxMenuItem::GetLabelText(names[ndx]);

      for (size_t lndx = 0; lndx < lcnt; lndx++) {
         item = list.Item(lndx)->GetData();
         if (item->GetItemLabelText() == label) {
            break;
         }
         pos++;
         item = NULL;
      }

      if (item == NULL) {
         return;
      }

      if (item->IsSubMenu()) {
         menu = item->GetSubMenu();
         item = NULL;
         continue;
      }

      if (ndx + 1 != cnt) {
         return;
      }
   }

   CommandListEntry *entry = NewIdentifier(name, label_in, menu, callback, false, 0, 0);
   int ID = entry->id;
   wxString label = GetLabel(entry);

   if (checkmark >= 0) {
      menu->InsertCheckItem(pos, ID, label);
      menu->Check(ID, checkmark != 0);
   }
   else {
      menu->Insert(pos, ID, label);
   }

   mbSeparatorAllowed = true;
}



void CommandManager::AddCheck(const wxChar *name,
                              const wxChar *label,
                              const CommandFunctorPointer &callback,
                              int checkmark)
{
   AddItem(name, label, callback, wxT(""), NoFlagsSpecifed, NoFlagsSpecifed, checkmark);
}

void CommandManager::AddCheck(const wxChar *name,
                              const wxChar *label,
                              const CommandFunctorPointer &callback,
                              int checkmark,
                              CommandFlag flags,
                              CommandMask mask)
{
   AddItem(name, label, callback, wxT(""), flags, mask, checkmark);
}

void CommandManager::AddItem(const wxChar *name,
                             const wxChar *label,
                             const CommandFunctorPointer &callback,
                             CommandFlag flags,
                             CommandMask mask)
{
   AddItem(name, label, callback, wxT(""), flags, mask);
}

void CommandManager::AddItem(const wxChar *name,
                             const wxChar *label_in,
                             const CommandFunctorPointer &callback,
                             const wxChar *accel,
                             CommandFlag flags,
                             CommandMask mask,
                             int checkmark)
{
   CommandListEntry *entry = NewIdentifier(name, label_in, accel, CurrentMenu(), callback, false, 0, 0);
   int ID = entry->id;
   wxString label = GetLabel(entry);

   if (flags != NoFlagsSpecifed || mask != NoFlagsSpecifed) {
      SetCommandFlags(name, flags, mask);
   }


   if (checkmark >= 0) {
      CurrentMenu()->AppendCheckItem(ID, label);
      CurrentMenu()->Check(ID, checkmark != 0);
   }
   else {
      CurrentMenu()->Append(ID, label);
   }

   mbSeparatorAllowed = true;
}

///
/// Add a list of menu items to the current menu.  When the user selects any
/// one of these, the given functor will be called
/// with its position in the list as the index number.
/// When you call Enable on this command name, it will enable or disable
/// all of the items at once.
void CommandManager::AddItemList(const wxString & name,
                                 const wxArrayString & labels,
                                 const CommandFunctorPointer &callback)
{
   for (size_t i = 0, cnt = labels.GetCount(); i < cnt; i++) {
      CommandListEntry *entry = NewIdentifier(name,
                                              labels[i],
                                              CurrentMenu(),
                                              callback,
                                              true,
                                              i,
                                              cnt);
      CurrentMenu()->Append(entry->id, GetLabel(entry));
      mbSeparatorAllowed = true;
   }
}

///
/// Add a command that doesn't appear in a menu.  When the key is pressed, the
/// given function pointer will be called (via the CommandManagerListener)
void CommandManager::AddCommand(const wxChar *name,
                                const wxChar *label,
                                const CommandFunctorPointer &callback,
                                CommandFlag flags,
                                CommandMask mask)
{
   AddCommand(name, label, callback, wxT(""), flags, mask);
}

void CommandManager::AddCommand(const wxChar *name,
                                const wxChar *label_in,
                                const CommandFunctorPointer &callback,
                                const wxChar *accel,
                                CommandFlag flags,
                                CommandMask mask)
{
   NewIdentifier(name, label_in, accel, NULL, callback, false, 0, 0);

   if (flags != NoFlagsSpecifed || mask != NoFlagsSpecifed) {
      SetCommandFlags(name, flags, mask);
   }
}

void CommandManager::AddGlobalCommand(const wxChar *name,
                                      const wxChar *label_in,
                                      const CommandFunctorPointer &callback,
                                      const wxChar *accel)
{
   CommandListEntry *entry = NewIdentifier(name, label_in, accel, NULL, callback, false, 0, 0);

   entry->enabled = false;
   entry->isGlobal = true;
   entry->flags = AlwaysEnabledFlag;
   entry->mask = AlwaysEnabledFlag;
}

void CommandManager::AddSeparator()
{
   if( mbSeparatorAllowed )
      CurrentMenu()->AppendSeparator();
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
CommandListEntry *CommandManager::NewIdentifier(const wxString & name,
                                                const wxString & label,
                                                wxMenu *menu,
                                                const CommandFunctorPointer &callback,
                                                bool multi,
                                                int index,
                                                int count)
{
   return NewIdentifier(name,
                        label.BeforeFirst(wxT('\t')),
                        label.AfterFirst(wxT('\t')),
                        menu,
                        callback,
                        multi,
                        index,
                        count);
}

CommandListEntry *CommandManager::NewIdentifier(const wxString & name,
   const wxString & label,
   const wxString & accel,
   wxMenu *menu,
   const CommandFunctorPointer &callback,
   bool multi,
   int index,
   int count)
{
   {
      // Make a unique_ptr or shared_ptr as appropriate:
      auto entry = make_movable<CommandListEntry>();

      wxString labelPrefix;
      if (!mSubMenuList.empty()) {
         labelPrefix = mSubMenuList.back()->name;
      }

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

#if defined(__WXMAC__)
      if (name == wxT("Preferences"))
         entry->id = wxID_PREFERENCES;
      else if (name == wxT("Exit"))
         entry->id = wxID_EXIT;
      else if (name == wxT("About"))
         entry->id = wxID_ABOUT;
#endif

      entry->name = name;
      entry->label = label;
      entry->key = KeyStringNormalize(accel.BeforeFirst(wxT('\t')));
      entry->defaultKey = entry->key;
      entry->labelPrefix = labelPrefix;
      entry->labelTop = wxMenuItem::GetLabelText(mCurrentMenuName);
      entry->menu = menu;
      entry->callback = callback;
      entry->multi = multi;
      entry->index = index;
      entry->count = count;
      entry->flags = mDefaultFlags;
      entry->mask = mDefaultMask;
      entry->enabled = true;
      entry->skipKeydown = (accel.Find(wxT("\tskipKeydown")) != wxNOT_FOUND);
      entry->wantKeyup = (accel.Find(wxT("\twantKeyup")) != wxNOT_FOUND) || entry->skipKeydown;
      entry->isGlobal = false;

      // For key bindings for commands with a list, such as effects,
      // the name in prefs is the category name plus the effect name.
      if (multi) {
         entry->name = wxString::Format(wxT("%s:%s"), name.c_str(), label.c_str());
      }

      // Key from preferences overridse the default key given
      gPrefs->SetPath(wxT("/NewKeys"));
      if (gPrefs->HasEntry(entry->name)) {
         entry->key = KeyStringNormalize(gPrefs->Read(entry->name, entry->key));
      }
      gPrefs->SetPath(wxT("/"));

      mCommandList.push_back(std::move(entry));
      // Don't use the variable entry eny more!
   }

   // New variable
   CommandListEntry *entry = &*mCommandList.back();
   mCommandIDHash[entry->id] = entry;

#if defined(__WXDEBUG__)
   CommandListEntry *prev = mCommandNameHash[entry->name];
   if (prev) {
      // Under Linux it looks as if we may ask for a newID for the same command
      // more than once.  So it's only an error if two different commands
      // have the exact same name.
      if( prev->label != entry->label )
      {
         wxLogDebug(wxT("Command '%s' defined by '%s' and '%s'"),
                    entry->name.c_str(),
                    prev->label.BeforeFirst(wxT('\t')).c_str(),
                    entry->label.BeforeFirst(wxT('\t')).c_str());
         wxFAIL_MSG(wxString::Format(wxT("Command '%s' defined by '%s' and '%s'"),
                    entry->name.c_str(),
                    prev->label.BeforeFirst(wxT('\t')).c_str(),
                    entry->label.BeforeFirst(wxT('\t')).c_str()));
      }
   }
#endif
   mCommandNameHash[entry->name] = entry;

   if (entry->key != wxT("")) {
      mCommandKeyHash[entry->key] = entry;
   }

   return entry;
}

wxString CommandManager::GetLabel(const CommandListEntry *entry) const
{
   wxString label = entry->label;
   if (!entry->key.IsEmpty())
   {
      label += wxT("\t") + entry->key;
   }

   return label;
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
   entry->enabled = entry->menu->IsEnabled(entry->id);

   // Only enabled if needed
   if (entry->enabled != enabled) {
      entry->menu->Enable(entry->id, enabled);
      entry->enabled = entry->menu->IsEnabled(entry->id);
   }

   if (entry->multi) {
      int i;
      int ID = entry->id;

      for(i=1; i<entry->count; i++) {
         ID = NextIdentifier(ID);

         // This menu item is not necessarily in the same menu, because
         // multi-items can be spread across multiple sub menus
         CommandListEntry *multiEntry = mCommandIDHash[ID];
         if (multiEntry) {
            wxMenuItem *item = multiEntry->menu->FindItem(ID);

         if (item) {
            item->Enable(enabled);
         } else {
            wxLogDebug(wxT("Warning: Menu entry with id %i in %s not found"),
                ID, (const wxChar*)entry->name);
         }
         } else {
            wxLogDebug(wxT("Warning: Menu entry with id %i not in hash"), ID);
         }
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

void CommandManager::EnableUsingFlags(CommandFlag flags, CommandMask mask)
{
   for(const auto &entry : mCommandList) {
      if (entry->multi && entry->index != 0)
         continue;

      auto combinedMask = (mask & entry->mask);
      if (combinedMask) {
         bool enable = ((flags & combinedMask) ==
                        (entry->flags & combinedMask));
         Enable(entry.get(), enable);
      }
   }
}

bool CommandManager::GetEnabled(const wxString &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry || !entry->menu) {
      wxLogDebug(wxT("Warning: command doesn't exist: '%s'"),
                 (const wxChar*)name);
      return false;
   }
   return entry->enabled;
}

void CommandManager::Check(const wxString &name, bool checked)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry || !entry->menu) {
      return;
   }

   entry->menu->Check(entry->id, checked);
}

///Changes the label text of a menu item
void CommandManager::Modify(const wxString &name, const wxString &newLabel)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry && entry->menu) {
      entry->label = newLabel;
      entry->menu->SetLabel(entry->id, GetLabel(entry));
   }
}

void CommandManager::SetKeyFromName(const wxString &name, const wxString &key)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry) {
      entry->key = KeyStringNormalize(key);
   }
}

void CommandManager::SetKeyFromIndex(int i, const wxString &key)
{
   const auto &entry = mCommandList[i];
   entry->key = KeyStringNormalize(key);
}

void CommandManager::TellUserWhyDisallowed( CommandFlag flagsGot, CommandMask flagsRequired )
{
   // The default string for 'reason' is a catch all.  I hope it won't ever be seen
   // and that we will get something more specific.
   wxString reason = _("There was a problem with your last action. If you think\nthis is a bug, please tell us exactly where it occurred.");

   auto missingFlags = flagsRequired & (~flagsGot );
   if( missingFlags & AudioIONotBusyFlag )
      reason = _("You can only do this when playing and recording are\n stopped. (Pausing is not sufficient.)");
   else if( missingFlags & StereoRequiredFlag )
      reason = _("You must first select some stereo audio for this\n to use. (You cannot use this with mono.)");
   else if( missingFlags & TimeSelectedFlag )
      reason = _("You must first select some audio for this to use.");
   else if( missingFlags & WaveTracksSelectedFlag)
      reason = _("You must first select some audio for this\n to use. (Selecting other kinds of track won't work.)");
   // If the only thing wrong was no tracks, we do nothing and don't report a problem
   else if( missingFlags == TracksExistFlag )
      return;

   wxMessageBox(reason, _("Disallowed"),  wxICON_WARNING | wxOK );
}

///
///
///
bool CommandManager::FilterKeyEvent(AudacityProject *project, const wxKeyEvent & evt, bool permit)
{
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
      entry->enabled = true;
      bool ret = HandleCommandEntry(entry, NoFlagsSpecifed, NoFlagsSpecifed, &evt);
      entry->enabled = false;
      return ret;
   }

   // Any other keypresses must be destined for this project window.
   if (!permit && 
       (wxGetTopLevelParent(wxWindow::FindFocus()) != project ||
        !wxEventLoop::GetActive()->IsMain()))
   {
      return false;
   }

   auto flags = project->GetUpdateFlags();

   wxKeyEvent temp = evt;

   if (type == wxEVT_KEY_DOWN)
   {
      if (entry->skipKeydown)
      {
         return true;
      }

      return HandleCommandEntry(entry, flags, NoFlagsSpecifed, &temp);
   }

   if (type == wxEVT_KEY_UP && entry->wantKeyup)
   {
      return HandleCommandEntry(entry, flags, NoFlagsSpecifed, &temp);
   }

   return false;
}

/// HandleCommandEntry() takes a CommandListEntry and executes it
/// returning true iff successful.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleCommandEntry(const CommandListEntry * entry,
                                        CommandFlag flags, CommandMask mask, const wxEvent * evt)
{
   if (!entry || !entry->enabled)
      return false;

   auto combinedMask = (mask & entry->mask);
   if (combinedMask) {

      AudacityProject * proj;
      proj = GetActiveProject();
      wxASSERT( proj );
      if( !proj )
         return false;

      // NB: The call may have the side effect of changing flags.
      bool allowed = proj->TryToMakeActionAllowed( flags, entry->flags, combinedMask );
      if (!allowed)
      {
         TellUserWhyDisallowed(
            flags & combinedMask, entry->flags & combinedMask);
         return false;
      }
   }

   (*(entry->callback))(entry->index, evt);

   return true;
}

///Call this when a menu event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleMenuID(int id, CommandFlag flags, CommandMask mask)
{
   CommandListEntry *entry = mCommandIDHash[id];
   return HandleCommandEntry( entry, flags, mask );
}

/// HandleTextualCommand() allows us a limitted version of script/batch
/// behavior, since we can get from a string command name to the actual
/// code to run.
bool CommandManager::HandleTextualCommand(wxString & Str, CommandFlag flags, CommandMask mask)
{
   // Linear search for now...
   for (const auto &entry : mCommandList)
   {
      if (!entry->multi)
      {
         if( Str.IsSameAs( entry->name ))
         {
            return HandleCommandEntry( entry.get(), flags, mask);
         }
      }
   }
   // Not one of the singleton commands.
   // We could/should try all the list-style commands.
   // instead we only try the effects.
   AudacityProject * proj = GetActiveProject();
   if( !proj )
   {
      return false;
   }

   PluginManager & pm = PluginManager::Get();
   EffectManager & em = EffectManager::Get();
   const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect);
   while (plug)
   {
      if (em.GetEffectByIdentifier(plug->GetID()).IsSameAs(Str))
      {
         return proj->OnEffect(plug->GetID(), AudacityProject::OnEffectFlags::kConfigured);
      }
      plug = pm.GetNextPlugin(PluginTypeEffect);
   }

   return false;
}

void CommandManager::GetCategories(wxArrayString &cats)
{
   cats.Clear();

   for (const auto &entry : mCommandList) {
      wxString cat = entry->labelTop;
      if (cats.Index(cat) == wxNOT_FOUND) {
         cats.Add(cat);
      }
   }
#if 0
   mCommandList.GetCount(); i++) {
      if (includeMultis || !mCommandList[i]->multi)
         names.Add(mCommandList[i]->name);
   }

   AudacityProject *p = GetActiveProject();
   if (p == NULL) {
      return;
   }

   wxMenuBar *bar = p->GetMenuBar();
   size_t cnt = bar->GetMenuCount();
   for (size_t i = 0; i < cnt; i++) {
      cats.Add(bar->GetMenuLabelText(i));
   }

   cats.Add(COMMAND);
#endif
}

void CommandManager::GetAllCommandNames(wxArrayString &names,
                                        bool includeMultis)
{
   for(const auto &entry : mCommandList) {
      if (!entry->multi)
         names.Add(entry->name);
      else if( includeMultis )
         names.Add(entry->name + wxT(":")/*+ mCommandList[i]->label*/);
   }
}

void CommandManager::GetAllCommandLabels(wxArrayString &names,
                                        bool includeMultis)
{
   for(const auto &entry : mCommandList) {
      if (!entry->multi)
         names.Add(entry->label);
      else if( includeMultis )
         names.Add(entry->label);
   }
}

void CommandManager::GetAllCommandData(
   wxArrayString &names,
   wxArrayString &keys,
   wxArrayString &default_keys,
   wxArrayString &labels,
   wxArrayString &categories,
#if defined(EXPERIMENTAL_KEY_VIEW)
   wxArrayString &prefixes,
#endif
   bool includeMultis)
{
   for(const auto &entry : mCommandList) {
      if (!entry->multi)
      {
         names.Add(entry->name);
         keys.Add(entry->key);
         default_keys.Add(entry->defaultKey);
         labels.Add(entry->label);
         categories.Add(entry->labelTop);
#if defined(EXPERIMENTAL_KEY_VIEW)
         prefixes.Add(entry->labelPrefix);
#endif
      }
      else if( includeMultis )
      {
         names.Add(entry->name);
         keys.Add(entry->key);
         default_keys.Add(entry->defaultKey);
         labels.Add(entry->label);
         categories.Add(entry->labelTop);
#if defined(EXPERIMENTAL_KEY_VIEW)
         prefixes.Add(entry->labelPrefix);
#endif
      }
   }
}
wxString CommandManager::GetLabelFromName(const wxString &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");

   return entry->label;
}

wxString CommandManager::GetPrefixedLabelFromName(const wxString &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");

#if defined(EXPERIMENTAL_KEY_VIEW)
   wxString prefix;
   if (!entry->labelPrefix.IsEmpty()) {
      prefix = entry->labelPrefix + wxT(" - ");
   }
   return wxMenuItem::GetLabelText(prefix + entry->label);
#else
   return wxString(entry->labelPrefix + wxT(" ") + entry->label).Trim(false).Trim(true);
#endif
}

wxString CommandManager::GetCategoryFromName(const wxString &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");

   return entry->labelTop;
}

wxString CommandManager::GetKeyFromName(const wxString &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");

   return entry->key;
}

wxString CommandManager::GetDefaultKeyFromName(const wxString &name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");

   return entry->defaultKey;
}

bool CommandManager::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("audacitykeyboard"))) {
      mXMLKeysRead = 0;
   }

   if (!wxStrcmp(tag, wxT("command"))) {
      wxString name;
      wxString key;

      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(value))
            name = value;
         if (!wxStrcmp(attr, wxT("key")) && XMLValueChecker::IsGoodString(value))
            key = KeyStringNormalize(value);
      }

      if (mCommandNameHash[name]) {
         if (GetDefaultKeyFromName(name) != key) {
            mCommandNameHash[name]->key = KeyStringNormalize(key);
            mXMLKeysRead++;
         }
      }
   }

   return true;
}

void CommandManager::HandleXMLEndTag(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("audacitykeyboard"))) {
      wxMessageBox(wxString::Format(_("Loaded %d keyboard shortcuts\n"),
                                    mXMLKeysRead),
                   _("Loading Keyboard Shortcuts"),
                   wxOK | wxCENTRE);
   }
}

XMLTagHandler *CommandManager::HandleXMLChild(const wxChar * WXUNUSED(tag))
{
   return this;
}

void CommandManager::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("audacitykeyboard"));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   for(const auto &entry : mCommandList) {
      wxString label = entry->label;
      label = wxMenuItem::GetLabelText(label.BeforeFirst(wxT('\t')));

      xmlFile.StartTag(wxT("command"));
      xmlFile.WriteAttr(wxT("name"), entry->name);
      xmlFile.WriteAttr(wxT("label"), label);
      xmlFile.WriteAttr(wxT("key"), entry->key);
      xmlFile.EndTag(wxT("command"));
   }

   xmlFile.EndTag(wxT("audacitykeyboard"));
}

void CommandManager::SetDefaultFlags(CommandFlag flags, CommandMask mask)
{
   mDefaultFlags = flags;
   mDefaultMask = mask;
}

void CommandManager::SetCommandFlags(const wxString &name,
                                     CommandFlag flags, CommandMask mask)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry) {
      entry->flags = flags;
      entry->mask = mask;
   }
}

void CommandManager::SetCommandFlags(const wxChar **names,
                                     CommandFlag flags, CommandMask mask)
{
   const wxChar **nptr = names;
   while(*nptr) {
      SetCommandFlags(wxString(*nptr), flags, mask);
      nptr++;
   }
}

void CommandManager::SetCommandFlags(CommandFlag flags, CommandMask mask, ...)
{
   va_list list;
   va_start(list, mask);
   for(;;) {
      const wxChar *name = va_arg(list, const wxChar *);
      if (!name)
         break;
      SetCommandFlags(wxString(name), flags, mask);
   }
   va_end(list);
}

#if defined(__WXDEBUG__)
void CommandManager::CheckDups()
{
   int cnt = mCommandList.size();
   for (size_t j = 0;  (int)j < cnt; j++) {
      if (mCommandList[j]->key.IsEmpty()) {
         continue;
      }

      if (mCommandList[j]->label.AfterLast(wxT('\t')) == wxT("allowDup")) {
         continue;
      }

      for (size_t i = 0; (int)i < cnt; i++) {
         if (i == j) {
            continue;
         }

         if (mCommandList[i]->key == mCommandList[j]->key) {
            wxString msg;
            msg.Printf(wxT("key combo '%s' assigned to '%s' and '%s'"),
                       mCommandList[i]->key.c_str(),
                       mCommandList[i]->label.BeforeFirst(wxT('\t')).c_str(),
                       mCommandList[j]->label.BeforeFirst(wxT('\t')).c_str());
            wxASSERT_MSG(mCommandList[i]->key != mCommandList[j]->key, msg);
         }
      }
   }
}

#endif
