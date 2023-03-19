/**********************************************************************

  Audacity: A Digital Audio Editor

  MenuCreator.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et al.

  Paul Licameli split from Menus.cpp

*******************************************************************//**

\file MenuCreator.cpp
\brief Functions for building toobar menus and enabling and disabling items

*//****************************************************************//**

\class MenuCreator
\brief MenuCreator is responsible for creating the main menu bar.

*//*******************************************************************/
#include "MenuCreator.h"

#include "ActiveProject.h"
#include "commands/CommandContext.h"
#include "commands/CommandManagerWindowClasses.h"
#include "KeyboardCapture.h"
#include "Journal.h"
#include "JournalOutput.h"
#include "JournalRegistry.h"
#include "Registry.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "UndoManager.h"
#include "AudacityMessageBox.h"

#include <wx/evtloop.h>
#include <wx/frame.h>
#include <wx/menu.h>
#include <wx/windowptr.h>

MenuCreator::SpecialItem::~SpecialItem() = default;

MenuCreator::MenuCreator(AudacityProject &project)
   : CommandManager{ project }
{
   mUndoSubscription = UndoManager::Get(project)
      .Subscribe(*this, &MenuCreator::OnUndoRedo);
}

MenuCreator::~MenuCreator() = default;

MenuCreator &MenuCreator::Get(AudacityProject &project)
{
   return static_cast<MenuCreator&>(CommandManager::Get(project));
}

const MenuCreator &MenuCreator::Get(const AudacityProject &project)
{
   return static_cast<const MenuCreator&>(CommandManager::Get(project));
}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

namespace {

using namespace MenuRegistry;

struct MenuItemVisitor : Visitor<Traits> {
   MenuItemVisitor(AudacityProject &proj, CommandManager &man)
   : Visitor<Traits> { std::tuple{
      // pre-visit
      std::tuple {
         [this](const MenuItem &menu, auto&) {
            manager.BeginMenu(menu.GetTitle());
         },
         [this](const ConditionalGroupItem &conditionalGroup, auto&) {
            const auto flag = conditionalGroup();
            if (!flag)
               manager.BeginOccultCommands();
            // to avoid repeated call of condition predicate in EndGroup():
            flags.push_back(flag);
         },
         [this](auto &item, auto&) {
            assert(IsSection(item));
         }
      },

      // leaf visit
      [this](const auto &item, const auto&) {
         const auto pCurrentMenu = manager.CurrentMenu();
         if (!pCurrentMenu) {
            // There may have been a mistake in the placement hint that registered
            // this single item.  It's not within any menu.
            assert(false);
         }
         else TypeSwitch::VDispatch<void, LeafTypes>(item,
            [&](const CommandItem &command) {
               manager.AddItem(
                  command.name, command.label_in,
                  command.finder, command.callback,
                  command.flags, command.options);
            },
            [&](const CommandGroupItem &commandList) {
               manager.AddItemList(commandList.name,
                  commandList.items.data(), commandList.items.size(),
                  commandList.finder, commandList.callback,
                  commandList.flags, commandList.isEffect);
            },
            [&](const SpecialItem &special) {
               if (auto pSpecial =
                  dynamic_cast<const MenuCreator::SpecialItem*>(&special))
                  pSpecial->fn(mProject, *pCurrentMenu);
            }
         );
      },

      // post-visit
      std::tuple {
         [this](const MenuItem &, const auto&) {
            manager.EndMenu();
         },
         [this](const ConditionalGroupItem &, const auto&) {
            const bool flag = flags.back();
            if (!flag)
               manager.EndOccultCommands();
            flags.pop_back();
         },
         [this](auto &item, auto&) {
            assert(IsSection(item));
         }
      }},

      [this]() {
         manager.AddSeparator();
      }
   }
   , mProject{ proj }
   , manager{ man }
   {}

   AudacityProject &mProject;
   CommandManager &manager;
   std::vector<bool> flags;
};
}

void MenuCreator::CreateMenusAndCommands()
{
   auto &project = mProject;

   // The list of defaults to exclude depends on
   // preference wxT("/GUI/Shortcuts/FullDefaults"), which may have changed.
   SetMaxList();

   auto menubar = AddMenuBar(wxT("appmenu"));
   wxASSERT(menubar);

   MenuItemVisitor visitor{ project, *this };
   MenuRegistry::Visit(visitor, project);

   GetProjectFrame( project ).SetMenuBar(menubar.release());

   mLastFlags = AlwaysEnabledFlag;

#if defined(_DEBUG)
//   c->CheckDups();
#endif
}

// TODO: This surely belongs in CommandManager?
void MenuCreator::ModifyUndoMenuItems()
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

// Get hackcess to a protected method
class wxFrameEx : public wxFrame
{
public:
   using wxFrame::DetachMenuBar;
};

void MenuCreator::RebuildMenuBar()
{
   auto &project = mProject;
   // On OSX, we can't rebuild the menus while a modal dialog is being shown
   // since the enabled state for menus like Quit and Preference gets out of
   // sync with wxWidgets idea of what it should be.
#if defined(__WXMAC__) && defined(_DEBUG)
   {
      wxDialog *dlg =
         wxDynamicCast(wxGetTopLevelParent(wxWindow::FindFocus()), wxDialog);
      wxASSERT((!dlg || !dlg->IsModal()));
   }
#endif

   // Delete the menus, since we will soon recreate them.
   // Rather oddly, the menus don't vanish as a result of doing this.
   {
      auto &window = static_cast<wxFrameEx&>( GetProjectFrame( project ) );
      wxWindowPtr<wxMenuBar> menuBar{ window.GetMenuBar() };
      window.DetachMenuBar();
      // menuBar gets deleted here
   }

   PurgeData();
   CreateMenusAndCommands();
}

constexpr auto JournalCode = wxT("CM");  // for CommandManager

void MenuCreator::ExecuteCommand(const CommandContext &context,
   const wxEvent *evt, const CommandListEntry &entry)
{
   Journal::Output({ JournalCode, entry.name.GET() });
   return CommandManager::ExecuteCommand(context, evt, entry);
}

void MenuCreator::OnUndoRedo(UndoRedoMessage message)
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
void MenuCreator::UpdateMenus( bool checkActive )
{
   auto &project = mProject;

   auto flags = GetUpdateFlags(checkActive);
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

/// The following method moves to the previous track
/// selecting and unselecting depending if you are on the start of a
/// block or not.

void MenuCreator::RebuildAllMenuBars()
{
   for( auto p : AllProjects{} ) {
      Get(*p).RebuildMenuBar();
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
      auto &window = GetProjectFrame( *p );
      wxRect r = window.GetRect();
      window.SetSize(wxSize(1,1));
      window.SetSize(r.GetSize());
#endif
   }
}

void MenuCreator::RemoveDuplicateShortcuts()
{
   const auto disabledShortcuts = ReportDuplicateShortcuts();
   if (!disabledShortcuts.Translation().empty()) {
      TranslatableString message = XO("The following commands have had their shortcuts removed,"
      " because their default shortcut is new or changed, and is the same shortcut"
      " that you have assigned to another command.")
         + disabledShortcuts;
      AudacityMessageBox(message, XO("Shortcuts have been removed"), wxOK | wxCENTRE);

      gPrefs->Flush();
      RebuildAllMenuBars();
   }
}

static CommandManager::Factory::SubstituteInShared<MenuCreator> scope;

namespace {

// Register a callback for the journal
Journal::RegisteredCommand sCommand{ JournalCode,
[]( const wxArrayStringEx &fields )
{
   // Expect JournalCode and the command name.
   // To do, perhaps, is to include some parameters.
   bool handled = false;
   if ( fields.size() == 2 ) {
      if (auto project = GetActiveProject().lock()) {
         auto pManager = &CommandManager::Get( *project );
         auto flags = CommandManager::Get( *project ).GetUpdateFlags();
         const CommandContext context( *project );
         auto &command = fields[1];
         handled =
            pManager->HandleTextualCommand( command, context, flags, false );
      }
   }
   return handled;
}
};

}

bool MenuCreator::FilterKeyEvent(
   AudacityProject &project, const wxKeyEvent & evt, bool permit)
{
   auto &cm = Get(project);
   
   auto pWindow = FindProjectFrame(&project);
   CommandListEntry *entry = cm.mCommandKeyHash[KeyEventToKeyString(evt)];
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
      entry->enabled = false;
      auto cleanup = valueRestorer( entry->enabled, true );
      return cm.HandleCommandEntry(entry, NoFlagsSpecified, false, &evt);
   }

   wxWindow * pFocus = wxWindow::FindFocus();
   wxWindow * pParent = wxGetTopLevelParent( pFocus );
   bool validTarget = pParent == pWindow;
   // Bug 1557.  MixerBoard should count as 'destined for project'
   // MixerBoard IS a TopLevelWindow, and its parent is the project.
   if( pParent && pParent->GetParent() == pWindow ){
      if(auto keystrokeHandlingWindow = dynamic_cast< TopLevelKeystrokeHandlingWindow* >( pParent ))
         validTarget = keystrokeHandlingWindow->HandleCommandKeystrokes();
   }
   validTarget = validTarget && wxEventLoop::GetActive()->IsMain();

   // Any other keypresses must be destined for this project window
   if (!permit && !validTarget )
   {
      return false;
   }

   auto flags = cm.GetUpdateFlags();

   wxKeyEvent temp = evt;

   // Possibly let wxWidgets do its normal key handling IF it is one of
   // the standard navigation keys.
   if((type == wxEVT_KEY_DOWN) || (type == wxEVT_KEY_UP ))
   {
      wxWindow * pWnd = wxWindow::FindFocus();
      bool bIntercept =
         pWnd && !dynamic_cast< NonKeystrokeInterceptingWindow * >( pWnd );

      //wxLogDebug("Focus: %p TrackPanel: %p", pWnd, pTrackPanel );
      // We allow the keystrokes below to be handled by wxWidgets controls IF we are
      // in some sub window rather than in the TrackPanel itself.
      // Otherwise they will go to our command handler and if it handles them
      // they will NOT be available to wxWidgets.
      if( bIntercept ){
         switch( evt.GetKeyCode() ){
         case WXK_LEFT:
         case WXK_RIGHT:
         case WXK_UP:
         case WXK_DOWN:
         // Don't trap WXK_SPACE (Bug 1727 - SPACE not starting/stopping playback
         // when cursor is in a time control)
         // case WXK_SPACE:
         case WXK_TAB:
         case WXK_BACK:
         case WXK_HOME:
         case WXK_END:
         case WXK_RETURN:
         case WXK_NUMPAD_ENTER:
         case WXK_DELETE:
         case '0':
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
         case '8':
         case '9':
            return false;
         case ',':
         case '.':
            if (!evt.HasAnyModifiers())
               return false;
         }
      }
   }

   if (type == wxEVT_KEY_DOWN)
   {
      if (entry->skipKeydown)
      {
         return true;
      }
      return cm.HandleCommandEntry(entry, flags, false, &temp);
   }

   if (type == wxEVT_KEY_UP && entry->wantKeyup)
   {
      return cm.HandleCommandEntry(entry, flags, false, &temp);
   }

   return false;
}

static KeyboardCapture::PreFilter::Scope scope1{
[]( wxKeyEvent & ) {
   // We must have a project since we will be working with the
   // CommandManager, which is tied to individual projects.
   auto project = GetActiveProject().lock();
   return project && GetProjectFrame( *project ).IsEnabled();
} };
static KeyboardCapture::PostFilter::Scope scope2{
[]( wxKeyEvent &key ) {
   // Capture handler window didn't want it, so ask the CommandManager.
   if (auto project = GetActiveProject().lock()) {
      return MenuCreator::FilterKeyEvent(*project, key);
   }
   else
      return false;
} };


NormalizedKeyString KeyEventToKeyString(const wxKeyEvent & event)
{
   wxString newStr;

   long key = event.GetKeyCode();

   if (event.ControlDown())
      newStr += wxT("Ctrl+");

   if (event.AltDown())
      newStr += wxT("Alt+");

   if (event.ShiftDown())
      newStr += wxT("Shift+");

#if defined(__WXMAC__)
   if (event.RawControlDown())
      newStr += wxT("RawCtrl+");
#endif

   if (event.RawControlDown() && key >= 1 && key <= 26)
      newStr += (wxChar)(64 + key);
   else if (key >= 33 && key <= 255 && key != 127)
      newStr += (wxChar)key;
   else
   {
      switch(key)
      {
      case WXK_BACK:
         newStr += wxT("Backspace");
         break;
      case WXK_DELETE:
         newStr += wxT("Delete");
         break;
      case WXK_SPACE:
         newStr += wxT("Space");
         break;
      case WXK_TAB:
         newStr += wxT("Tab");
         break;
      case WXK_RETURN:
         newStr += wxT("Return");
         break;
      case WXK_PAGEUP:
         newStr += wxT("PageUp");
         break;
      case WXK_PAGEDOWN:
         newStr += wxT("PageDown");
         break;
      case WXK_END:
         newStr += wxT("End");
         break;
      case WXK_HOME:
         newStr += wxT("Home");
         break;
      case WXK_LEFT:
         newStr += wxT("Left");
         break;
      case WXK_UP:
         newStr += wxT("Up");
         break;
      case WXK_RIGHT:
         newStr += wxT("Right");
         break;
      case WXK_DOWN:
         newStr += wxT("Down");
         break;
      case WXK_ESCAPE:
         newStr += wxT("Escape");
         break;
      case WXK_INSERT:
         newStr += wxT("Insert");
         break;
      case WXK_NUMPAD0:
         newStr += wxT("NUMPAD0");
         break;
      case WXK_NUMPAD1:
         newStr += wxT("NUMPAD1");
         break;
      case WXK_NUMPAD2:
         newStr += wxT("NUMPAD2");
         break;
      case WXK_NUMPAD3:
         newStr += wxT("NUMPAD3");
         break;
      case WXK_NUMPAD4:
         newStr += wxT("NUMPAD4");
         break;
      case WXK_NUMPAD5:
         newStr += wxT("NUMPAD5");
         break;
      case WXK_NUMPAD6:
         newStr += wxT("NUMPAD6");
         break;
      case WXK_NUMPAD7:
         newStr += wxT("NUMPAD7");
         break;
      case WXK_NUMPAD8:
         newStr += wxT("NUMPAD8");
         break;
      case WXK_NUMPAD9:
         newStr += wxT("NUMPAD9");
         break;
      case WXK_MULTIPLY:
         newStr += wxT("*");
         break;
      case WXK_ADD:
         newStr += wxT("+");
         break;
      case WXK_SUBTRACT:
         newStr += wxT("-");
         break;
      case WXK_DECIMAL:
         newStr += wxT(".");
         break;
      case WXK_DIVIDE:
         newStr += wxT("/");
         break;
      case WXK_F1:
         newStr += wxT("F1");
         break;
      case WXK_F2:
         newStr += wxT("F2");
         break;
      case WXK_F3:
         newStr += wxT("F3");
         break;
      case WXK_F4:
         newStr += wxT("F4");
         break;
      case WXK_F5:
         newStr += wxT("F5");
         break;
      case WXK_F6:
         newStr += wxT("F6");
         break;
      case WXK_F7:
         newStr += wxT("F7");
         break;
      case WXK_F8:
         newStr += wxT("F8");
         break;
      case WXK_F9:
         newStr += wxT("F9");
         break;
      case WXK_F10:
         newStr += wxT("F10");
         break;
      case WXK_F11:
         newStr += wxT("F11");
         break;
      case WXK_F12:
         newStr += wxT("F12");
         break;
      case WXK_F13:
         newStr += wxT("F13");
         break;
      case WXK_F14:
         newStr += wxT("F14");
         break;
      case WXK_F15:
         newStr += wxT("F15");
         break;
      case WXK_F16:
         newStr += wxT("F16");
         break;
      case WXK_F17:
         newStr += wxT("F17");
         break;
      case WXK_F18:
         newStr += wxT("F18");
         break;
      case WXK_F19:
         newStr += wxT("F19");
         break;
      case WXK_F20:
         newStr += wxT("F20");
         break;
      case WXK_F21:
         newStr += wxT("F21");
         break;
      case WXK_F22:
         newStr += wxT("F22");
         break;
      case WXK_F23:
         newStr += wxT("F23");
         break;
      case WXK_F24:
         newStr += wxT("F24");
         break;
      case WXK_NUMPAD_ENTER:
         newStr += wxT("NUMPAD_ENTER");
         break;
      case WXK_NUMPAD_F1:
         newStr += wxT("NUMPAD_F1");
         break;
      case WXK_NUMPAD_F2:
         newStr += wxT("NUMPAD_F2");
         break;
      case WXK_NUMPAD_F3:
         newStr += wxT("NUMPAD_F3");
         break;
      case WXK_NUMPAD_F4:
         newStr += wxT("NUMPAD_F4");
         break;
      case WXK_NUMPAD_HOME:
         newStr += wxT("NUMPAD_HOME");
         break;
      case WXK_NUMPAD_LEFT:
         newStr += wxT("NUMPAD_LEFT");
         break;
      case WXK_NUMPAD_UP:
         newStr += wxT("NUMPAD_UP");
         break;
      case WXK_NUMPAD_RIGHT:
         newStr += wxT("NUMPAD_RIGHT");
         break;
      case WXK_NUMPAD_DOWN:
         newStr += wxT("NUMPAD_DOWN");
         break;
      case WXK_NUMPAD_PAGEUP:
         newStr += wxT("NUMPAD_PAGEUP");
         break;
      case WXK_NUMPAD_PAGEDOWN:
         newStr += wxT("NUMPAD_PAGEDOWN");
         break;
      case WXK_NUMPAD_END:
         newStr += wxT("NUMPAD_END");
         break;
      case WXK_NUMPAD_BEGIN:
         newStr += wxT("NUMPAD_HOME");
         break;
      case WXK_NUMPAD_INSERT:
         newStr += wxT("NUMPAD_INSERT");
         break;
      case WXK_NUMPAD_DELETE:
         newStr += wxT("NUMPAD_DELETE");
         break;
      case WXK_NUMPAD_EQUAL:
         newStr += wxT("NUMPAD_EQUAL");
         break;
      case WXK_NUMPAD_MULTIPLY:
         newStr += wxT("NUMPAD_MULTIPLY");
         break;
      case WXK_NUMPAD_ADD:
         newStr += wxT("NUMPAD_ADD");
         break;
      case WXK_NUMPAD_SUBTRACT:
         newStr += wxT("NUMPAD_SUBTRACT");
         break;
      case WXK_NUMPAD_DECIMAL:
         newStr += wxT("NUMPAD_DECIMAL");
         break;
      case WXK_NUMPAD_DIVIDE:
         newStr += wxT("NUMPAD_DIVIDE");
         break;
      default:
         return {}; // Don't do anything if we don't recognize the key
      }
   }

   return NormalizedKeyString{ newStr };
}
