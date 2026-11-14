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
#include "CommandContext.h"
#include "commands/CommandManagerWindowClasses.h"
#include "KeyboardCapture.h"
#include "Journal.h"
#include "JournalOutput.h"
#include "JournalRegistry.h"
#include "Registry.h"
#include "ProjectWindows.h"
#include "AudacityMessageBox.h"

#include <wx/evtloop.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>
#include <wx/windowptr.h>

namespace {
struct MenuBarListEntry
{
    MenuBarListEntry(const wxString& name, wxMenuBar* menubar);
    ~MenuBarListEntry();

    wxString name;
    wxWeakRef<wxMenuBar> menubar; // This structure does not assume memory ownership!
};

struct SubMenuListEntry
{
    SubMenuListEntry();
    SubMenuListEntry(SubMenuListEntry&&) = default;
    ~SubMenuListEntry();

    TranslatableString name;
    std::unique_ptr<wxMenu> menu;
};

MenuBarListEntry::MenuBarListEntry(const wxString& name, wxMenuBar* menubar)
    : name{name}, menubar{menubar}
{
}

MenuBarListEntry::~MenuBarListEntry()
{
}

SubMenuListEntry::SubMenuListEntry()
    : menu{std::make_unique<wxMenu>()}
{
}

SubMenuListEntry::~SubMenuListEntry()
{
}
}

MenuCreator::SpecialItem::~SpecialItem() = default;

MenuCreator::MenuCreator(AudacityProject& project)
    : CommandManager{project}
{
}

MenuCreator::~MenuCreator() = default;

MenuCreator& MenuCreator::Get(AudacityProject& project)
{
    return static_cast<MenuCreator&>(CommandManager::Get(project));
}

const MenuCreator& MenuCreator::Get(const AudacityProject& project)
{
    return static_cast<const MenuCreator&>(CommandManager::Get(project));
}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

namespace {
using namespace MenuRegistry;

struct MenuItemVisitor final : CommandManager::Populator {
    explicit MenuItemVisitor(AudacityProject& proj)
        : CommandManager::Populator{proj,
                                    // leaf visit
                                    [this](const auto& item, const auto&) {
            const auto pCurrentMenu = CurrentMenu();
            if (!pCurrentMenu) {
                // There may have been a mistake in the placement hint that
                // registered this single item.  It's not within any menu.
                assert(false);
            } else {
                TypeSwitch::VDispatch<void, LeafTypes>(item,
                                                       [&](const SpecialItem& special) {
                    if (auto pSpecial
                            =dynamic_cast<const MenuCreator::SpecialItem*>(&special)) {
                        pSpecial->fn(mProject, *pCurrentMenu);
                    }
                },
                                                       [this](auto& item){ DoVisit(item); }
                                                       );
            }
        },

                                    [this]{
            if (mbSeparatorAllowed) {
                CurrentMenu()->AppendSeparator();
            }
            Populator::DoSeparator();
        }
                                    }
    {
        auto menubar = AddMenuBar(wxT("appmenu"));
        assert(menubar);

        MenuRegistry::Visit(*this, mProject);

        GetProjectFrame(mProject).SetMenuBar(menubar.release());
    }

    ~MenuItemVisitor() override;

    struct CommandListEntryEx final : CommandManager::CommandListEntry {
        ~CommandListEntryEx() final;
        void UpdateCheckmark(AudacityProject& project) final;
        void Modify(const TranslatableString& newLabel) final;
        bool GetEnabled() const final;
        void Check(bool checked) final;
        void Enable(bool enabled) final;
        void EnableMultiItem(bool enabled) final;

        wxMenu* menu{};
    };

private:
    std::unique_ptr<CommandManager::CommandListEntry>
    AllocateEntry(const MenuRegistry::Options& options) final;
    void VisitEntry(CommandManager::CommandListEntry&, const MenuRegistry::Options* pOptions) final;
    void BeginMenu(const TranslatableString& tName) final;
    void EndMenu() final;
    void BeginMainMenu(const TranslatableString& tName);
    void EndMainMenu();
    void BeginSubMenu(const TranslatableString& tName);
    void EndSubMenu();
    void BeginOccultCommands() final;
    void EndOccultCommands() final;

    std::unique_ptr<wxMenuBar> AddMenuBar(const wxString& sMenu);
    wxMenuBar* CurrentMenuBar() const;
    wxMenuBar* GetMenuBar(const wxString& sMenu) const;
    wxMenu* CurrentSubMenu() const;
    wxMenu* CurrentMenu() const;

    std::vector<MenuBarListEntry> mMenuBarList;
    std::vector<SubMenuListEntry> mSubMenuList;
    std::unique_ptr<wxMenuBar> mTempMenuBar;
    std::unique_ptr<wxMenu> uCurrentMenu;
    wxMenu* mCurrentMenu {};
};

MenuItemVisitor::CommandListEntryEx::~CommandListEntryEx() = default;

void
MenuItemVisitor::CommandListEntryEx::UpdateCheckmark(AudacityProject& project)
{
    if (menu && checkmarkFn && !isOccult) {
        menu->Check(id, checkmarkFn(project));
    }
}

void
MenuItemVisitor::CommandListEntryEx::Modify(const TranslatableString& newLabel)
{
    if (menu) {
        label = newLabel;
        menu->SetLabel(id, FormatLabelForMenu());
    }
}

bool MenuItemVisitor::CommandListEntryEx::GetEnabled() const
{
    if (!menu) {
        return false;
    }
    return enabled;
}

void MenuItemVisitor::CommandListEntryEx::Check(bool checked)
{
    if (!menu || isOccult) {
        return;
    }
    menu->Check(id, checked);
}

void MenuItemVisitor::CommandListEntryEx::Enable(bool b)
{
    if (!menu) {
        enabled = b;
        return;
    }

    // LL:  Refresh from real state as we can get out of sync on the
    //      Mac due to its reluctance to enable menus when in a modal
    //      state.
    enabled = menu->IsEnabled(id);

    // Only enabled if needed
    if (enabled != b) {
        menu->Enable(id, b);
        enabled = menu->IsEnabled(id);
    }
}

void MenuItemVisitor::CommandListEntryEx::EnableMultiItem(bool b)
{
    if (menu) {
        const auto item = menu->FindItem(id);
        if (item) {
            item->Enable(b);
            return;
        }
    }
    // using GET in a log message for devs' eyes only
    wxLogDebug(wxT("Warning: Menu entry with id %i in %s not found"),
               id, name.GET());
}

auto MenuItemVisitor::AllocateEntry(const MenuRegistry::Options& options)
-> std::unique_ptr<CommandManager::CommandListEntry>
{
    auto result = std::make_unique<CommandListEntryEx>();
    if (!options.global) {
        result->menu = CurrentMenu();
    }
    return result;
}

// A label that may have its accelerator disabled.
// The problem is that as soon as we show accelerators in the menu, the menu might
// catch them in normal wxWidgets processing, rather than passing the key presses on
// to the controls that had the focus.  We would like all the menu accelerators to be
// disabled, in fact.
static wxString
FormatLabelWithDisabledAccel(const CommandManager::CommandListEntry& entry)
{
    auto label = entry.label.Translation();
#if 1
    wxString Accel;
    do{
        if (!entry.key.empty()) {
            // Dummy accelerator that looks Ok in menus but is non functional.
            // Note the space before the key.
#ifdef __WXMSW__
            // using GET to compose menu item name for wxWidgets
            auto key = entry.key.GET();
            Accel = wxString("\t ") + key;
            if (key.StartsWith("Left")) {
                break;
            }
            if (key.StartsWith("Right")) {
                break;
            }
            if (key.StartsWith("Up")) {
                break;
            }
            if (key.StartsWith("Down")) {
                break;
            }
            if (key.StartsWith("Return")) {
                break;
            }
            if (key.StartsWith("Tab")) {
                break;
            }
            if (key.StartsWith("Shift+Tab")) {
                break;
            }
            if (key.StartsWith("0")) {
                break;
            }
            if (key.StartsWith("1")) {
                break;
            }
            if (key.StartsWith("2")) {
                break;
            }
            if (key.StartsWith("3")) {
                break;
            }
            if (key.StartsWith("4")) {
                break;
            }
            if (key.StartsWith("5")) {
                break;
            }
            if (key.StartsWith("6")) {
                break;
            }
            if (key.StartsWith("7")) {
                break;
            }
            if (key.StartsWith("8")) {
                break;
            }
            if (key.StartsWith("9")) {
                break;
            }
            // Uncomment the below so as not to add the illegal accelerators.
            // Accel = "";
            //if( entry.key.StartsWith("Space" )) break;
            // These ones appear to be illegal already and mess up accelerator processing.
            if (key.StartsWith("NUMPAD_ENTER")) {
                break;
            }
            if (key.StartsWith("Backspace")) {
                break;
            }
            if (key.StartsWith("Delete")) {
                break;
            }

            // https://github.com/audacity/audacity/issues/4457
            // This code was proposed by David Bailes to fix
            // the decimal separator input in wxTextCtrls that
            // are children of the main window.
            if (key.StartsWith(",")) {
                break;
            }
            if (key.StartsWith(".")) {
                break;
            }

            // https://github.com/audacity/audacity/issues/5868
            // On German and Norwegian keyboards, [ and ] are
            // AltGr 8 and AltGr 9. On Windows typing 8 or 9 match
            // [ or ] repectively when they are accelerators in menus.
            if (key.StartsWith("[")) {
                break;
            }
            if (key.StartsWith("]")) {
                break;
            }

#endif
            //wxLogDebug("Added Accel:[%s][%s]", entry.label, entry.key );
            // Normal accelerator.
            // using GET to compose menu item name for wxWidgets
            Accel = wxString("\t") + entry.key.GET();
        }
    } while (false);
    label += Accel;
#endif
    return label;
}

void MenuItemVisitor::VisitEntry(CommandManager::CommandListEntry& entry,
                                 const MenuRegistry::Options* pOptions)
{
    if (!pOptions) {
        // command list item
        CurrentMenu()->Append(entry.id, entry.FormatLabelForMenu());
    } else if (pOptions->global) {
    } else {
        auto ID = entry.id;
        auto label = FormatLabelWithDisabledAccel(entry);
        auto& checker = pOptions->checker;
        if (checker) {
            CurrentMenu()->AppendCheckItem(ID, label);
            CurrentMenu()->Check(ID, checker(mProject));
        } else {
            CurrentMenu()->Append(ID, label);
        }
    }
}

MenuItemVisitor::~MenuItemVisitor() = default;

void MenuItemVisitor::BeginMenu(const TranslatableString& tName)
{
    if (mCurrentMenu) {
        return BeginSubMenu(tName);
    } else {
        return BeginMainMenu(tName);
    }
}

/// This attaches a menu, if it's main, to the menubar
//  and in all cases ends the menu
void MenuItemVisitor::EndMenu()
{
    if (mSubMenuList.empty()) {
        EndMainMenu();
    } else {
        EndSubMenu();
    }
}

void MenuItemVisitor::BeginMainMenu(const TranslatableString& tName)
{
    uCurrentMenu = std::make_unique<wxMenu>();
    mCurrentMenu = uCurrentMenu.get();
}

/// This attaches a menu to the menubar and ends the menu
void MenuItemVisitor::EndMainMenu()
{
    // Add the menu to the menubar after all menu items have been
    // added to the menu to allow OSX to rearrange special menu
    // items like Preferences, About, and Quit.
    assert(uCurrentMenu);
    CurrentMenuBar()->Append(
        uCurrentMenu.release(), MenuNames()[0].Translation());
    mCurrentMenu = nullptr;
}

/// This starts a new submenu, and names it according to
/// the function's argument.
void MenuItemVisitor::BeginSubMenu(const TranslatableString& tName)
{
    mSubMenuList.emplace_back();
    mbSeparatorAllowed = false;
}

/// This function is called after the final item of a SUBmenu is added.
/// Submenu items are added just like regular menu items; they just happen
/// after BeginSubMenu() is called but before EndSubMenu() is called.
void MenuItemVisitor::EndSubMenu()
{
    //Save the submenu's information
    SubMenuListEntry tmpSubMenu{ std::move(mSubMenuList.back()) };

    //Pop off the NEW submenu so CurrentMenu returns the parent of the submenu
    mSubMenuList.pop_back();

    //Add the submenu to the current menu
    auto name = MenuNames().back().Translation();
    CurrentMenu()->Append(0, name, tmpSubMenu.menu.release(),
                          name /* help string */);
    mbSeparatorAllowed = true;
}

void MenuItemVisitor::BeginOccultCommands()
{
    // To do:  perhaps allow occult item switching at lower levels of the
    // menu tree.
    assert(!CurrentMenu());

    // Make a temporary menu bar collecting items added after.
    // This bar will be discarded but other side effects on the command
    // manager persist.
    mTempMenuBar = AddMenuBar(wxT("ext-menu"));
}

void MenuItemVisitor::EndOccultCommands()
{
    auto iter = mMenuBarList.end();
    if (iter != mMenuBarList.begin()) {
        mMenuBarList.erase(--iter);
    } else {
        assert(false);
    }
    mTempMenuBar.reset();
}
}

void MenuCreator::CreateMenusAndCommands()
{
    {
        MenuItemVisitor visitor{ mProject };
    }

    mLastFlags = AlwaysEnabledFlag;

#if defined(_DEBUG)
//   c->CheckDups();
#endif
}

// Get hackcess to a protected method
class wxFrameEx : public wxFrame
{
public:
    using wxFrame::DetachMenuBar;
};

void MenuCreator::RebuildMenuBar()
{
    auto& project = mProject;
    // On OSX, we can't rebuild the menus while a modal dialog is being shown
    // since the enabled state for menus like Quit and Preference gets out of
    // sync with wxWidgets idea of what it should be.
#if defined(__WXMAC__) && defined(_DEBUG)
    {
        wxDialog* dlg
            =wxDynamicCast(wxGetTopLevelParent(wxWindow::FindFocus()), wxDialog);
        assert((!dlg || !dlg->IsModal()));
    }
#endif

    // Delete the menus, since we will soon recreate them.
    // Rather oddly, the menus don't vanish as a result of doing this.
    {
        auto& window = static_cast<wxFrameEx&>(GetProjectFrame(project));
        wxWindowPtr<wxMenuBar> menuBar{ window.GetMenuBar() };
        window.DetachMenuBar();
        // menuBar gets deleted here
    }

    PurgeData();
    CreateMenusAndCommands();
}

constexpr auto JournalCode = wxT("CM");  // for CommandManager

void MenuCreator::ExecuteCommand(const CommandContext& context,
                                 const wxEvent* evt, const CommandListEntry& entry)
{
    Journal::Output({ JournalCode, entry.name.GET() });
    return CommandManager::ExecuteCommand(context, evt, entry);
}

// a temporary hack that should be removed as soon as we
// get multiple effect preview working
bool MenuCreator::ReallyDoQuickCheck()
{
    return !GetProjectFrame(mProject).IsActive();
}

/// The following method moves to the previous track
/// selecting and unselecting depending if you are on the start of a
/// block or not.

void MenuCreator::RebuildAllMenuBars()
{
    for ( auto p : AllProjects{} ) {
        Get(*p).RebuildMenuBar();
#if defined(__WXGTK__)
        // Workaround for:
        //
        //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
        //
        // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
        auto& window = GetProjectFrame(*p);
        wxRect r = window.GetRect();
        window.SetSize(wxSize(1, 1));
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
                                     []( const wxArrayStringEx& fields )
    {
        // Expect JournalCode and the command name.
        // To do, perhaps, is to include some parameters.
        bool handled = false;
        if (fields.size() == 2) {
            if (auto project = GetActiveProject().lock()) {
                auto pManager = &CommandManager::Get(*project);
                auto flags = CommandManager::Get(*project).GetUpdateFlags();
                const CommandContext context(*project);
                auto& command = fields[1];
                handled
                    =pManager->HandleTextualCommand(command, context, flags, false);
            }
        }
        return handled;
    }
};
}

bool MenuCreator::FilterKeyEvent(
    AudacityProject& project, const wxKeyEvent& evt, bool permit)
{
    auto& cm = Get(project);

    auto pWindow = FindProjectFrame(&project);
    CommandListEntry* entry = cm.mCommandKeyHash[KeyEventToKeyString(evt)];
    if (entry == NULL) {
        return false;
    }

    int type = evt.GetEventType();

    // Global commands aren't tied to any specific project
    if (entry->isGlobal && type == wxEVT_KEY_DOWN) {
        // Global commands are always disabled so they do not interfere with the
        // rest of the command handling.  But, to use the common handler, we
        // enable them temporarily and then disable them again after handling.
        // LL:  Why do they need to be disabled???
        entry->enabled = false;
        auto cleanup = valueRestorer(entry->enabled, true);
        return cm.HandleCommandEntry(entry, NoFlagsSpecified, false, &evt);
    }

    wxWindow* pFocus = wxWindow::FindFocus();
    wxWindow* pParent = wxGetTopLevelParent(pFocus);
    bool validTarget = pParent == pWindow;
    // Bug 1557.  MixerBoard should count as 'destined for project'
    // MixerBoard IS a TopLevelWindow, and its parent is the project.
    if (pParent && pParent->GetParent() == pWindow) {
        if (auto keystrokeHandlingWindow = dynamic_cast< TopLevelKeystrokeHandlingWindow* >(pParent)) {
            validTarget = keystrokeHandlingWindow->HandleCommandKeystrokes();
        }
    }
    validTarget = validTarget && wxEventLoop::GetActive()->IsMain();

    // Any other keypresses must be destined for this project window
    if (!permit && !validTarget) {
        return false;
    }

    auto flags = cm.GetUpdateFlags();

    wxKeyEvent temp = evt;

    // Possibly let wxWidgets do its normal key handling IF it is one of
    // the standard navigation keys.
    if ((type == wxEVT_KEY_DOWN) || (type == wxEVT_KEY_UP)) {
        wxWindow* pWnd = wxWindow::FindFocus();
        bool bIntercept
            =pWnd && !dynamic_cast< NonKeystrokeInterceptingWindow* >(pWnd);

        //wxLogDebug("Focus: %p TrackPanel: %p", pWnd, pTrackPanel );
        // We allow the keystrokes below to be handled by wxWidgets controls IF we are
        // in some sub window rather than in the TrackPanel itself.
        // Otherwise they will go to our command handler and if it handles them
        // they will NOT be available to wxWidgets.
        if (bIntercept) {
            switch (evt.GetKeyCode()) {
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
                if (!evt.HasAnyModifiers()) {
                    return false;
                }
            }
        }
    }

    if (type == wxEVT_KEY_DOWN) {
        if (entry->skipKeydown) {
            return true;
        }
        return cm.HandleCommandEntry(entry, flags, false, &temp);
    }

    if (type == wxEVT_KEY_UP && entry->wantKeyup) {
        return cm.HandleCommandEntry(entry, flags, false, &temp);
    }

    return false;
}

static KeyboardCapture::PreFilter::Scope scope1{
    []( wxKeyEvent& ) {
        // We must have a project since we will be working with the
        // CommandManager, which is tied to individual projects.
        auto project = GetActiveProject().lock();
        return project && GetProjectFrame(*project).IsEnabled();
    } };
static KeyboardCapture::PostFilter::Scope scope2{
    []( wxKeyEvent& key ) {
        // Capture handler window didn't want it, so ask the CommandManager.
        if (auto project = GetActiveProject().lock()) {
            return MenuCreator::FilterKeyEvent(*project, key);
        } else {
            return false;
        }
    } };

NormalizedKeyString KeyEventToKeyString(const wxKeyEvent& event)
{
    wxString newStr;

    long key = event.GetKeyCode();

    if (event.ControlDown()) {
        newStr += wxT("Ctrl+");
    }

    if (event.AltDown()) {
        newStr += wxT("Alt+");
    }

    if (event.ShiftDown()) {
        newStr += wxT("Shift+");
    }

#if defined(__WXMAC__)
    if (event.RawControlDown()) {
        newStr += wxT("RawCtrl+");
    }
#endif

    if (event.RawControlDown() && key >= 1 && key <= 26) {
        newStr += (wxChar)(64 + key);
    } else if (key >= 33 && key <= 255 && key != 127) {
        newStr += (wxChar)key;
    } else {
        switch (key) {
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

///
/// Makes a NEW menubar for placement on the top of a project
/// Names it according to the passed-in string argument.
///
/// If the menubar already exists, that's unexpected.
std::unique_ptr<wxMenuBar> MenuItemVisitor::AddMenuBar(const wxString& sMenu)
{
    wxMenuBar* menuBar = GetMenuBar(sMenu);
    if (menuBar) {
        wxASSERT(false);
        return {};
    }

    auto result = std::make_unique<wxMenuBar>();
    mMenuBarList.emplace_back(sMenu, result.get());

    return result;
}

///
/// Retrieves the menubar based on the name given in AddMenuBar(name)
///
wxMenuBar* MenuItemVisitor::GetMenuBar(const wxString& sMenu) const
{
    for (const auto& entry : mMenuBarList) {
        if (entry.name == sMenu) {
            return entry.menubar;
        }
    }

    return NULL;
}

///
/// Retrieve the 'current' menubar; either NULL or the
/// last on in the mMenuBarList.
wxMenuBar* MenuItemVisitor::CurrentMenuBar() const
{
    if (mMenuBarList.empty()) {
        return NULL;
    }

    return mMenuBarList.back().menubar;
}

/// This returns the 'Current' Submenu, which is the one at the
///  end of the mSubMenuList (or NULL, if it doesn't exist).
wxMenu* MenuItemVisitor::CurrentSubMenu() const
{
    if (mSubMenuList.empty()) {
        return NULL;
    }

    return mSubMenuList.back().menu.get();
}

///
/// This returns the current menu that we're appending to - note that
/// it could be a submenu if BeginSubMenu was called and we haven't
/// reached EndSubMenu yet.
wxMenu* MenuItemVisitor::CurrentMenu() const
{
    if (!mCurrentMenu) {
        return NULL;
    }

    wxMenu* tmpCurrentSubMenu = CurrentSubMenu();

    if (!tmpCurrentSubMenu) {
        return mCurrentMenu;
    }

    return tmpCurrentSubMenu;
}
