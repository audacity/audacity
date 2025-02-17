/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicMenu.cpp

Paul Licameli

**********************************************************************/

#include "BasicMenu.h"
#include "Journal.h"
#include "JournalOutput.h"
#include "JournalRegistry.h"
#include "JournalEvents.h"
#include "MemoryX.h"
#include "wxWidgetsWindowPlacement.h"
#include "wxArrayStringEx.h"
#include <wx/eventfilter.h>
#include <wx/menu.h>
#include <wx/weakref.h>
#include <wx/window.h>
#include <optional>

namespace BasicMenu {
namespace {
const auto JournalCode = L"PopupMenu";

using Menus = std::vector< wxWeakRef< wxMenu > >;

// Each item in this stack lists a menu and all descendant sub-menus
std::vector< Menus > sMenuStack;
bool sHandledEvent = false;

Menus FindDescendants(wxMenu& menu)
{
    Menus result{ &menu };
    // We can discover them breadth-first
    for ( size_t ii = 0; ii < result.size(); ++ii ) {
        if (auto pMenu = result[ii]) {
            for ( const auto& pItem : pMenu->GetMenuItems()) {
                if (const auto pSubMenu = pItem->GetSubMenu()) {
                    result.push_back(pSubMenu);
                }
            }
        }
    }
    return result;
}

inline bool ContainsMenu(const Menus& menus, void* pObj)
{
    return std::count(menus.begin(), menus.end(), pObj);
}

// Find a path name for the id in the given menu, but only if it, and
// each sub-menu above it, is uniquely named among peer items
std::optional< wxArrayStringEx > FindPathName(wxMenu& theMenu, int id)
{
    wxMenuItem* pItem = nullptr;
    wxMenu* pSubMenu = nullptr;
    if (!(pItem = theMenu.FindItem(id, &pSubMenu))) {
        return std::nullopt;
    }

    // Gather path components, checking uniqueness at each level
    wxArrayStringEx names;
    for (; pSubMenu; pSubMenu = pSubMenu->GetParent()) {
        const auto& items = pSubMenu->GetMenuItems();
        const auto begin = items.begin(), end = items.end();
        if (!names.empty()) {
            // Update pItem on second and later passes
            if (const auto iter = std::find_if(begin, end,
                                               [&]( auto pNewItem ){
                return pNewItem->GetSubMenu() == pItem->GetMenu();
            });
                iter == end) {
                return std::nullopt;
            } else {
                pItem = *iter;
            }
        }
        auto name = pItem->GetItemLabelText();
        if (1 != std::count_if(begin, end, [&](auto item){
            return item->GetItemLabelText() == name;
        })) {
            // nonuniqueness
            return std::nullopt;
        }
        names.push_back(name);
    }
    std::reverse(names.begin(), names.end());
    return { names };
}

//! Singleton object listens to global wxEvent stream
struct Watcher : wxEventFilter
{
    Watcher()
    {
        wxEvtHandler::AddFilter(this);
    }

    ~Watcher()
    {
        wxEvtHandler::RemoveFilter(this);
    }

    int FilterEvent(wxEvent& event) override
    {
        using namespace Journal::Events;

        // Record something only if we are recording events, this is a menu
        // event, there is an outstanding popup menu, and that or a descendant
        // is the event object
        auto pObj = event.GetEventObject();
        if (!(IsWatching()
              && event.GetEventType() == wxEVT_MENU
              && !sMenuStack.empty()
              && ContainsMenu(sMenuStack.back(), pObj))) {
            return Event_Skip;
        }

        // Find a path identifying the object
        auto pPath = FindPathName(*static_cast<wxMenu*>(pObj), event.GetId());
        if (!pPath) {
            FailedEventSerialization();
            return Event_Skip;
        }

        // Write a representation to the journal.
        // Write names, not numerical ids, so the journal is not
        // fragile if the assignment of ids to commands changes.
        pPath->insert(pPath->begin(), JournalCode);
        Journal::Output(*pPath);
        sHandledEvent = true;

        return Event_Skip;
    }
};

void Watch()
{
    static Watcher instance;
}

// Add a callback for startup of journalling
Journal::RegisteredInitializer initializer{ []{
        using namespace Journal;

        if (!GetError() && IsRecording()) {
            // one time installation
            Watch();
        }

        return true;
    } };

void ReplayPopup(wxMenu* theMenu)
{
    // Expect JournalCode and maybe a path.
    const auto fields = Journal::GetTokens();
    if (fields[0] == JournalCode) {
        if (fields.size() == 1) {
            // No command, so just eat the journal line
            return;
        }

        // Locate the menu item by name in the current popup menu or descendant.
        auto found = [&]() -> std::pair<wxMenuItem*, wxMenu*> {
            wxMenuItem* pItem = nullptr;
            auto pMenu = theMenu;
            for ( auto pField = fields.begin() + 1, endFields = fields.end();
                  pMenu && pField != endFields; ++pField ) {
                auto& name = *pField;
                const auto& list = pMenu->GetMenuItems();
                const auto pred = [&name](auto& pItem){
                    return pItem->GetItemLabelText() == name;
                };
                const auto begin = list.begin(), end = list.end(),
                           iter = std::find_if(begin, end, pred);

                // Check existence and uniqueness
                if (auto next = iter;
                    end == next || end != std::find_if(++next, end, pred)) {
                    return { nullptr, nullptr }
                }

                pItem = *iter;
                if (pField + 1 != endFields) {
                    pMenu = pItem->GetSubMenu();
                }
            }
            return { pItem, pMenu };
        }();

        if (auto [pItem, pMenu] = found; pItem && pMenu) {
            // Don't really pop up the menu, which uses native event handling
            // that we can't filter.  Simulate an event instead.
            // Require that some event is bound to the item, so it is
            // handled, or else the journal fails replay.
            wxCommandEvent event{ wxEVT_MENU, pItem->GetId() };
            event.SetEventObject(pMenu);
            if (pMenu->ProcessEvent(event)) {
                sHandledEvent = true;
                return;
            }
        }
    }

    // Replay did not find all as expected
    throw Journal::SyncException(wxString::Format(
                                     "PopupMenu has failed to invoke %s",
                                     wxJoin(fields, ',').ToStdString().c_str()));
}
}

void Handle::Popup(const BasicUI::WindowPlacement& window, const Point& pos)
{
    wxMenu* const pMenu = mpMenu;
    if (!pMenu) {
        return;
    }

    if (auto pWindow = wxWidgetsWindowPlacement::GetParent(window)) {
        sHandledEvent = false;

        // Put the menu pointers where the event filter can find them
        sMenuStack.push_back(FindDescendants(*pMenu));
        auto cleanup = finally([]{ sMenuStack.pop_back(); });

        if (Journal::IsReplaying()) {
            ReplayPopup(pMenu);
        } else {
            pWindow->PopupMenu(pMenu, { pos.x, pos.y });
        }

        if (!sHandledEvent) {
            // Menu popped but no command was selected.  Record that.
            Journal::Output(JournalCode);
        }
    }
}
}
