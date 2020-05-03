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

#include <type_traits>

#include <wx/frame.h>
#include <wx/windowptr.h>

#include "wxWidgetsWindowPlacement.h"

namespace BasicMenu {

namespace Item {

static_assert( std::is_same_v<ID, wxWindowID>,
   "Does Menu::Item::ID lose information?" );

TranslatableString Label::Full() const
{
   return accel.empty()
      ? main
      : TranslatableString{ main }
         .Join( Verbatim( accel.GET() ), L"\t" );
}

}

using namespace Item;

class Handle::Menu : public wxMenu
{
public:
   using wxMenu::wxMenu;
   ~Menu() override = default;

   // Remember the unstranslated strings
   TranslatableStrings mLabels;
};

struct Handle::Impl {
   Impl() = default;

   explicit Impl( std::unique_ptr<Menu> uMenu )
      : muMenu{ move( uMenu ) }
      , mwMenu{ muMenu.get() }
   {}

   explicit Impl( wxWeakRef<Menu> wMenu )
      : muMenu{}, mwMenu{ wMenu }
   {}

   std::unique_ptr< Menu > muMenu;
   wxWeakRef< Menu > mwMenu;
};

Handle::Handle()
   : mpImpl{ std::make_unique<Impl>() }
{}

Handle::Handle( FreshMenu_t )
   : mpImpl{ std::make_unique<Impl>( std::make_unique< Menu >() ) }
{}

Handle::Handle( const Handle &other )
   : mpImpl{ std::make_unique<Impl>( other.mpImpl->mwMenu ) }
{}

Handle::Handle( Menu *pMenu )
   : mpImpl{ std::make_unique<Impl>( pMenu ) }
{}

Handle::Handle( Handle &&other )
   : mpImpl{
      other.mpImpl->muMenu
      ? std::make_unique<Impl>( move( other.mpImpl->muMenu ) )
      : std::make_unique<Impl>( other.mpImpl->mwMenu )
   }
{}

Handle &Handle::operator =( Handle &&other )
{
   // Note other.mpImpl remains non-null
   *this->mpImpl = std::move( *other.mpImpl );
   return *this;
}

Handle::~Handle() = default;

wxMenu *Handle::GetWxMenu() const
{
   return mpImpl->mwMenu;
}

Handle::operator bool() const
{
   return mpImpl->mwMenu;
}

namespace {
   void ApplyState(
      wxMenuItem &item, const State &state, unsigned mask = ~0u )
   {
      if ( (mask & State::Enable) )
         item.Enable( state.enabled );
      if ( (mask & State::Check) &&
            item.IsCheckable() && !item.IsSeparator() )
         item.Check( state.checked );
   }

   inline wxItemKind toItemKind( Type type )
   {
      switch ( type ) {
      case Type::Separator:
         return wxITEM_SEPARATOR;
      case Type::Normal:
         return wxITEM_NORMAL;
      case Type::Check:
         return wxITEM_CHECK;
      case Type::Radio:
         return wxITEM_RADIO;
      case Type::SubMenu:
         return wxITEM_DROPDOWN;
      default:
         return static_cast< wxItemKind >( type );
      }
   }

   inline Type toItemType( wxItemKind kind )
   {
      switch ( kind ) {
      case wxITEM_SEPARATOR:
         return Type::Separator;
      case wxITEM_NORMAL:
         return Type::Normal;
      case wxITEM_CHECK:
         return Type::Check;
      case wxITEM_RADIO:
         return Type::Radio;
      case wxITEM_DROPDOWN:
         return Type::SubMenu;
      default:
         return static_cast< Type >( kind );
      }
   }
}

void Handle::Append( Type type,
   const Text &text,
   Action action,
   const State &state,
   ID itemid )
{
   if (!mpImpl->mwMenu)
      return;

   auto kind = toItemKind( type );
   if ( type == Type::Separator )
      itemid = wxID_SEPARATOR, kind = wxITEM_NORMAL;
   auto result = mpImpl->mwMenu->Append( itemid,
      text.label.Full().Translation(), text.GetHelp().Translation(), kind );

   // Always bind some action to the button, for journalling reasons:
   // playback needs to detect that the menu event is really handled
   mpImpl->mwMenu->Bind( wxEVT_MENU,
      [action](wxCommandEvent&){ if (action) action(); },
      result->GetId() );

   ApplyState( *result, state );
   mpImpl->mwMenu->mLabels.push_back( text.label.main );
}

void Handle::AppendSubMenu(Handle &&submenu,
   const Text &text,
   const State &state )
{
   if (!(this->mpImpl->mwMenu || submenu.mpImpl->muMenu))
      return;
   // Beware the release!  Exception-safety...
   auto rawText = text.label.Full().Translation();
   auto rawHelp = text.GetHelp().Translation();
   auto result = mpImpl->mwMenu->Append( 0, rawText,
      submenu.mpImpl->muMenu.release(), rawHelp );
   ApplyState( *result, state );
   mpImpl->mwMenu->mLabels.push_back( text.label.main );
}

void Handle::Clear()
{
   wxMenuItemList items = mpImpl->mwMenu->GetMenuItems();
   for (auto iter = items.begin(); iter != items.end();)
      mpImpl->mwMenu->Destroy(*iter++);
}

namespace {

const auto JournalCode = L"PopupMenu";

using Menus = std::vector< wxWeakRef< wxMenu > >;

// Each item in this stack lists a menu and all descendant sub-menus
std::vector< Menus > sMenuStack;
bool sHandledEvent = false;

Menus FindDescendants( wxMenu &menu )
{
   Menus result{ &menu };
   // We can discover them breadth-first
   for ( size_t ii = 0; ii < result.size(); ++ii ) {
      if ( auto pMenu = result[ii] ) {
         for ( const auto &pItem : pMenu->GetMenuItems() ) {
            if ( const auto pSubMenu = pItem->GetSubMenu() )
               result.push_back( pSubMenu );
         }
      }
   }
   return result;
}

inline bool ContainsMenu( const Menus &menus, void *pObj )
{
   return std::count( menus.begin(), menus.end(), pObj );
}

// Find a path name for the id in the given menu, but only if it, and
// each sub-menu above it, is uniquely named among peer items
std::optional< wxArrayStringEx > FindPathName( wxMenu &theMenu, int id )
{
   wxMenuItem *pItem = nullptr;
   wxMenu *pSubMenu = nullptr;
   if ( !( pItem = theMenu.FindItem( id, &pSubMenu ) ) )
      return std::nullopt;

   // Gather path components, checking uniqueness at each level
   wxArrayStringEx names;
   for ( ; pSubMenu; pSubMenu = pSubMenu->GetParent() ) {
      const auto &items = pSubMenu->GetMenuItems();
      const auto begin = items.begin(), end = items.end();
      if ( !names.empty() ) {
         // Update pItem on second and later passes
         if ( const auto iter = std::find_if( begin, end,
               [&]( auto pNewItem ){
                  return pNewItem->GetSubMenu() == pItem->GetMenu(); } );
             iter == end )
            return std::nullopt;
         else
            pItem = *iter;
      }
      auto name = pItem->GetItemLabelText();
      if ( 1 != std::count_if( begin, end, [&](auto item){
            return item->GetItemLabelText() == name; } ) )
         // nonuniqueness
         return std::nullopt;
      names.push_back( name );
   }
   std::reverse( names.begin(), names.end() );
   return { names };
}

//! Singleton object listens to global wxEvent stream
struct Watcher : wxEventFilter
{
   Watcher()
   {
      wxEvtHandler::AddFilter( this );
   }

   ~Watcher()
   {
      wxEvtHandler::RemoveFilter( this );
   }

   int FilterEvent( wxEvent &event ) override
   {
      using namespace Journal::Events;

      // Record something only if we are recording events, this is a menu
      // event, there is an outstanding popup menu, and that or a descendant
      // is the event object
      auto pObj = event.GetEventObject();
      if (!(IsWatching() &&
            event.GetEventType() == wxEVT_MENU &&
            !sMenuStack.empty() &&
            ContainsMenu( sMenuStack.back(), pObj ) ))
         return Event_Skip;

      // Find a path identifying the object
      auto pPath = FindPathName( *static_cast<wxMenu*>(pObj), event.GetId() );
      if ( !pPath ) {
         FailedEventSerialization();
         return Event_Skip;
      }

      // Write a representation to the journal.
      // Write names, not numerical ids, so the journal is not
      // fragile if the assignment of ids to commands changes.
      pPath->insert( pPath->begin(), JournalCode );
      Journal::Output( *pPath );
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

   if ( !GetError() && IsRecording() )
      // one time installation
      Watch();

   return true;
} };

void ReplayPopup( wxMenu *theMenu )
{
   // Expect JournalCode and maybe a path.
   const auto fields = Journal::GetTokens();
   if ( fields[0] == JournalCode ) {
      if ( fields.size() == 1)
         // No command, so just eat the journal line
         return;

      // Locate the menu item by name in the current popup menu or descendant.
      auto found = [&]() -> std::pair<wxMenuItem *, wxMenu*> {
         wxMenuItem *pItem = nullptr;
         auto pMenu = theMenu;
         for ( auto pField = fields.begin() + 1, endFields = fields.end();
              pMenu && pField != endFields; ++pField ) {
            auto &name = *pField;
            const auto &list = pMenu->GetMenuItems();
            const auto pred = [&name](auto &pItem){
               return pItem->GetItemLabelText() == name; };
            const auto begin = list.begin(), end = list.end(),
               iter = std::find_if(begin, end, pred);

            // Check existence and uniqueness
            if ( auto next = iter;
                end == next || end != std::find_if(++next, end, pred) )
               return { nullptr, nullptr };

            pItem = *iter;
            if ( pField + 1 != endFields )
               pMenu = pItem->GetSubMenu();
         }
         return { pItem, pMenu };
      }();

      if ( auto [pItem, pMenu] = found; pItem && pMenu ) {
         // Don't really pop up the menu, which uses native event handling
         // that we can't filter.  Simulate an event instead.
         // Require that some event is bound to the item, so it is
         // handled, or else the journal fails replay.
         wxCommandEvent event{ wxEVT_MENU, pItem->GetId() };
         event.SetEventObject( pMenu );
         if ( pMenu->ProcessEvent( event ) ) {
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

void Handle::Popup( const BasicUI::WindowPlacement &window, const Point &pos )
{
   wxMenu *const pMenu = mpImpl->mwMenu;
   if ( !pMenu )
      return;

   if ( auto pWindow = wxWidgetsWindowPlacement::GetParent( window ) ) {
      sHandledEvent = false;

      // Put the menu pointers where the event filter can find them
      sMenuStack.push_back( FindDescendants( *pMenu ) );
      auto cleanup = finally( []{ sMenuStack.pop_back(); } );

      if ( Journal::IsReplaying() )
         ReplayPopup( pMenu );
      else
         pWindow->PopupMenu( pMenu, { pos.x, pos.y } );

      if ( !sHandledEvent )
         // Menu popped but no command was selected.  Record that.
         Journal::Output( JournalCode );
   }
}

auto Handle::begin() const -> Iterator
{
   return { *this, true };
}

auto Handle::end() const -> Iterator
{
   return { *this, false };
}

State Handle::GetState( ID itemid )
{
   if (!mpImpl->mwMenu)
      return { false, false };
   return {
      mpImpl->mwMenu->IsEnabled( itemid ),
      mpImpl->mwMenu->IsChecked( itemid )
   };
}

bool Handle::SetState(
   ID itemid, const State &state, unsigned mask )
{
   if (!mpImpl->mwMenu)
      return false;
   const auto pItem = mpImpl->mwMenu->FindItem( itemid );
   if ( pItem ) {
      ApplyState( *pItem, state, mask );
      return true;
   }
   else
      return false;
}

bool Handle::SetLabel( ID itemid, const Label& label)
{
   if (!mpImpl->mwMenu)
      return false;
   const auto pItem = mpImpl->mwMenu->FindItem( itemid );
   if ( pItem ) {
      pItem->SetItemLabel( label.Full().Translation() );
      return true;
   }
   else
      return false;
}

struct Handle::Iterator::Position {
   Position( Menu *pMenu, bool begin )
      : mwMenu{ pMenu }
   {
      if (pMenu) {
         auto items = pMenu->GetMenuItems();
         if (begin)
            mIterator = items.begin();
         else
            mIterator = items.end();
      }
   }

   using iterator = wxMenuItemList::const_iterator;

   wxWeakRef<const Menu> mwMenu;
   iterator mIterator;
   size_t mIndex = 0; //! Does not distinguish the end iterator
};

Handle::Iterator::Iterator( const Handle &handle, bool begin )
: mpPosition{ std::make_unique<Position>( handle.mpImpl->mwMenu, begin ) }
{}

Handle::Iterator::Iterator( Iterator && ) = default;

Handle::Iterator::~Iterator() = default;

auto Handle::Iterator::operator++() -> Iterator &
{
   if (mpPosition) {
      ++mpPosition->mIterator;
      ++mpPosition->mIndex;
   }
   return *this;
}

Info Handle::Iterator::operator *()
{
   if (!mpPosition || !mpPosition->mwMenu)
      return {};

   auto &item = **mpPosition->mIterator;
   ID itemid = item.GetId();
   Type type = toItemType( item.GetKind() );

   // Retrieve the TranslatableString, not the item text wxWidgets holds
   auto label = mpPosition->mwMenu->mLabels[ mpPosition->mIndex ];

   auto accel = item.GetItemLabel();
   if( accel.Contains("\t") )
      accel = accel.AfterLast('\t');
   else
      accel = "";
   State state{ item.IsEnabled(), item.IsChecked() };

   return { itemid, type, label, accel, state,
      { dynamic_cast< Menu* >( item.GetSubMenu() ) } };
}

bool operator == (
   const Handle::Iterator &x, const Handle::Iterator & y )
{
   return
      //! Note that moved-from iterator never compares equal
      x.mpPosition && y.mpPosition &&
      x.mpPosition->mwMenu == y.mpPosition->mwMenu &&
      x.mpPosition->mIterator == y.mpPosition->mIterator;
}

bool operator == ( const Handle &x, const Handle &y )
{
   return x.mpImpl->mwMenu == y.mpImpl->mwMenu;
}


class BarHandle::MenuBar : public wxMenuBar
{
public:
   using wxMenuBar::wxMenuBar;
   ~MenuBar() override = default;

   // Remember the unstranslated strings
   TranslatableStrings mTitles;
};

struct BarHandle::Impl {
   Impl() = default;

   explicit Impl( std::unique_ptr<MenuBar> uMenuBar )
      : muMenuBar{ move( uMenuBar ) }
      , mwMenuBar{ muMenuBar.get() }
   {}

   explicit Impl( wxWeakRef<MenuBar> wMenuBar )
      : muMenuBar{}, mwMenuBar{ wMenuBar }
   {}

   explicit Impl( wxFrame *pFrame )
      : muMenuBar{}
      , mwMenuBar{ dynamic_cast<MenuBar*>(
         pFrame ? pFrame->GetMenuBar() : nullptr ) }
   {}

   std::unique_ptr< MenuBar > muMenuBar;
   wxWeakRef< MenuBar > mwMenuBar;
};

BarHandle::BarHandle()
   : mpImpl{ std::make_unique<Impl>() }
{}

BarHandle::BarHandle( FreshMenu_t )
   : mpImpl{ std::make_unique<Impl>( std::make_unique<MenuBar>() ) }
{}

BarHandle::BarHandle( const BasicUI::WindowPlacement &frame )
   : mpImpl{ std::make_unique<Impl>(
      dynamic_cast<wxFrame*>(
         wxWidgetsWindowPlacement::GetParent( frame ) ) ) }
{}

BarHandle::BarHandle( const BarHandle &other )
   : mpImpl{ std::make_unique<Impl>( other.mpImpl->mwMenuBar ) }
{}

BarHandle::BarHandle( BarHandle &&other )
   : mpImpl{ std::make_unique<Impl>( move( other.mpImpl->muMenuBar ) ) }
{}

BarHandle &BarHandle::operator =( BarHandle &&other )
{
   //! Note other.mpImpl remains non-null
   *this->mpImpl = std::move( *other.mpImpl );
   return *this;
}

BarHandle::~BarHandle() = default;

BarHandle::operator bool() const
{
   return mpImpl->mwMenuBar;
}

void BarHandle::Append(
   Handle &&menu, const TranslatableString &title )
{
   if (!mpImpl->mwMenuBar)
      return;
   if (auto &uMenu = menu.mpImpl->muMenu) {
      mpImpl->mwMenuBar->Append( uMenu.release(), title.Translation() );
      mpImpl->mwMenuBar->mTitles.push_back( title );
   }
}

#ifdef __WXMAC__
void BarHandle::MacSetCommonMenuBar( BarHandle &&pMenuBar )
{
   wxMenuBar::MacSetCommonMenuBar( pMenuBar.mpImpl->muMenuBar.release() );
}
#endif

namespace {
// Get hackcess to a protected method
class wxFrameEx : public wxFrame
{
public:
   using wxFrame::DetachMenuBar;
};
}

void BarHandle::AttachTo( const BasicUI::WindowPlacement &frame ) &&
{
   if (auto pFrame = dynamic_cast<wxFrame *>(
       wxWidgetsWindowPlacement::GetParent( frame ) )
   ) {
      // Delete the menus, since we will soon recreate them.
      // Rather oddly, the menus don't vanish as a result of doing this.
      auto &window = static_cast<wxFrameEx&>( *pFrame );
      auto oldMenuBar = window.GetMenuBar();
      if ( oldMenuBar != mpImpl->muMenuBar.get() ) {
         wxWindowPtr<wxMenuBar>{ oldMenuBar };
         window.DetachMenuBar();
         window.SetMenuBar( mpImpl->muMenuBar.release() );
         // oldMenuBar gets deleted here
      }
   }
}

auto BarHandle::begin() const -> Iterator
{
   return { *this, true };
}

auto BarHandle::end() const -> Iterator
{
   return { *this, false };
}

struct BarHandle::Iterator::Position {
   Position( MenuBar *pMenuBar, bool begin )
      : mwMenuBar{ pMenuBar }
   {
      if (pMenuBar) {
         if (!begin)
            mIndex = pMenuBar->GetMenuCount();
      }
   }

   wxWeakRef<const MenuBar> mwMenuBar;
   size_t mIndex = 0; //! Needed to distinguish the end iterator
};

BarHandle::Iterator::Iterator( const BarHandle &p, bool begin )
: mpPosition{ std::make_unique<Position>( p.mpImpl->mwMenuBar, begin ) }
{}

BarHandle::Iterator::Iterator( Iterator && ) = default;

BarHandle::Iterator::~Iterator() = default;

auto BarHandle::Iterator::operator++() -> Iterator &
{
   if (mpPosition)
      ++mpPosition->mIndex;
   return *this;
}

BarInfo BarHandle::Iterator::operator *()
{
   if (!mpPosition)
      return {};
   if (const MenuBar *pMenuBar = mpPosition->mwMenuBar) {
      auto title = pMenuBar->mTitles[ mpPosition->mIndex ];
      return { title,
         { static_cast< Handle::Menu* >(
            pMenuBar->GetMenu( mpPosition->mIndex ) ) } };
   }
   else
      return {};
}

bool operator == (
   const BarHandle::Iterator &x, const BarHandle::Iterator & y )
{
   return
      //! Note that moved-from iterator never compares equal
      x.mpPosition && y.mpPosition &&
      x.mpPosition->mwMenuBar == y.mpPosition->mwMenuBar &&
      x.mpPosition->mIndex == y.mpPosition->mIndex;
}

}
