/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file JournalWindowPaths.cpp

  Paul Licameli

*********************************************************************/

#include "JournalWindowPaths.h"

#include <wx/stattext.h>
#include <wx/toplevel.h>
#include "BasicUI.h"
#include "Identifier.h"
#include "Theme.h"
#include "wxArrayStringEx.h"
#include "widgets/auStaticText.h"


namespace Journal {

namespace WindowPaths {

namespace {

constexpr auto PathSeparator = ':';
constexpr auto EscapeCharacter = '\\';

/*! @return a predicate, on a pointer to a window,
   that it has the given name.
*/
inline auto HasName( const wxString &name )
{
   return [name](const wxWindow *pWindow2){
      return pWindow2 &&
      // Ignore wxStaticText windows, which ShuttleGui may assign the same
      // name as the related control.  Those windows are non-interactive so
      // don't interest us for journalling.
      !dynamic_cast<const wxStaticText *>( pWindow2 ) &&
      // Also excluse this special window type defined in Audacity.
      !dynamic_cast<const auStaticText *>( pWindow2 ) &&
      pWindow2->GetName() == name; };
}

/*! @return a predicate, on a pointer to a window,
   that it has the same name as the given window.
*/
inline auto SameName( const wxWindow &window )
{
   return HasName( window.GetName() );
}

//! @return unique window in the list satisfying the predicate, or null
template< typename Pred >
wxWindow *UniqueWindowAmongPeers(
   const wxWindowList &list, const Pred &pred )
{
   const auto begin = list.begin(), end = list.end();
   auto iter1 = std::find_if(begin, end, pred);
   if (iter1 == end)
      return nullptr;
   auto next = iter1, iter2 = std::find_if(++next, end, pred);
   if (iter2 != end)
      return nullptr;
   return *iter1;
}

//! @return whether the window is uniquely named within the given list
bool HasUniqueNameAmongPeers( const wxWindow &window, const wxWindowList &list )
{
   return &window == UniqueWindowAmongPeers(list, SameName(window));
}

// Find the unique window in the list for the given name, if there is such
wxWindow *FindByNameAmongPeers(
   const wxString &name, const wxWindowList &list )
{
   return UniqueWindowAmongPeers(list, HasName(name));
}

// Find array of window names, starting with a top-level window and ending
// with the given window.  Also find out whether the window for that path is
// unique
void FindPathComponents(
   const wxWindow &window, std::pair<wxArrayStringEx, bool> &results )
{
   bool unique = false;
   if ( auto pParent = window.GetParent() ) {
      // Recur
      FindPathComponents( *pParent, results );
      unique = HasUniqueNameAmongPeers( window, pParent->GetChildren() );
   }
   else if ( dynamic_cast<const wxTopLevelWindow*>( &window ) )
      unique = HasUniqueNameAmongPeers( window, wxTopLevelWindows );

   auto &components = results.first;
   components.push_back( window.GetName() );

   if (!unique)
      results.second = false;
}

// Find a path; second member of result tells whether window's path is unique
std::pair<wxArrayStringEx, bool> PathComponents( const wxWindow &window )
{
   std::pair<wxArrayStringEx, bool> results{ {}, true };
   FindPathComponents( window, results );
   return results;
}

}

Path FindPath( const wxWindow &window )
{
   auto [components, complete] = PathComponents( window );
   if (!complete) {
#ifdef IS_ALPHA
      BasicUI::CallAfter( [components = std::move(components)]{
         BasicUI::ShowMessageBox(
            Verbatim("Non-unique window at path %s")
               .Format( wxJoin(components, PathSeparator) ) );
      } );
#endif
      components.clear();
   }
   return wxJoin( components, PathSeparator, EscapeCharacter );
}

wxWindow *FindByPath( const Path &path )
{
   auto components = wxSplit( path.GET(), PathSeparator, EscapeCharacter );
   if ( !components.empty() ) {
      auto iter = components.begin(), end = components.end();
      auto pWindow = FindByNameAmongPeers( *iter++, wxTopLevelWindows );
      while ( pWindow && iter != end )
         pWindow = FindByNameAmongPeers( *iter++, pWindow->GetChildren() );
      return pWindow;
   }
   return nullptr;
}

}

}
