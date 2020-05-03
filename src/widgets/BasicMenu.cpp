/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicMenu.cpp

Paul Licameli

**********************************************************************/

#include "BasicMenu.h"
#include "wxWidgetsWindowPlacement.h"
#include <wx/window.h>

namespace BasicMenu {

void Handle::Popup( const BasicUI::WindowPlacement &window, const Point &pos )
{
   if (auto pWindow = wxWidgetsWindowPlacement::GetParent(window))
      pWindow->PopupMenu( mpMenu, { pos.x, pos.y } );
}

}
