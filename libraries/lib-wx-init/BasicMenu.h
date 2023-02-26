/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicMenu.h
@brief Abstractions of menus and their items

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_BASIC_MENU__
#define __AUDACITY_BASIC_MENU__

#include "BasicUIPoint.h"

namespace BasicUI{ class WindowPlacement; }

class wxMenu; // To be removed

namespace BasicMenu {

using Point = BasicUI::Point;

class WX_INIT_API Handle
{
public:

   explicit Handle( wxMenu *pMenu ) : mpMenu{ pMenu } {}

   Handle( const Handle &other ) = delete;
   Handle &operator =( const Handle &other ) = delete;

   //! Display the menu at pos, invoke at most one action, then hide it
   void Popup( const BasicUI::WindowPlacement &window,
      const Point &pos = {} );

private:
   wxMenu *mpMenu;
};

}

#endif

