/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicMenu.h
@brief Abstractions of menus and their items

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_BASIC_MENU__
#define __AUDACITY_BASIC_MENU__

#include "BasicUIPoint.h"
#include <functional>
#include <optional>
#include "Internat.h"
#include "../commands/Keyboard.h"

namespace BasicUI{ class WindowPlacement; }

class wxMenu; // To be removed

namespace BasicMenu {

using Point = BasicUI::Point;

namespace Item {

//! Identifies menu items
using ID = int;
static constexpr ID InvalidID = -1;

   //! Types of menu items
enum class Type {
    Separator = -1,
    Normal,
    Check,
    Radio,
    SubMenu,
};

//! Describes actual or requested state of a menu item
struct State {
   //! Mask bits to specify a subset of state
   enum : unsigned {
      Enable = 0x01,
      Check = 0x02,
   };

   //! Implicitly constructible from one bool, just for enabled state
   State( bool enable = true, bool check = false )
      : enabled{ enable }, checked{ check }
   {}

   bool enabled;
   bool checked;
};
   
//! Determines user-visible text on the menu button
struct AUDACITY_DLL_API Label {
   TranslatableString main;
   NormalizedKeyString accel;

   Label() = default;
   Label( const TranslatableString &main,
      const NormalizedKeyString &accel = {} )
      : main{ main }, accel{ accel }
   {}

   //! Computes the full label text
   TranslatableString Full() const;
};

//! Full menu texts including an optional help string
struct Text {
   Label label;
   std::optional<TranslatableString> pHelp;

   Text() = default;
   Text( const TranslatableString &main,
      const NormalizedKeyString &accel = {} )
      : label{ main, accel }
   {}
   Text( const TranslatableString &main,
      const NormalizedKeyString &accel,
      const TranslatableString &help )
      : label{ main, accel }
      , pHelp{ help }
   {}
   Text( const Label &label)
      : label{ label }
   {}
   Text( const Label &label, const TranslatableString &help )
      : label{ label }
      , pHelp{ help }
   {}

   TranslatableString GetHelp() const
   {
      return pHelp ? *pHelp : label.main.Stripped();
   }
};

//! Callback to associate with a menu item
using Action = std::function< void() >;

}

class AUDACITY_DLL_API Handle
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

