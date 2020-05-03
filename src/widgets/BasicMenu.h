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
#include "MemoryX.h"
#include "Internat.h"
#include "../commands/Keyboard.h"
#include <wx/weakref.h>

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

//! Argument type overloading non-default constructors of handles
struct FreshMenu_t{ constexpr FreshMenu_t() = default; };
constexpr FreshMenu_t FreshMenu{};

struct Info;

//! Wraps a menu so that you must supply translatable strings as you build it
/*
Can also act as a weak reference to a menu already inserted into the user
interface, which may be rebuilt or visited with iterators.
This is a cheaply copied or moved handle to a shared menu structure.
*/
class AUDACITY_DLL_API Handle
{
   class Menu;
   friend class BarHandle;
   struct Impl;
   //! @invariant not null
   std::unique_ptr<Impl> mpImpl;

   Handle( Menu *pMenu );

public:

   //! A null handle
   Handle();

   //! Handle to a fresh unshared menu object
   Handle( FreshMenu_t );

   //! Copy construction produces a non-owning weak reference
   Handle( const Handle &other );

   //! There is no copy assignment
   Handle &operator =( const Handle &other ) = delete;

   //! Leave only a weak reference in other
   /*! Transfer the unshared ownership, if other had it */
   Handle( Handle &&other );
   //! Leave only a weak reference in other
   /*! Transfer the unshared ownership, if other had it */
   Handle &operator =( Handle &&other );

   virtual ~Handle();

   //! to be removed
   /*! If constructed with FreshMenu_t and not moved, returns nonnull */
   wxMenu *GetWxMenu() const;

   //! @return false if it doesn't point to a menu
   /*! That is, it was constructed as null, or its weak reference was expired */
   explicit operator bool() const;

   friend bool operator == (
      const Handle &x, const Handle &y );

   friend inline bool operator != (
      const Handle &x, const Handle &y )
   { return !( x == y ); }

   //! @name Building
   //! @{

   //! Construct any kind of menu item except for sub-menus
   /*! No effect if the handle was expired */
   void Append( Item::Type type,
      const Item::Text &text = {},
      Item::Action action = {},
      const Item::State &state = {},
      Item::ID ItemID = Item::InvalidID );

   //! Construct an ordinary menu item
   void Append(
      const Item::Text &text = {},
      Item::Action action = {},
      const Item::State &state = {},
      Item::ID ItemID = Item::InvalidID )
   { Append( Item::Type::Normal, text, move(action), state, ItemID ); }

   /*!
    @brief Construct a menu item presenting an exclusive choice among
    neighboring items of the same type
    */
   void AppendRadioItem(
      const Item::Text &text = {},
      Item::Action action = {},
      const Item::State &state = {},
      Item::ID ItemID = Item::InvalidID )
   { Append( Item::Type::Radio, text, move(action), state, ItemID ); }

   //! Construct a menu item that may have a checkmark independently of others
   void AppendCheckItem(
      const Item::Text &text = {},
      Item::Action action = {},
      const Item::State &state = {},
      Item::ID ItemID = Item::InvalidID )
   { Append( Item::Type::Check, text, move(action), state, ItemID ); }

   //! Make a line between other menu items
   void AppendSeparator()
   { Append( Item::Type::Separator ); }

   //! Construct a menu item that can open as a cascading sub-menu
   /*! submenu gives up unique ownership but retains a weak reference.
       No effect if this handle or submenu was expired */
   void AppendSubMenu( Handle &&submenu,
      const Item::Text &text = {},
      const Item::State &state = {} );

   //! @}

   //! Delete all items
   void Clear();

   //! Display the menu at pos, invoke at most one action, then hide it
   void Popup( const BasicUI::WindowPlacement &window,
      const Point &pos = {} );

   //! Iterator over existing menu items
   /*! Insertion or removal of items invalidates iterators */
   class Iterator : ValueIterator< Info >{
      friend Handle;
      Iterator( const Handle &handle, bool begin );

      struct Position;
      //! May be null if moved-from
      std::unique_ptr< Position > mpPosition;

   public:
      ~Iterator();
      Iterator( Iterator && );
      Iterator &operator ++ ();
      Info operator * ();
      friend bool operator == ( const Iterator &x, const Iterator & y );
      friend inline bool operator != ( const Iterator &x, const Iterator & y )
      { return !( x == y ); }
   };

   Iterator begin() const;
   Iterator end() const;

   //! @name Other item-level accessors and mutators
   //! @{

   Item::State GetState( Item::ID ItemID );

   //! @param mask a bitwise or of enum values defined in MenuItemState
   //! @return true if the item was found
   bool SetState(
      Item::ID ItemID, const Item::State &state, unsigned mask = ~0u );

   //! @return whether the item was found
   bool SetLabel( Item::ID ItemID, const Item::Label& label);

   //! @}
};

//! Information accessed by iterator over menu items
struct Info {
   Item::ID id;
   Item::Type type;
   TranslatableString label;
   wxString accel;
   Item::State state;
   Handle pSubMenu{};
};

//! Information accessed by iterator over menu bar items
struct BarInfo {
   TranslatableString title;
   Handle pSubMenu{};
};

//! Wraps a menu bar built from translatable strings
/*!
Can also act as a weak reference to a menu bar already inserted into the user
interface.
This is a cheaply copied or moved handle to a shared menu bar structure.
*/
class AUDACITY_DLL_API BarHandle
{
   class MenuBar;
   struct Impl;
   //! @invariant not null
   std::unique_ptr<Impl> mpImpl;

public:
   //! A null handle
   BarHandle();

   //! A fresh unshared menu bar object
   BarHandle( FreshMenu_t );

   //! A weak reference to a menu bar attached to a frame window
   BarHandle( const BasicUI::WindowPlacement &frame );

   //! Copy construction produces a non-owning weak reference
   BarHandle( const BarHandle &other );

   //! There is no copy assignment
   BarHandle &operator =( const BarHandle &other ) = delete;

   //! Leave only a weak reference in other
   /*! Transfer the unshared ownership, if other had it */
   BarHandle( BarHandle &&other );
   //! Leave only a weak reference in other
   /*! Transfer the unshared ownership, if other had it */
   BarHandle &operator =( BarHandle &&other );

   virtual ~BarHandle();

   //! @return false if it doesn't point to a menu bar
   /*! That is, it was constructed as null, or its weak reference was expired */
   explicit operator bool() const;

   //! @name Building
   //! @{

   //! menu gives up ownership but retains a weak reference
   /*! No effect if this handle or menu was expired */
   void Append( Handle &&menu, const TranslatableString &title );

   //! @}

   //! Defined for macOS only
   static void MacSetCommonMenuBar( BarHandle &&pMenuBar );

   //! Retain a weak reference to the menu bar
   void AttachTo( const BasicUI::WindowPlacement &frame ) &&;

   //! Iterator over existing drop-down menus
   /*! Insertion or removal of items invalidates iterators */
   class Iterator : ValueIterator< BarInfo >{
      friend BarHandle;
      Iterator( const BarHandle &handle, bool begin );

      struct Position;
      //! May be null if moved-from
      std::unique_ptr< Position > mpPosition;

   public:
      ~Iterator();
      Iterator( Iterator && );
      Iterator &operator ++ ();
      BarInfo operator * ();
      friend bool operator == ( const Iterator &x, const Iterator & y );
      friend inline bool operator != ( const Iterator &x, const Iterator & y )
      { return !( x == y ); }
   };

   Iterator begin() const;
   Iterator end() const;
};
}

#endif

