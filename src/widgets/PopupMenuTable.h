/**********************************************************************

Audacity: A Digital Audio Editor

PopupMenuTable.h

Paul Licameli

This file defines PopupMenuTable, which inherits from wxEventHandler,

associated macros simplifying the population of tables,

and PopupMenuTable::Menu which is buildable from one or more such
tables, and automatically attaches and detaches the event handlers.

**********************************************************************/

#ifndef __AUDACITY_POPUP_MENU_TABLE__
#define __AUDACITY_POPUP_MENU_TABLE__

class wxCommandEvent;
class wxString;
#include <functional>
#include <vector>
#include <wx/menu.h> // to inherit wxMenu
#include "../MemoryX.h"

#include "../Internat.h"
#include "../commands/CommandManager.h"

class PopupMenuHandler;
class PopupMenuTable;

struct PopupMenuTableEntry : Registry::SingleItem
{
   enum Type { Item, RadioItem, CheckItem };
   using InitFunction =
      std::function< void( PopupMenuHandler &handler, wxMenu &menu, int id ) >;

   Type type;
   int id;
   TranslatableString caption;
   wxCommandEventFunction func;
   PopupMenuHandler &handler;
   InitFunction init;

   PopupMenuTableEntry( const Identifier &stringId,
      Type type_, int id_, const TranslatableString &caption_,
      wxCommandEventFunction func_, PopupMenuHandler &handler_,
      InitFunction init_ )
      : SingleItem{ stringId }
      , type(type_)
      , id(id_)
      , caption(caption_)
      , func(func_)
      , handler( handler_ )
      , init( init_ )
   {}

   ~PopupMenuTableEntry() override;
};

struct PopupSubMenu : Registry::ConcreteGroupItem< false >
   , MenuTable::WholeMenu
{
   TranslatableString caption;
   PopupMenuTable &table;

   PopupSubMenu( const Identifier &stringId,
      const TranslatableString &caption_, PopupMenuTable &table );

   ~PopupSubMenu() override;
};

struct PopupMenuSection
   : Registry::ConcreteGroupItem< false >
   , MenuTable::MenuSection {
   using ConcreteGroupItem< false >::ConcreteGroupItem;
};

class PopupMenuHandler : public wxEvtHandler
{
public:
   PopupMenuHandler() = default;
   PopupMenuHandler( const PopupMenuHandler& ) = delete;
   PopupMenuHandler& operator=( const PopupMenuHandler& ) = delete;

   // Called before the menu items are appended.
   // Store context data, if needed.
   // May be called more than once before the menu opens.
   virtual void InitUserData(void *pUserData) = 0;

   // Called when menu is destroyed.
   // May be called more than once.
   virtual void DestroyMenu() = 0;
};

class PopupMenuTable : public PopupMenuHandler
{
public:
   using Entry = PopupMenuTableEntry;

   // Supply a nonempty caption for sub-menu tables
   PopupMenuTable( const Identifier &id, const TranslatableString &caption = {} )
      : mId{ id }
      , mCaption{ caption }
   {}

   // Optional pUserData gets passed to the InitUserData routines of tables.
   // No memory management responsibility is assumed by this function.
   static std::unique_ptr<wxMenu> BuildMenu
      (wxEvtHandler *pParent, PopupMenuTable *pTable, void *pUserData = NULL);

   const Identifier &Id() const { return mId; }
   const TranslatableString &Caption() const { return mCaption; }

   // menu must have been built by BuildMenu
   // More items get added to the end of it
   static void ExtendMenu( wxMenu &menu, PopupMenuTable &otherTable );
   
   const std::shared_ptr< Registry::GroupItem > &Get( void *pUserData )
   {
      this->InitUserData( pUserData );
      if (!mTop)
         Populate();
      return mTop;
   }

protected:
   virtual void Populate() = 0;

   // To be used in implementations of Populate():
   void Append(
      const Identifier &stringId, PopupMenuTableEntry::Type type, int id,
      const TranslatableString &string, wxCommandEventFunction memFn,
      // This callback might check or disable a menu item:
      const PopupMenuTableEntry::InitFunction &init );

   void AppendItem( const Identifier &stringId, int id,
      const TranslatableString &string, wxCommandEventFunction memFn,
      // This callback might check or disable a menu item:
      const PopupMenuTableEntry::InitFunction &init = {} )
   { Append( stringId, PopupMenuTableEntry::Item, id, string, memFn, init ); }

   void AppendRadioItem( const Identifier &stringId, int id,
      const TranslatableString &string, wxCommandEventFunction memFn,
      // This callback might check or disable a menu item:
      const PopupMenuTableEntry::InitFunction &init = {} )
   { Append( stringId, PopupMenuTableEntry::RadioItem, id, string, memFn, init ); }

   void AppendCheckItem( const Identifier &stringId, int id,
      const TranslatableString &string, wxCommandEventFunction memFn,
      const PopupMenuTableEntry::InitFunction &init = {} )
   { Append( stringId, PopupMenuTableEntry::CheckItem, id, string, memFn, init ); }

   void BeginSection( const Identifier &name );
   void EndSection();

   void Clear() { mTop.reset(); }
   
   std::shared_ptr< Registry::GroupItem > mTop;
   std::vector< Registry::GroupItem* > mStack;
   Identifier mId;
   TranslatableString mCaption;
};

/*
The following macros make it easy to attach a popup menu to a window.

Exmple of usage:

In class MyTable (maybe in the private section),
which inherits from PopupMenuTable,

DECLARE_POPUP_MENU(MyTable);
virtual void InitUserData(void *pUserData);
virtual void DestroyMenu();

Then in MyTable.cpp,

void MyTable::InitUserData(void *pUserData)
{
   // Remember pData
   auto pData = static_cast<MyData*>(pUserData);
}

void MyTable::DestroyMenu()
{
   // Do cleanup
}

BEGIN_POPUP_MENU(MyTable)
   // This is inside a function and can contain arbitrary code.  But usually
   // you only need a sequence of calls:

   AppendItem("Cut",
      OnCutSelectedTextID, XO("Cu&t"), POPUP_MENU_FN( OnCutSelectedText ),
      // optional argument:
      [](PopupMenuHandler &handler, wxMenu &menu, int id)
      {
         auto data = static_cast<MyTable&>( handler ).pData;
         // maybe enable or disable the menu item
      }
   );
   // etc.
 
END_POPUP_MENU()

where OnCutSelectedText is a (maybe private) member function of MyTable.

Elswhere,

MyTable myTable;
MyData data;
auto pMenu = PopupMenuTable::BuildMenu(pParent, &myTable, &data);

// Optionally:
OtherTable otherTable;
PopupMenuTable::ExtendMenu( *pMenu, otherTable );

pParent->PopupMenu(pMenu.get(), event.m_x, event.m_y);

That's all!
*/

#define DECLARE_POPUP_MENU(HandlerClass) \
   void Populate() override;

// begins function
#define BEGIN_POPUP_MENU(HandlerClass) \
void HandlerClass::Populate() { \
   using My = HandlerClass; \
   mTop = std::make_shared< PopupSubMenu >( \
         Id(), Caption(), *this ); \
   mStack.clear(); \
   mStack.push_back( mTop.get() );

#define POPUP_MENU_FN( memFn ) ( (wxCommandEventFunction) (&My::memFn) )

#define POPUP_MENU_SUB_MENU(stringId, classname, pUserData ) \
   mStack.back()->items.push_back( \
      Registry::Shared( classname::Instance().Get( pUserData ) ) );

// ends function
#define END_POPUP_MENU() }

#endif
