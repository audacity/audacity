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

class wxPoint;
class wxWindow;

#include <functional>
#include <vector>
#include <memory>

#include "Internat.h"
#include "../commands/CommandManager.h"
#include "BasicMenu.h"

class PopupMenuHandler;
class PopupMenuTable;

struct AUDACITY_DLL_API PopupMenuTableEntry : Registry::SingleItem
{
   enum Type { Item, RadioItem, CheckItem };
   //! Function to determine whether a menu item should be enabled and checked
   using StateFunction = std::function< BasicMenu::Item::State() >;

   Type type;
   int id;
   BasicMenu::Item::Label caption;
   std::function< void() > callback;
   PopupMenuHandler &handler;
   StateFunction stateFn;

   //! @pre func is not null
   PopupMenuTableEntry( const Identifier &stringId,
      Type type_, int id_, const BasicMenu::Item::Label &caption_,
      std::function< void() > callback, PopupMenuHandler &handler_,
      StateFunction stateFn = {} )
      : SingleItem{ stringId }
      , type(type_)
      , id(id_)
      , caption(caption_)
      , callback(move(callback))
      , handler( handler_ )
      , stateFn( move(stateFn) )
   {
   }

   ~PopupMenuTableEntry() override;
};

struct AUDACITY_DLL_API PopupSubMenu : Registry::ConcreteGroupItem< false >
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

   //! Called before the menu items are appended.
   /*! Store context data, if needed.
      May be called more than once before the menu opens.
      Pointer remains valid for the duration of any callback, if
      PopupMenuTable::BuildMenu() is called and the result's Popup() is called
      before any other menus are built.
    */
   virtual void InitUserData(void *pUserData) = 0;
};

struct PopupMenuVisitor : public MenuVisitor {
   explicit PopupMenuVisitor( PopupMenuTable &table ) : mTable{ table } {}
   PopupMenuTable &mTable;
};

// Opaque structure built by PopupMenuTable::BuildMenu
class AUDACITY_DLL_API PopupMenu
{
public:
   virtual ~PopupMenu();
   virtual void Popup( wxWindow &window, const wxPoint &pos ) = 0;
};

class AUDACITY_DLL_API PopupMenuTable : public PopupMenuHandler
{
public:
   using Entry = PopupMenuTableEntry;

   // Supply a nonempty caption for sub-menu tables
   PopupMenuTable( const Identifier &id, const TranslatableString &caption = {} )
      : mId{ id }
      , mCaption{ caption }
      , mRegistry{ std::make_unique<Registry::TransparentGroupItem<>>( mId ) }
   {}

   // Optional pUserData gets passed to the InitUserData routines of tables.
   // No memory management responsibility is assumed by this function.
   static std::unique_ptr<PopupMenu> BuildMenu(
      PopupMenuTable *pTable, void *pUserData = NULL);

   const Identifier &Id() const { return mId; }
   const TranslatableString &Caption() const { return mCaption; }
   const Registry::GroupItem *GetRegistry() const { return mRegistry.get(); }

   // Typically statically constructed:
   struct AttachedItem {
      AttachedItem( PopupMenuTable &table,
         const Registry::Placement &placement, Registry::BaseItemPtr pItem )
      { table.RegisterItem( placement, std::move( pItem ) ); }
   };

   // menu must have been built by BuildMenu
   // More items get added to the end of it
   static void ExtendMenu( PopupMenu &menu, PopupMenuTable &otherTable );
   
   const std::shared_ptr< Registry::GroupItem > &Get( void *pUserData )
   {
      if ( pUserData )
         this->InitUserData( pUserData );
      if (!mTop)
         Populate();
      return mTop;
   }

   void Clear()
   {
      mTop.reset();
   }

   // Forms a computed item, which may be omitted when function returns null
   // and thus can be a conditional item
   template< typename Table >
   static Registry::BaseItemPtr Computed(
      const std::function< Registry::BaseItemPtr( Table& ) > &factory )
   {
      using namespace Registry;
      return std::make_unique< ComputedItem >(
         [factory]( Visitor &baseVisitor ){
            auto &visitor = static_cast< PopupMenuVisitor& >( baseVisitor );
            auto &table =  static_cast< Table& >( visitor.mTable );
            return factory( table );
         }
      );
   }

private:
   void RegisterItem(
      const Registry::Placement &placement, Registry::BaseItemPtr pItem );

protected:
   // This convenience function composes a label, with the following optional
   // part put in parentheses if useExtra is true
   static TranslatableString MakeLabel( const TranslatableString &label,
      bool useExtra, const TranslatableString &extra )
   {
      return useExtra
         ? XXO("%s (%s)").Format( label, extra )
         : label;
   }

   virtual void Populate() = 0;

   // To be used in implementations of Populate():
   void Append( Registry::BaseItemPtr pItem );

   using Callback = std::function<void()>;
   
   void Append(
      const Identifier &stringId, PopupMenuTableEntry::Type type, int id,
      const BasicMenu::Item::Label &string, Callback callback,
      const PopupMenuTableEntry::StateFunction &stateFn );

   void AppendItem( const Identifier &stringId, int id,
      const BasicMenu::Item::Label &string, Callback callback,
      const PopupMenuTableEntry::StateFunction &stateFn = {} )
   { Append( stringId, PopupMenuTableEntry::Item, id, string,
      move(callback), stateFn ); }

   void AppendRadioItem( const Identifier &stringId, int id,
      const BasicMenu::Item::Label &string, Callback callback,
      const PopupMenuTableEntry::StateFunction &stateFn = {} )
   { Append( stringId, PopupMenuTableEntry::RadioItem, id, string,
      move(callback), stateFn ); }

    void AppendCheckItem( const Identifier &stringId, int id,
      const BasicMenu::Item::Label &string, Callback callback,
      const PopupMenuTableEntry::StateFunction &stateFn = {} )
   { Append( stringId, PopupMenuTableEntry::CheckItem, id, string,
      move(callback), stateFn ); }

   void BeginSection( const Identifier &name );
   void EndSection();

   std::shared_ptr< Registry::GroupItem > mTop;
   std::vector< Registry::GroupItem* > mStack;
   Identifier mId;
   TranslatableString mCaption;
   std::unique_ptr<Registry::GroupItem> mRegistry;
};

// A "CRTP" class that injects a convenience function, which appends a menu item
// computed lazily by a function that is passed the table (after it has stored
// its user data)
template< typename Derived, typename Base = PopupMenuTable >
class ComputedPopupMenuTable : public Base
{
public:
   using Base::Base;
   using Base::Append;

   // Appends a computed item, which may be omitted when function returns null
   // and thus can be a conditional item
   using Factory = std::function< Registry::BaseItemPtr( Derived& ) >;
   static Registry::BaseItemPtr Computed( const Factory &factory )
   {
      return Base::Computed( factory );
   }

   void Append( const Factory &factory )
   {
      Append( Computed( factory ) );
   }
};

/*
The following macros make it easy to attach a popup menu to a window.

Example of usage:

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
      [](PopupMenuHandler &handler, BasicMenu::Handle &menu, int id)
      {
         auto data = static_cast<MyTable&>( handler ).pData;
         // maybe enable or disable the menu item
      }
   );
   // etc.
 
END_POPUP_MENU()

where OnCutSelectedText is a (maybe private) member function of MyTable.

Elsewhere,

MyTable myTable;
MyData data;
auto pMenu = PopupMenuTable::BuildMenu(pParent, &myTable, &data);

// Optionally:
OtherTable otherTable;
PopupMenuTable::ExtendMenu( *pMenu, otherTable );

pMenu->Popup( *pParent, { event.m_x, event.m_y } );

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

#define POPUP_MENU_FN( memFn ) ( [this]{ memFn(); } )

#define POPUP_MENU_SUB_MENU(stringId, classname, pUserData ) \
   mStack.back()->items.push_back( \
      Registry::Shared( classname::Instance().Get( pUserData ) ) );

// ends function
#define END_POPUP_MENU() }

#endif
