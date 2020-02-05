/**********************************************************************

Audacity: A Digital Audio Editor

PopupMenuTable.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../Audacity.h"
#include "PopupMenuTable.h"

PopupMenuTableEntry::~PopupMenuTableEntry()
{}

PopupSubMenu::~PopupSubMenu()
{}

PopupSubMenu::PopupSubMenu( const Identifier &stringId,
   const TranslatableString &caption_, PopupMenuTable &table )
   : ConcreteGroupItem< false >{ stringId }
   , caption{ caption_ }
{
}

namespace {
struct PopupMenu : public wxMenu
{
   PopupMenu(wxEvtHandler *pParent_, void *pUserData_)
      : pParent{ pParent_ }, tables{}, pUserData{ pUserData_ } {}

   ~PopupMenu() override;

   void Extend(PopupMenuTable *pTable);
   void DisconnectTable(PopupMenuTable *pTable);
   void Disconnect();

   wxEvtHandler *pParent;
   std::vector<PopupMenuTable*> tables;
   void *pUserData;
};

class PopupMenuBuilder : public MenuVisitor {
public:
   explicit
   PopupMenuBuilder( PopupMenuTable &table, PopupMenu &menu, void *pUserData )
      : mTable{ table }
      , mMenu{ &menu }
      , mRoot{ mMenu }
      , mpUserData{ pUserData }
   {}

   void DoBeginGroup( Registry::GroupItem &item, const Path &path ) override;
   void DoEndGroup( Registry::GroupItem &item, const Path &path ) override;
   void DoVisit( Registry::SingleItem &item, const Path &path ) override;
   void DoSeparator() override;

   PopupMenuTable &mTable;
   std::vector< std::unique_ptr<PopupMenu> > mMenus;
   PopupMenu *mMenu, *mRoot;
   void *const mpUserData;
};

void PopupMenuBuilder::DoBeginGroup( Registry::GroupItem &item, const Path &path )
{
   if ( auto pItem = dynamic_cast<PopupSubMenu*>(&item) ) {
      if ( !pItem->caption.empty() ) {
         auto newMenu =
            std::make_unique<PopupMenu>( mMenu->pParent, mMenu->pUserData );
         mMenu = newMenu.get();
         mMenus.push_back( std::move( newMenu ) );
      }
   }
}

void PopupMenuBuilder::DoEndGroup( Registry::GroupItem &item, const Path &path )
{
   if ( auto pItem = dynamic_cast<PopupSubMenu*>(&item) ) {
      if ( !pItem->caption.empty() ) {
         auto subMenu = std::move( mMenus.back() );
         mMenus.pop_back();
         mMenu = mMenus.empty() ? mRoot : mMenus.back().get();
         mMenu->AppendSubMenu( subMenu.release(), pItem->caption.Translation());
      }
   }
}

void PopupMenuBuilder::DoVisit( Registry::SingleItem &item, const Path &path )
{
   auto connect = [this]( const PopupMenuTable::Entry *pEntry ) {
      mMenu->pParent->Bind
         (wxEVT_COMMAND_MENU_SELECTED,
         pEntry->func, &mTable, pEntry->id);
   };

   auto pEntry = static_cast<PopupMenuTableEntry*>( &item );
   switch (pEntry->type) {
      case PopupMenuTable::Entry::Item:
      {
         mMenu->Append(pEntry->id, pEntry->caption.Translation());
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::RadioItem:
      {
         mMenu->AppendRadioItem(pEntry->id, pEntry->caption.Translation());
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::CheckItem:
      {
         mMenu->AppendCheckItem(pEntry->id, pEntry->caption.Translation());
         connect( pEntry );
         break;
      }
      default:
         break;
   }
}

void PopupMenuBuilder::DoSeparator()
{
   mMenu->AppendSeparator();
}

PopupMenu::~PopupMenu()
{
   // Event connections between the parent window and the singleton table
   // object must be broken when this menu is destroyed.
   Disconnect();
}
}

void PopupMenuTable::ExtendMenu( wxMenu &menu, PopupMenuTable &table )
{
   auto &theMenu = dynamic_cast<PopupMenu&>(menu);

   table.InitUserData(theMenu.pUserData);

   PopupMenuBuilder visitor{ table, theMenu, theMenu.pUserData };
   Registry::Visit( visitor, table.Get().get() );

   table.InitMenu( &menu );
   theMenu.tables.push_back( &table );
}

namespace{
void PopupMenu::DisconnectTable(PopupMenuTable *pTable)
{
   class PopupMenuDestroyer : public MenuVisitor {
   public:
      explicit
      PopupMenuDestroyer( PopupMenuTable &table, PopupMenu &menu )
         : mTable{ table }
         , mMenu{ menu }
      {}

      void DoVisit( Registry::SingleItem &item, const Path &path ) override
      {
         auto pEntry = static_cast<PopupMenuTableEntry*>( &item );
         mMenu.pParent->Unbind( wxEVT_COMMAND_MENU_SELECTED,
         pEntry->func, &mTable, pEntry->id );
      }

      PopupMenuTable &mTable;
      PopupMenu &mMenu;
   };

   PopupMenuDestroyer visitor{ *pTable, *this };
   Registry::Visit( visitor, pTable->Get().get() );

   pTable->DestroyMenu();
}

void PopupMenu::Disconnect()
{
   for ( auto pTable : tables )
      DisconnectTable(pTable);
}
}

void PopupMenuTable::InitMenu(wxMenu *)
{
}

// static
std::unique_ptr<wxMenu> PopupMenuTable::BuildMenu
( wxEvtHandler *pParent, PopupMenuTable *pTable, void *pUserData )
{
   // Rebuild as needed each time.  That makes it safe in case of language change.
   auto theMenu = std::make_unique<PopupMenu>( pParent, pUserData );
   ExtendMenu( *theMenu, *pTable );
   return theMenu;
}

