/**********************************************************************

Audacity: A Digital Audio Editor

PopupMenuTable.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../Audacity.h"
#include "PopupMenuTable.h"

PopupMenuTable::Menu::~Menu()
{
   Disconnect();
}

void PopupMenuTable::Menu::Extend(PopupMenuTable *pTable)
{
   auto connect = [&]( const PopupMenuTable::Entry *pEntry ) {
      this->pParent->Connect
         (pEntry->id, wxEVT_COMMAND_MENU_SELECTED,
         pEntry->func, NULL, pTable);
   };

   for (const PopupMenuTable::Entry *pEntry = &*pTable->Get().begin();
      pEntry->IsValid(); ++pEntry) {
      switch (pEntry->type) {
      case PopupMenuTable::Entry::Item:
      {
         this->Append(pEntry->id, pEntry->caption);
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::RadioItem:
      {
         this->AppendRadioItem(pEntry->id, pEntry->caption);
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::CheckItem:
      {
         this->AppendCheckItem(pEntry->id, pEntry->caption);
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::Separator:
         this->AppendSeparator();
         break;
      case PopupMenuTable::Entry::SubMenu:
      {
         const auto subTable = pEntry->subTable;
         auto subMenu = BuildMenu( this->pParent, subTable, pUserData );
         this->AppendSubMenu( subMenu.release(), pEntry->caption );
      }
      default:
         break;
      }
   }

   pTable->InitMenu(this, pUserData);
}

void PopupMenuTable::Menu::DisconnectTable(PopupMenuTable *pTable)
{
   for (const PopupMenuTable::Entry *pEntry = &*pTable->Get().begin();
      pEntry->IsValid(); ++pEntry) {
      if ( pEntry->IsItem() )
         pParent->Disconnect( pEntry->id, wxEVT_COMMAND_MENU_SELECTED,
         pEntry->func, NULL, pTable );
      else if ( pEntry->IsSubMenu() )
         // recur
         DisconnectTable(pEntry->subTable);
   }

   pTable->DestroyMenu();
}

void PopupMenuTable::Menu::Disconnect()
{
   for ( auto pTable : tables )
      DisconnectTable(pTable);
}

// static
std::unique_ptr<PopupMenuTable::Menu> PopupMenuTable::BuildMenu
( wxEvtHandler *pParent, PopupMenuTable *pTable, void *pUserData )
{
   // Rebuild as needed each time.  That makes it safe in case of language change.
   std::unique_ptr<Menu> theMenu{ safenew Menu( pParent, pUserData ) };
   theMenu->Extend(pTable);
   return std::move( theMenu );
}
