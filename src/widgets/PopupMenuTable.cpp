/**********************************************************************

Audacity: A Digital Audio Editor

PopupMenuTable.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../Audacity.h"
#include "PopupMenuTable.h"

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

PopupMenu::~PopupMenu()
{
   // Event connections between the parent window and the singleton table
   // object must be broken when this menu is destroyed.
   Disconnect();
}

void PopupMenu::Extend(PopupMenuTable *pTable)
{
   pTable->InitUserData(pUserData);

   auto connect = [&]( const PopupMenuTable::Entry *pEntry ) {
      this->pParent->Bind
         (wxEVT_COMMAND_MENU_SELECTED,
         pEntry->func, pTable, pEntry->id);
   };

   for (const PopupMenuTable::Entry *pEntry = &*pTable->Get().begin();
      pEntry->IsValid(); ++pEntry) {
      switch (pEntry->type) {
      case PopupMenuTable::Entry::Item:
      {
         this->Append(pEntry->id, pEntry->caption.Translation());
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::RadioItem:
      {
         this->AppendRadioItem(pEntry->id, pEntry->caption.Translation());
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::CheckItem:
      {
         this->AppendCheckItem(pEntry->id, pEntry->caption.Translation());
         connect( pEntry );
         break;
      }
      case PopupMenuTable::Entry::Separator:
         this->AppendSeparator();
         break;
      case PopupMenuTable::Entry::SubMenu:
      {
         const auto subTable = pEntry->subTable;
         auto subMenu =
            PopupMenuTable::BuildMenu( this->pParent, subTable, pUserData );
         this->AppendSubMenu(
            subMenu.release(), subTable->Caption().Translation());
      }
      default:
         break;
      }
   }

   pTable->InitMenu(this);
   tables.push_back( pTable );
}

void PopupMenu::DisconnectTable(PopupMenuTable *pTable)
{
   for (const PopupMenuTable::Entry *pEntry = &*pTable->Get().begin();
      pEntry->IsValid(); ++pEntry) {
      if ( pEntry->IsItem() )
         pParent->Unbind( wxEVT_COMMAND_MENU_SELECTED,
         pEntry->func, pTable, pEntry->id );
      else if ( pEntry->IsSubMenu() )
         // recur
         DisconnectTable(pEntry->subTable);
   }

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
   theMenu->Extend(pTable);
   return theMenu;
}

void PopupMenuTable::ExtendMenu( wxMenu &menu, PopupMenuTable &otherTable )
{
   dynamic_cast<PopupMenu&>(menu).Extend(&otherTable);
}
