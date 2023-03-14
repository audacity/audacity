/**********************************************************************

  Audacity: A Digital Audio Editor

  MenuCreator.h

  Dominic Mazzoni

  Paul Licameli split from Menus.h

**********************************************************************/
#ifndef __AUDACITY_MENU_CREATOR__
#define __AUDACITY_MENU_CREATOR__

#include "Menus.h"
#include "Identifier.h"

class AUDACITY_DLL_API MenuCreator final : public MenuManager
{
public:
   static MenuCreator &Get(AudacityProject &project);
   static const MenuCreator &Get(const AudacityProject &project);

   MenuCreator(AudacityProject &project);
   ~MenuCreator() override;
   void CreateMenusAndCommands();
   void RebuildMenuBar();
   static void RebuildAllMenuBars();

   void ModifyUndoMenuItems();

   // checkActive is a temporary hack that should be removed as soon as we
   // get multiple effect preview working
   void UpdateMenus( bool checkActive = true );

   void RemoveDuplicateShortcuts();

private:
   void OnUndoRedo(struct UndoRedoMessage);
   Observer::Subscription mUndoSubscription;
};

#endif
