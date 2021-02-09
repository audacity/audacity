/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file AudacityTextEntryDialog.cpp
 
 Paul Licameli split from ErrorDialog.cpp
 
 **********************************************************************/

#include "AudacityTextEntryDialog.h"

void AudacityTextEntryDialog::SetInsertionPointEnd()
{
   mSetInsertionPointEnd = true;
}

bool AudacityTextEntryDialog::Show(bool show)
{
   bool ret = wxTabTraversalWrapper< wxTextEntryDialog >::Show(show);

   if (show && mSetInsertionPointEnd) {
      // m_textctrl is protected member of wxTextEntryDialog
      m_textctrl->SetInsertionPointEnd();
   }

   return ret;
}
