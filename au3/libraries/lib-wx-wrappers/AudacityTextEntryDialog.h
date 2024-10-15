/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file AudacityTextEntryDialog.h
 
 Paul Licameli split from ErrorDialog.h
 
 **********************************************************************/

#ifndef __AUDACITY_TEXT_ENTRY_DIALOG__
#define __AUDACITY_TEXT_ENTRY_DIALOG__

#include <wx/textdlg.h> // to inherit
#include "wxPanelWrapper.h" // to inherit

/**************************************************************************//**
\class AudacityTextEntryDialog
\brief Wrap wxTextEntryDialog so that caption IS translatable.
********************************************************************************/
class WX_WRAPPERS_API AudacityTextEntryDialog
   : public wxTabTraversalWrapper< wxTextEntryDialog >
{
public:
    AudacityTextEntryDialog(
         wxWindow *parent,
         const TranslatableString& message,
         const TranslatableString& caption, // don't use = wxGetTextFromUserPromptStr,
         const wxString& value = {},
         long style = wxTextEntryDialogStyle,
         const wxPoint& pos = wxDefaultPosition)
   : wxTabTraversalWrapper< wxTextEntryDialog>(
      parent,
      message.Translation(), caption.Translation(), value, style, pos )
   {}
   
   void SetInsertionPointEnd();
   bool Show(bool show = true) override;

private:
   bool mSetInsertionPointEnd{};
};

#endif // __AUDACITY_ERRORDIALOG__
