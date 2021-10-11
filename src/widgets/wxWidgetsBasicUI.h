/*!********************************************************************

Audacity: A Digital Audio Editor

@file wxWidgetsBasicUI.h
@brief Implementation of BasicUI using wxWidgets

Paul Licameli

**********************************************************************/
#ifndef __WXWIDGETS_BASIC_UI__
#define __WXWIDGETS_BASIC_UI__

#include "BasicUI.h"

class wxWindow;

//! An implementation of BasicUI::Services in terms of the wxWidgets toolkit
/*! This is a singleton that doesn't need AUDACITY_DLL_API visibility */
class wxWidgetsBasicUI final : public BasicUI::Services {
public:
   ~wxWidgetsBasicUI() override;

protected:
   void DoCallAfter(const BasicUI::Action &action) override;
   void DoYield() override;
   void DoShowErrorDialog(const BasicUI::WindowPlacement &placement,
      const TranslatableString &dlogTitle,
      const TranslatableString &message,
      const ManualPageID &helpPage,
      const BasicUI::ErrorDialogOptions &options) override;
   BasicUI::MessageBoxResult DoMessageBox(
      const TranslatableString &message,
      BasicUI::MessageBoxOptions options) override;
   std::unique_ptr<BasicUI::ProgressDialog>
   DoMakeProgress(const TranslatableString & title,
      const TranslatableString &message,
      unsigned flags,
      const TranslatableString &remainingLabelText) override;
   std::unique_ptr<BasicUI::GenericProgressDialog>
   DoMakeGenericProgress(const BasicUI::WindowPlacement &placement,
      const TranslatableString &title,
      const TranslatableString &message) override;
};

#endif
