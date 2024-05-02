/*  SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#include "BasicUI.h"

class QQmlEngine;
class QObject;

class QT_INIT_API QtQuickUiServices final : public BasicUI::Services
{
   std::unique_ptr<QQmlEngine> mFallbackQMLEngine;

   QQmlEngine& GetOrCreateEngine(const QObject* object);

   QtQuickUiServices();
   
public:

   static QtQuickUiServices& Get();

   ~QtQuickUiServices() override;
   
   void DoCallAfter(const BasicUI::Action& action) override;
   void DoYield() override;
   void DoShowErrorDialog(const BasicUI::WindowPlacement& placement, const TranslatableString& dlogTitle,
      const TranslatableString& message, const ManualPageID& helpPage,
      const BasicUI::ErrorDialogOptions& options) override;
   BasicUI::MessageBoxResult
   DoMessageBox(const TranslatableString& message, BasicUI::MessageBoxOptions options) override;
   std::unique_ptr<BasicUI::ProgressDialog> DoMakeProgress(const TranslatableString& title,
      const TranslatableString& message, unsigned flag, const TranslatableString& remainingLabelText) override;
   std::unique_ptr<BasicUI::GenericProgressDialog> DoMakeGenericProgress(const BasicUI::WindowPlacement& placement,
      const TranslatableString& title, const TranslatableString& message) override;
   int DoMultiDialog(const TranslatableString& message, const TranslatableString& title,
      const TranslatableStrings& buttons, const ManualPageID& helpPage, const TranslatableString& boxMsg,
      bool log) override;
   bool DoOpenInDefaultBrowser(const wxString& url) override;
   std::unique_ptr<BasicUI::WindowPlacement> DoFindFocus() override;
   void DoSetFocus(const BasicUI::WindowPlacement& focus) override;
   bool IsUsingRtlLayout() const override;
   bool IsUiThread() const override;
};
