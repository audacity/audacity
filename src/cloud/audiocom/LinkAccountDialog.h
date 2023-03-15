/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkAccountDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

class wxButton;
class wxTextCtrl;

namespace cloud::audiocom
{
class LinkAccountDialog final : public wxDialogWrapper
{
public:
   explicit LinkAccountDialog(wxWindow* parent = nullptr);
   ~LinkAccountDialog() override;

private:
   void OnContinue();
   void OnTextChanged();

   wxButton* mContinueButton { nullptr };
   wxTextCtrl* mToken { nullptr };
};
} // namespace cloud::audiocom
