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

namespace audacity::cloud::audiocom
{
class LinkWithTokenDialog final : public wxDialogWrapper
{
public:
   explicit LinkWithTokenDialog(wxWindow* parent = nullptr);
   ~LinkWithTokenDialog() override;

private:
   void OnContinue();
   void OnTextChanged();

   wxButton* mContinueButton { nullptr };
   wxTextCtrl* mToken { nullptr };
};
} // namespace audacity::cloud::audiocom
