/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkFailedDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

namespace cloud::audiocom
{
class LinkFailedDialog final : public wxDialogWrapper
{
public:
   explicit LinkFailedDialog(wxWindow* parent);
   ~LinkFailedDialog() override;

   
}; // class LinkFailedDialog
} // namespace cloud::audiocom
