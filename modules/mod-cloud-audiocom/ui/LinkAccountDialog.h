/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkAccountDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

namespace cloud::audiocom
{
class LinkAccountDialog final : public wxDialogWrapper
{
public:
   explicit LinkAccountDialog(wxWindow* parent);
   ~LinkAccountDialog() override;

}; // class LinkAccountDialog
} // namespace cloud::audiocom
