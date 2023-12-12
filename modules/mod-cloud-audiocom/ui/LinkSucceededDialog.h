/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkSucceededDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

namespace cloud::audiocom
{
class LinkSucceededDialog final : public wxDialogWrapper
{
public:
   explicit LinkSucceededDialog(wxWindow* parent);
   ~LinkSucceededDialog() override;

}; // class LinkSucceededDialog
} // namespace cloud::audiocom
