/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SelectSaveLocationDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

namespace audacity::cloud::audiocom::sync
{
class SelectSaveLocationDialog final : public wxDialogWrapper
{
public:
   explicit SelectSaveLocationDialog(wxWindow* parent);
   ~SelectSaveLocationDialog() override;
};
} // namespace audacity::cloud::audiocom::sync
