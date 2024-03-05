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
enum class SaveLocationDialogResult
{
   Local,
   Cloud,
   Cancel,
};

class SelectSaveLocationDialog final : private wxDialogWrapper
{
public:
   explicit SelectSaveLocationDialog(wxWindow* parent);
   ~SelectSaveLocationDialog() override;

   SaveLocationDialogResult ShowDialog();
};
} // namespace audacity::cloud::audiocom::sync
