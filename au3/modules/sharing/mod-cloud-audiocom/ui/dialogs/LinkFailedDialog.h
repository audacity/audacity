/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkFailedDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
class LinkFailedDialog final : public wxDialogWrapper
{
public:
    LinkFailedDialog(wxWindow* parent, AudiocomTrace trace);
    ~LinkFailedDialog() override;
}; // class LinkFailedDialog
} // namespace audacity::cloud::audiocom
