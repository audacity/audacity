/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudLocationDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

namespace audacity::cloud::audiocom::sync {
enum class LocationDialogResult
{
    Local,
    Cloud,
    Cancel,
};

enum class LocationDialogType
{
    Save,
    Export,
};

class CloudLocationDialog final : private wxDialogWrapper
{
public:
    CloudLocationDialog(wxWindow* parent, LocationDialogType type);
    ~CloudLocationDialog() override;

    LocationDialogResult ShowDialog();

private:
    LocationDialogType mType;
    bool mDoNotShow;
};
} // namespace audacity::cloud::audiocom::sync
