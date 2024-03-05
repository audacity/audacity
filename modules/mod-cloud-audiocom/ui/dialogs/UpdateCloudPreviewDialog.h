/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UpdateCloudPreviewDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync
{
class UpdateCloudPreviewDialog final : public AudioComDialogBase
{
public:
   explicit UpdateCloudPreviewDialog(const AudacityProject* project);

   static DialogButtonIdentifier RenderPreviewIdentifier();
};
} // namespace audacity::cloud::audiocom::sync
