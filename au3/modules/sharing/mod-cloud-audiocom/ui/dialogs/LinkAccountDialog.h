/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkAccountDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "AudioComDialogBase.h"

namespace audacity::cloud::audiocom::sync {
class LinkAccountDialog final : public AudioComDialogBase
{
public:
    LinkAccountDialog(
        const AudacityProject* project, const TranslatableString& alternativeButtonText = {});
    ~LinkAccountDialog() override = default;

    static DialogButtonIdentifier AlternativeButtonIdentifier();
    static DialogButtonIdentifier SignInButtonIdentifier();
}; // class LinkAccountDialog
} // namespace audacity::cloud::audiocom::sync
