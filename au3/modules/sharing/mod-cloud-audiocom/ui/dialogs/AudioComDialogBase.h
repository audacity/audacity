/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComDialogBase.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <functional>
#include <string>
#include <vector>

#include "Identifier.h"
#include "TranslatableString.h"
#include "wxPanelWrapper.h"

class AudacityProject;

class wxBoxSizer;

namespace audacity::cloud::audiocom::sync {
struct DialogIdentifierTag
{
};
using DialogIdentifier = TaggedIdentifier<DialogIdentifierTag>;

struct DialogButtonIdentifierTag
{
};
using DialogButtonIdentifier = TaggedIdentifier<DialogButtonIdentifierTag>;

class AudioComDialogBase : wxDialogWrapper
{
public:
    DialogButtonIdentifier
    ShowDialog(std::function<DialogButtonIdentifier()> poller = {});

    static DialogButtonIdentifier CancelButtonIdentifier();

protected:
    enum class DialogMode
    {
        Opening,
        Saving,
    };

    AudioComDialogBase(
        const AudacityProject* project, const DialogIdentifier& optionalPrefsIdentifier = {},
        DialogMode dialogMode                           = DialogMode::Saving);

    virtual ~AudioComDialogBase() = default;

    void AddTitle(const TranslatableString& title);
    void AddParagraph(const TranslatableString& paragraph);

    enum ButtonType
    {
        None          = 0,
        DefaultButton = 1,
        EscButton     = 2,
    };

    void AddButton(
        DialogButtonIdentifier identifier, const TranslatableString& text, int type = None);

    void SetDialogTitle(const TranslatableString& dialog);

    virtual bool HasSeparator() const;

    void EndDialog(DialogButtonIdentifier identifier);

private:
    const AudacityProject* mProject;
    DialogIdentifier mOptionalPrefsIdentifier;

    wxBoxSizer* mDialogSizer;
    wxBoxSizer* mButtonSizer;

    DialogButtonIdentifier mEscButtonIdentifier { CancelButtonIdentifier() };
    DialogButtonIdentifier mResultButtonIdentifier;
}; // class AudioComDialogBase

void ShowDialogOn(
    std::function<bool()> condition, std::function<void()> dialogFactory);
} // namespace audacity::cloud::audiocom::sync
