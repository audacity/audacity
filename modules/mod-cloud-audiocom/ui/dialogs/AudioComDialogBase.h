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

namespace cloud::audiocom::sync
{
struct DialogIdentifierTag {};
using DialogIdentifier = TaggedIdentifier<DialogIdentifierTag>;

struct DialogButtonIdentifierTag {};
using DialogButtonIdentifier = TaggedIdentifier<DialogButtonIdentifierTag>;

class AudioComDialogBase : wxDialogWrapper
{
public:
   DialogButtonIdentifier
   ShowDialog(std::function<DialogButtonIdentifier()> poller = {});

   static DialogButtonIdentifier CancellButtonIdentifier();

protected:
   AudioComDialogBase(
      const AudacityProject* project,
      const DialogIdentifier& optionalPrefsIdentifier = {});

   virtual ~AudioComDialogBase() = default;

   void AddTitle(const TranslatableString& title);
   void AddParagraph(const TranslatableString& paragraph);
   void AddParagraphWithLink(
      const TranslatableString& paragraph, const wxString& placeholder,
      const wxString& urlText,
      const std::string& url);

   enum ButtonType
   {
      None = 0,
      DefaultButton = 1,
      EscButton = 2,
   };

   void AddButton(
      DialogButtonIdentifier identifier, const TranslatableString& text,
      int type = None);

   void SetDialogTitle(const TranslatableString& dialog);

   virtual bool HasSeparator() const;

   void EndDialog(DialogButtonIdentifier identifier);
private:
   const AudacityProject* mProject;
   DialogIdentifier mOptionalPrefsIdentifier;

   wxBoxSizer* mDialogSizer;
   wxBoxSizer* mButtonSizer;

   DialogButtonIdentifier mEscButtonIdentifier { CancellButtonIdentifier() };
   DialogButtonIdentifier mResultButtonIdentifier;
}; // class AudioComDialogBase
} // namespace cloud::audiocom::sync
