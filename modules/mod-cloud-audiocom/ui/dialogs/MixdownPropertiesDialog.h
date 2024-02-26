/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownPropertiesDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

namespace audacity::cloud::audiocom::sync
{
class MixdownPrefsPanel;

class MixdownPropertiesDialog final : public wxDialogWrapper
{
   MixdownPropertiesDialog(wxWindow* parent);
   ~MixdownPropertiesDialog() override;

   static void Show(wxWindow* parent);

public:
   static void ShowIfNeeded(wxWindow* parent);

   void SetFrequency(int frequency);
   int GetFrequency() const;

private:
   MixdownPrefsPanel* mMixdownPrefsPanel {};
}; // class MixdownPropertiesDialog

} // namespace audacity::cloud::audiocom::sync
