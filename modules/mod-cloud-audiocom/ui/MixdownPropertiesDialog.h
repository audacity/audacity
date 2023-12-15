/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownPropertiesDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

class wxRadioButton;
class wxChoice;

namespace cloud::audiocom::sync
{

class MixdownPropertiesDialog final : public wxDialogWrapper
{
   MixdownPropertiesDialog(wxWindow* parent);
   ~MixdownPropertiesDialog() override;

public:
   static int Show(wxWindow* parent);

   void SetFrequency(int frequency);
   int GetFrequency() const;
private:
   wxRadioButton* mRBNever {};
   wxRadioButton* mRBAlways {};
   wxRadioButton* mRBEvery {};

   wxChoice* mSavesFrequencyChoice {};
}; // class MixdownPropertiesDialog

} // namespace cloud::audiocom::sync
