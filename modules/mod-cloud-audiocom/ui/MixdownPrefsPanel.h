/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownPrefsPanel.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

#include "Observer.h"

class wxRadioButton;
class wxChoice;

namespace audacity::cloud::audiocom::sync
{
struct MixdownPrefsChangedMessage final
{
   int Frequency {};
};

class MixdownPrefsPanel final :
    public wxPanelWrapper,
    public Observer::Publisher<MixdownPrefsChangedMessage>
{
public:
   MixdownPrefsPanel(wxWindow* parent, bool compact);

   void SetFrequency(int frequency);
   int GetFrequency() const;

private:
   void EnableFrequencyChoice(bool enable);
   void OnFrequencyChanged();

   wxRadioButton* mRBNever {};
   wxRadioButton* mRBAlways {};
   wxRadioButton* mRBEvery {};

   wxChoice* mSavesFrequencyChoice {};
}; // class MixdownPrefsPanel
} // namespace audacity::cloud::audiocom::sync
