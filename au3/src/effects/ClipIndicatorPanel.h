/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipIndicatorPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "Observer.h"
#include "wxPanelWrapper.h"

class CompressorInstance;
class wxPaintDC;

class ClipIndicatorPanel final : public wxPanelWrapper
{
public:
    ClipIndicatorPanel(wxWindow* parent, int id);
    void SetClipped();
    void Reset();

protected:
    DECLARE_EVENT_TABLE();

private:
    void OnPaint(wxPaintEvent& evt);

    const Observer::Subscription mPlaybackStartStopSubscription;
    bool mClipping = false;
};
