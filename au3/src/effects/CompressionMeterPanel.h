/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorPanelCommon.h"
#include "DynamicRangeProcessorTypes.h"
#include "LockFreeQueue.h"
#include "MeterValueProvider.h"
#include "Observer.h"
#include "wxPanelWrapper.h"
#include <functional>
#include <wx/timer.h>

class CompressorInstance;
class wxPaintDC;

class CompressionMeterPanel final : public wxPanelWrapper
{
public:
    CompressionMeterPanel(
        wxWindow* parent, int id, CompressorInstance& instance, float dbRange, std::function<void()> onClipped);

    void SetDbRange(float dbRange);
    void Reset();
    void ResetClipped();

protected:
    DECLARE_EVENT_TABLE();

private:
    void PaintMeter(
        wxPaintDC& dc, const wxColor& color, const wxRect& rect, const MeterValueProvider& provider);

    void OnPaint(wxPaintEvent& evt);
    void OnTimer(wxTimerEvent& evt);

    bool AcceptsFocus() const override;
    // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const override;

    const std::shared_ptr<DynamicRangeProcessorMeterValuesQueue>
    mMeterValuesQueue;
    const Observer::Subscription mPlaybackStartStopSubscription;
    const Observer::Subscription mPlaybackPausedSubscription;
    const std::function<void()> mOnClipped;
    std::unique_ptr<MeterValueProvider> mCompressionMeter;
    std::unique_ptr<MeterValueProvider> mOutputMeter;
    float mDbBottomEdgeValue;

    wxTimer mTimer;
    bool mStopWhenZero = false;
    bool mClipped = false;
};
