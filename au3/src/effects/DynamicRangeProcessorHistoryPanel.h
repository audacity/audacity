/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorHistoryPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorClock.h"
#include "DynamicRangeProcessorHistory.h"
#include "Observer.h"
#include "wxPanelWrapper.h"
#include <chrono>
#include <functional>
#include <optional>
#include <wx/geometry.h>
#include <wx/timer.h>

class CompressorInstance;
class wxPaintEvent;
class wxEraseEvent;

class DynamicRangeProcessorHistoryPanel final : public wxPanelWrapper
{
public:
    static constexpr auto minWidth = 600;

    DynamicRangeProcessorHistoryPanel(
        wxWindow* parent, wxWindowID winid, CompressorInstance& instance, std::function<void(float)> onDbRangeChanged);

    struct ClockSynchronization
    {
        const float firstPacketTime;
        const std::chrono::steady_clock::time_point start;
        std::chrono::steady_clock::time_point now;
    };

    // For now no-opt functions, but there are plans to add visibility toggles.
    void ShowInput(bool show);
    void ShowOutput(bool show);
    void ShowActual(bool show);
    void ShowTarget(bool show);

    DECLARE_EVENT_TABLE();

private:
    void OnPaint(wxPaintEvent& evt);
    void OnSize(wxSizeEvent& evt);
    void OnTimer(wxTimerEvent& evt);
    void InitializeForPlayback(CompressorInstance& instance, double sampleRate);

    bool AcceptsFocus() const override;
    // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const override;

    CompressorInstance& mCompressorInstance;
    std::shared_ptr<DynamicRangeProcessorOutputPacketQueue> mOutputQueue;
    std::vector<DynamicRangeProcessorOutputPacket> mPacketBuffer;
    std::optional<DynamicRangeProcessorHistory> mHistory;
    DynamicRangeProcessorClock mClock;
    const std::function<void(float)> mOnDbRangeChanged;
    const Observer::Subscription mInitializeProcessingSettingsSubscription;
    const Observer::Subscription mRealtimeResumeSubscription;
    const Observer::Subscription mPlaybackEventSubscription;
    wxTimer mTimer;
    std::optional<ClockSynchronization> mSync;
    std::vector<double> mX;
    std::vector<wxPoint2DDouble> mTarget; // Compression
    std::vector<wxPoint2DDouble> mActual; // Compression
    std::vector<wxPoint2DDouble> mInput;
    std::vector<wxPoint2DDouble> mOutput;
    bool mPlaybackAboutToStart = false;
    bool mShowInput = true;
    bool mShowOutput = true;
    bool mShowActual = true;
    bool mShowTarget = true;
};
