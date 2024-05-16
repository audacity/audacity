// TODO header
#pragma once

#include "DynamicRangeProcessorHistory.h"
#include "Observer.h"
#include "wxPanelWrapper.h"
#include <chrono>
#include <optional>
#include <wx/timer.h>

class CompressorInstance;
class DynamicRangeProcessorOutputs;
class wxPaintEvent;
class wxEraseEvent;

class DynamicRangeProcessorHistoryPanel final : public wxPanelWrapper
{
public:
   static constexpr auto minHeight = 100;
   static constexpr auto minRangeDb = 10.f;

   DynamicRangeProcessorHistoryPanel(
      wxWindow* parent, wxWindowID winid, DynamicRangeProcessorOutputs& outputs,
      CompressorInstance& instance,
      std::function<void(float)> onDbRangeChanged);

   struct ClockSynchronization
   {
      const float firstPacketTime;
      const std::chrono::steady_clock::time_point start;
      std::chrono::steady_clock::time_point now;
   };

   DECLARE_EVENT_TABLE();

private:
   void OnPaint(wxPaintEvent& evt);
   void OnSize(wxSizeEvent& evt);
   void OnTimer(wxTimerEvent& evt);
   void InitializeForPlayback(double sampleRate);

   bool AcceptsFocus() const override;
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override;

   std::optional<DynamicRangeProcessorHistory> mHistory;
   const std::function<void(float)> mOnDbRangeChanged;
   const Observer::Subscription mInitializeProcessingSettingsSubscription;
   const Observer::Subscription mRealtimeResumeSubscription;
   wxTimer mTimer;
   std::optional<ClockSynchronization> mSync;
   bool mPlaybackAboutToStart = false;
};
