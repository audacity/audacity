#pragma once

#include <QRect>

#include "TimelineViewItem.h"
#include "WaveTrack.h"

class ZoomInfo;
class ClipTimes;

class WaveClipItem final : public TimelineViewItem
{
public:
   static bool ClipDetailsVisible(const ClipTimes& clip, const ZoomInfo& zoomInfo, const QRect& viewRect);

   WaveClipItem(WaveTrack& waveTrack);
   ~WaveClipItem() override;

   WaveClipItem(const WaveClipItem&) = delete;
   WaveClipItem& operator=(const WaveClipItem&) = delete;
   WaveClipItem(WaveClipItem&&) = delete;
   WaveClipItem operator=(WaveClipItem&&) = delete;

   void SetInterval(std::shared_ptr<WaveTrack::Interval> interval);

   void Paint(QQmlEngine& engine,
              QPainter& painter,
              const QRect& viewRect,
              const TimelineContext& trackPanel) override;

   std::unique_ptr<TimelineViewUIHandle> HitTest(const QPoint& at) const override;

private:
   bool mHovered{false};
   WaveTrack& mWaveTrack;
   std::shared_ptr<WaveTrack::Interval> mInterval;
   QRect mClipRect;
   QRect mHeaderRect;
   QRect mMenuButtonRect;
};
