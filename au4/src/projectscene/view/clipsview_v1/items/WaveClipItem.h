#pragma once

#include <QRect>

#include "WaveTrack.h"

class ZoomInfo;
class ClipTimes;
class TimelineContext;

class WaveClipItem
{
public:
    static bool ClipDetailsVisible(const ClipTimes& clip, const ZoomInfo& zoomInfo, const QRect& viewRect);

    WaveClipItem(WaveTrack& waveTrack);
    ~WaveClipItem();

    WaveClipItem(const WaveClipItem&) = delete;
    WaveClipItem& operator=(const WaveClipItem&) = delete;
    WaveClipItem(WaveClipItem&&) = delete;
    WaveClipItem operator=(WaveClipItem&&) = delete;

    void SetClip(WaveClip* clip);

    void Paint(QPainter& painter, const QRect& viewRect, const TimelineContext& trackPanel);

private:
    bool mHovered{ false };
    WaveTrack& mWaveTrack;
    WaveClip* mClip = nullptr;
    QRect mMenuButtonRect;
};
