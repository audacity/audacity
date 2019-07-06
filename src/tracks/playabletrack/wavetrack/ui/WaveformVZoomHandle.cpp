/**********************************************************************

Audacity: A Digital Audio Editor

WaveformVZoomHandle.cpp

Paul Licameli split from WaveTrackVZoomHandle.cpp

**********************************************************************/

#include "WaveformVZoomHandle.h"

WaveformVZoomHandle::WaveformVZoomHandle(
   const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y)
      : mpTrack{ pTrack } , mZoomStart(y), mZoomEnd(y), mRect(rect)
{
}

WaveformVZoomHandle::~WaveformVZoomHandle() = default;
