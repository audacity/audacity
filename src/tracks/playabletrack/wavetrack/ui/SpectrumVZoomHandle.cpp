/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumVZoomHandle.cpp

Paul Licameli split from WaveTrackVZoomHandle.cpp

**********************************************************************/

#include "SpectrumVZoomHandle.h"

SpectrumVZoomHandle::SpectrumVZoomHandle
(const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y)
   : mpTrack{ pTrack } , mZoomStart(y), mZoomEnd(y), mRect(rect)
{
}

SpectrumVZoomHandle::~SpectrumVZoomHandle() = default;
