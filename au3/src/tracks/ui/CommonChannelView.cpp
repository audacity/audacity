/**********************************************************************

Audacity: A Digital Audio Editor

CommonChannelView.cpp

Paul Licameli split from class TrackView (now called ChannelView)

**********************************************************************/

#include "CommonChannelView.h"

#include "BackgroundCell.h"
#include "CommonTrackInfo.h"
#include "TimeShiftHandle.h"
#include "TrackControls.h"
#include "ZoomHandle.h"
#include "../ui/SelectHandle.h"
#include "AColor.h"
#include "PendingTracks.h"
#include "../../ProjectSettings.h"
#include "Track.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"

#include <wx/dc.h>
#include <wx/graphics.h>

std::vector<UIHandlePtr> CommonChannelView::HitTest
    (const TrackPanelMouseState& st,
    const AudacityProject* pProject)
{
    UIHandlePtr result;
    using namespace ToolCodes;
    std::vector<UIHandlePtr> results;
    const auto& settings = ProjectSettings::Get(*pProject);
    const auto currentTool = settings.GetTool();
    const bool isMultiTool = (currentTool == multiTool);

    // In other tools, let subclasses determine detailed hits.
    results
        =DetailedHitTest(st, pProject, currentTool, isMultiTool);

    // There are still some general cases.

   #if 0
    // Sliding applies in more than one track type.
    if (!isMultiTool && currentTool == slideTool) {
        result = TimeShiftHandle::HitAnywhere(
            mTimeShiftHandle, FindTrack(), false);
        if (result) {
            results.push_back(result);
        }
    }
   #endif

    // Let the multi-tool right-click handler apply only in default of all
    // other detailed hits.
    if (isMultiTool) {
        result = ZoomHandle::HitTest(
            BackgroundCell::Get(*pProject).mZoomHandle, st.state);
        if (result) {
            results.push_back(result);
        }
    }

    // Finally, default of all is adjustment of the selection box.
    if (isMultiTool || currentTool == selectTool) {
        result = SelectHandle::HitTest(
            mSelectHandle, st, pProject, shared_from_this());
        if (result) {
            results.push_back(result);
        }
    }

    return results;
}

std::shared_ptr<TrackPanelCell> CommonChannelView::ContextMenuDelegate()
{
    const auto pTrack = FindTrack();
    if (pTrack) {
        return TrackControls::Get(*pTrack).shared_from_this();
    }
    return nullptr;
}

int CommonChannelView::GetMinimizedHeight() const
{
    const auto height = CommonTrackInfo::MinimumTrackHeight();
    auto pChannel = FindChannel().get();
    if (!pChannel) {
        return height;
    }
    const auto pTrack
        =dynamic_cast<const Track*>(&pChannel->GetChannelGroup());
    if (!pTrack) {
        return 0;
    }
    if (const auto pList = pTrack->GetOwner()) {
        if (const auto p = pList->GetOwner()) {
            pChannel
                =&PendingTracks::Get(*p).SubstituteOriginalChannel(*pChannel);
        }
    }

    // Find index of the channel in its group and use that to round off correctly
    const auto index = pChannel->GetChannelIndex();
    const auto nChannels = pChannel->GetChannelGroup().Channels().size();
    return (height * (index + 1) / nChannels) - (height * index / nChannels);
}

#include "Envelope.h"
#include "ZoomInfo.h"
void CommonChannelView::GetEnvelopeValues(const Envelope& env,
                                          double alignedTime, double sampleDur,
                                          double* buffer, int bufferLen, int leftOffset,
                                          const ZoomInfo& zoomInfo)
{
    // Getting many envelope values, corresponding to pixel columns, which may
    // not be uniformly spaced in time when there is a fisheye.

    double prevDiscreteTime=0.0, prevSampleVal=0.0, nextSampleVal=0.0;
    for ( int xx = 0; xx < bufferLen; ++xx ) {
        auto time = zoomInfo.PositionToTime(xx, -leftOffset);
        if (sampleDur <= 0) {
            // Sample interval not defined (as for time track)
            buffer[xx] = env.GetValue(time);
        } else {
            // The level of zoom-in may resolve individual samples.
            // If so, then instead of evaluating the envelope directly,
            // we draw a piecewise curve with knees at each sample time.
            // This actually makes clearer what happens as you drag envelope
            // points and make discontinuities.
            auto leftDiscreteTime = alignedTime
                                    + sampleDur * floor((time - alignedTime) / sampleDur);
            if (xx == 0 || leftDiscreteTime != prevDiscreteTime) {
                prevDiscreteTime = leftDiscreteTime;
                prevSampleVal
                    =env.GetValue(prevDiscreteTime, sampleDur);
                nextSampleVal
                    =env.GetValue(prevDiscreteTime + sampleDur, sampleDur);
            }
            auto ratio = (time - leftDiscreteTime) / sampleDur;
            if (env.GetExponential()) {
                buffer[ xx ] = exp(
                    (1.0 - ratio) * log(prevSampleVal)
                    + ratio * log(nextSampleVal));
            } else {
                buffer[ xx ]
                    =(1.0 - ratio) * prevSampleVal + ratio * nextSampleVal;
            }
        }
    }
}
