/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackPanelResizeHandle.h"

#include <wx/cursor.h>
#include <wx/event.h>
#include <wx/translation.h>

#include "HitTestResult.h"
#include "ProjectHistory.h"
#include "RefreshCode.h"
#include "Track.h"
#include "TrackPanelMouseEvent.h"
#include "tracks/ui/ChannelView.h"

HitTestPreview TrackPanelResizeHandle::HitPreview(bool bLinked)
{
    // TODO: more-than-two-channels-message

    static wxCursor resizeCursor{ wxCURSOR_SIZENS };

    /// When in the resize area we can adjust size or relative size.
    // Check to see whether it is the first channel of a stereo track
    if (bLinked) {
        // If we are in the label we got here 'by mistake' and we're
        // not actually in the resize area at all.  (The resize area
        // is shorter when it is between stereo tracks).

        return {
            XO(
                "Click and drag to adjust relative size of stereo tracks, double-click to make heights equal"),
            &resizeCursor
        };
    } else {
        return {
            XO("Click and drag to resize the track."),
            &resizeCursor
        };
    }
}

TrackPanelResizeHandle::~TrackPanelResizeHandle()
{
}

std::shared_ptr<const Track> TrackPanelResizeHandle::FindTrack() const
{
    return TrackFromChannel(mwChannel.lock());
}

std::shared_ptr<Channel> TrackPanelResizeHandle::FindChannel()
{
    return mwChannel.lock();
}

UIHandle::Result TrackPanelResizeHandle::Click(
    const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    using namespace RefreshCode;
    if (evt.event.LeftDClick() && mMode == IsResizingBetweenLinkedTracks) {
        auto& tracks = TrackList::Get(*pProject);
        auto theChannel = FindChannel();
        if (!theChannel) {
            return RefreshNone;
        }
        auto& view = ChannelView::Get(*theChannel);
        if (!view.GetMinimized()) {
            auto range = GetTrack(*theChannel).Channels();
            auto size = range.size();
            auto height = range.sum([](auto pChannel){
                return ChannelView::Get(*pChannel).GetHeight();
            });
            int ii = 1;
            int coord = 0;
            for (const auto pChannel : range) {
                int newCoord = ((double)ii++ / size) * height;
                ChannelView::Get(*pChannel).SetExpandedHeight(newCoord - coord);
                coord = newCoord;
            }
            ProjectHistory::Get(*pProject).ModifyState(false);
            // Do not start a drag
            return Cancelled | RefreshAll;
        }
    }
    return RefreshNone;
}

TrackPanelResizeHandle::TrackPanelResizeHandle(
    const std::shared_ptr<Channel>& pChannel, int y)
    : mwChannel{pChannel}
    , mMouseClickY(y)
{
    // TODO: more-than-two-channels

    //STM:  Determine whether we should rescale one or two tracks
    auto channels = GetTrack(*pChannel).Channels();
    auto last = *channels.rbegin();
    auto& lastView = ChannelView::Get(*last);
    mInitialTrackHeight = lastView.GetHeight();
    mInitialExpandedHeight = lastView.GetExpandedHeight();
    mInitialMinimized = lastView.GetMinimized();

    if (channels.size() > 1) {
        auto first = *channels.begin();
        auto& firstView = ChannelView::Get(*first);

        mInitialUpperTrackHeight = firstView.GetHeight();
        mInitialUpperExpandedHeight = firstView.GetExpandedHeight();

        if (pChannel == *channels.rbegin()) {
            // pChannel is lowest among two or more,
            // so there is a previous channel
            mMode = IsResizingBelowLinkedTracks;
        } else {
            // pChannel is not the lowest among two or more,
            // so there is a next channel
            mMode = IsResizingBetweenLinkedTracks;
        }
    } else {
        // Don't assume there is a next or previous channel
        mMode = IsResizing;
    }
}

Channel* TrackPanelResizeHandle::PrevChannel(Channel& channel)
{
    // Assume channel is last of two
    // TODO: more-than-two-channels
    auto channels = GetTrack(channel).Channels();
    return &**channels.begin();
}

Channel* TrackPanelResizeHandle::NextChannel(Channel& channel)
{
    // Assume channel is first of two
    // TODO: more-than-two-channels
    auto channels = GetTrack(channel).Channels();
    return &**channels.rbegin();
}

UIHandle::Result TrackPanelResizeHandle::Drag
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    auto& tracks = TrackList::Get(*pProject);
    auto theChannel = FindChannel();
    if (!theChannel) {
        return RefreshCode::Cancelled;
    }

    auto& view = ChannelView::Get(*theChannel);

    if (view.GetMinimized() && mMode == IsResizingBetweenLinkedTracks) {
        return RefreshCode::Cancelled;
    }

    const wxMouseEvent& event = evt.event;

    int delta = (event.m_y - mMouseClickY);

    // On first drag, jump out of minimized mode.  Initial height
    // will be height of minimized track.
    //
    // This used to be in HandleResizeClick(), but simply clicking
    // on a resize border would switch the minimized state.
    if (view.GetMinimized()) {
        auto channels = GetTrack(*theChannel).Channels();
        for (auto pChannel : channels) {
            auto& channelView = ChannelView::Get(*pChannel);
            channelView.SetExpandedHeight(channelView.GetHeight());
            channelView.SetMinimized(false);
        }

        if (channels.size() > 1) {
            // Initial values must be reset since they weren't based on the
            // minimized heights.
            auto& channelView = ChannelView::Get(**channels.begin());
            mInitialUpperTrackHeight = channelView.GetHeight();
            mInitialTrackHeight = channelView.GetHeight();
        }
    }

    // Common pieces of code for MONO_WAVE_PAN and otherwise.
    auto doResizeBelow = [&] (Channel* prev) {
        // TODO: more-than-two-channels

        auto& prevView = ChannelView::Get(*prev);

        double proportion = static_cast < double >(mInitialTrackHeight)
                            / (mInitialTrackHeight + mInitialUpperTrackHeight);

        int newTrackHeight = static_cast < int >
                             (mInitialTrackHeight + delta * proportion);

        int newUpperTrackHeight = static_cast < int >
                                  (mInitialUpperTrackHeight + delta * (1.0 - proportion));

        //make sure neither track is smaller than its minimum height
        if (newTrackHeight < view.GetMinimizedHeight()) {
            newTrackHeight = view.GetMinimizedHeight();
        }
        if (newUpperTrackHeight < prevView.GetMinimizedHeight()) {
            newUpperTrackHeight = prevView.GetMinimizedHeight();
        }

        view.SetExpandedHeight(newTrackHeight);
        prevView.SetExpandedHeight(newUpperTrackHeight);
    };

    auto doResizeBetween = [&] (Channel* next) {
        // TODO: more-than-two-channels

        auto& nextView = ChannelView::Get(*next);
        int newUpperTrackHeight = mInitialUpperTrackHeight + delta;
        int newTrackHeight = mInitialTrackHeight - delta;

        // make sure neither track is smaller than its minimum height
        if (newTrackHeight < nextView.GetMinimizedHeight()) {
            newTrackHeight = nextView.GetMinimizedHeight();
            newUpperTrackHeight
                =mInitialUpperTrackHeight + mInitialTrackHeight - nextView.GetMinimizedHeight();
        }
        if (newUpperTrackHeight < view.GetMinimizedHeight()) {
            newUpperTrackHeight = view.GetMinimizedHeight();
            newTrackHeight
                =mInitialUpperTrackHeight + mInitialTrackHeight - view.GetMinimizedHeight();
        }

        view.SetExpandedHeight(newUpperTrackHeight);
        nextView.SetExpandedHeight(newTrackHeight);
    };

    auto doResize = [&] {
        int newTrackHeight = mInitialTrackHeight + delta;
        if (newTrackHeight < view.GetMinimizedHeight()) {
            newTrackHeight = view.GetMinimizedHeight();
        }
        view.SetExpandedHeight(newTrackHeight);
    };

    //STM: We may be dragging one or two (stereo) tracks.
    // If two, resize proportionally if we are dragging the lower track, and
    // adjust compensatively if we are dragging the upper track.

    switch (mMode) {
    case IsResizingBelowLinkedTracks:
    {
        // Assume previous channel is present, see constructor
        doResizeBelow(PrevChannel(*theChannel));
        break;
    }
    case IsResizingBetweenLinkedTracks:
    {
        // Assume next channel is present, see constructor
        doResizeBetween(NextChannel(*theChannel));
        break;
    }
    case IsResizing:
    {
        doResize();
        break;
    }
    default:
        // don't refresh in this case.
        return RefreshCode::RefreshNone;
    }

    return RefreshCode::RefreshAll;
}

HitTestPreview TrackPanelResizeHandle::Preview
    (const TrackPanelMouseState&, AudacityProject*)
{
    return HitPreview(mMode == IsResizingBetweenLinkedTracks);
}

UIHandle::Result TrackPanelResizeHandle::Release
    (const TrackPanelMouseEvent&, AudacityProject* pProject,
    wxWindow*)
{
    ///  This happens when the button is released from a drag.
    ///  Since we actually took care of resizing the track when
    ///  we got drag events, all we have to do here is clean up.
    ///  We also modify the undo state (the action doesn't become
    ///  undo-able, but it gets merged with the previous undo-able
    ///  event).
    ProjectHistory::Get(*pProject).ModifyState(false);
    return RefreshCode::FixScrollbars;
}

UIHandle::Result TrackPanelResizeHandle::Cancel(AudacityProject* pProject)
{
    auto& tracks = TrackList::Get(*pProject);
    auto theChannel = FindChannel();
    if (!theChannel) {
        return RefreshCode::Cancelled;
    }

    switch (mMode) {
    case IsResizing:
    {
        auto& view = ChannelView::Get(*theChannel);
        view.SetExpandedHeight(mInitialExpandedHeight);
        view.SetMinimized(mInitialMinimized);
    }
    break;
    case IsResizingBetweenLinkedTracks:
    {
        // Assume next channel is present, see constructor
        const auto next = NextChannel(*theChannel);
        auto& view = ChannelView::Get(*theChannel),
        & nextView = ChannelView::Get(*next);
        view.SetExpandedHeight(mInitialUpperExpandedHeight);
        view.SetMinimized(mInitialMinimized);
        nextView.SetExpandedHeight(mInitialExpandedHeight);
        nextView.SetMinimized(mInitialMinimized);
    }
    break;
    case IsResizingBelowLinkedTracks:
    {
        // Assume previous channel is present, see constructor
        const auto prev = PrevChannel(*theChannel);
        auto& view = ChannelView::Get(*theChannel),
        & prevView = ChannelView::Get(*prev);
        view.SetExpandedHeight(mInitialExpandedHeight);
        view.SetMinimized(mInitialMinimized);
        prevView.SetExpandedHeight(mInitialUpperExpandedHeight);
        prevView.SetMinimized(mInitialMinimized);
    }
    break;
    }

    return RefreshCode::RefreshAll;
}

Track& TrackPanelResizeHandle::GetTrack(Channel& channel)
{
    return *static_cast<Track*>(&channel.GetChannelGroup());
}
