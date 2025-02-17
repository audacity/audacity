/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_RESIZE_HANDLE__
#define __AUDACITY_TRACK_PANEL_RESIZE_HANDLE__

#include "UIHandle.h"

class Channel;
class Track;

//! Constructed from one channel, but changes height of all channels in a track
class TrackPanelResizeHandle final : public UIHandle
{
    TrackPanelResizeHandle(const TrackPanelResizeHandle&) = delete;

public:
    /*!
     @pre `pChannel != nullptr`
     */
    TrackPanelResizeHandle(const std::shared_ptr<Channel>& pChannel, int y);

    TrackPanelResizeHandle& operator=(const TrackPanelResizeHandle&) = default;

    static HitTestPreview HitPreview(bool bLinked);

    virtual ~TrackPanelResizeHandle();

    std::shared_ptr<const Track> FindTrack() const override;
    std::shared_ptr<Channel> FindChannel();

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

private:
    static Track& GetTrack(Channel& channel);
    Channel* PrevChannel(Channel& channel);
    Channel* NextChannel(Channel& channel);

    enum Mode {
        IsResizing,
        IsResizingBetweenLinkedTracks,
        IsResizingBelowLinkedTracks,
    };
    Mode mMode{ IsResizing };

    std::weak_ptr<Channel> mwChannel;

    bool mInitialMinimized{};
    int mInitialTrackHeight{};
    int mInitialExpandedHeight{};
    int mInitialUpperTrackHeight{};
    int mInitialUpperExpandedHeight{};

    int mMouseClickY{};
};

#endif
