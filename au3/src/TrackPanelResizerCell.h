/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelResizerCell.h

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_RESIZER_CELL__
#define __AUDACITY_TRACK_PANEL_RESIZER_CELL__

#include "ClientData.h" // to inherit
#include "tracks/ui/CommonTrackPanelCell.h" // to inherit

class Channel;
class TrackPanelResizeHandle;

class TrackPanelResizerCell : public CommonChannelCell, public std::enable_shared_from_this< TrackPanelResizerCell >
{
    TrackPanelResizerCell(const TrackPanelResizerCell&) = delete;
    TrackPanelResizerCell& operator=(const TrackPanelResizerCell&) = delete;
public:

    /*!
     @pre `dynamic_cast<Track*>(&channel.GetChannelGroup()) != nullptr`
     */
    static TrackPanelResizerCell& Get(Channel& channel);
    static const TrackPanelResizerCell& Get(const Channel& channel);

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState&, const AudacityProject*) override;

    explicit TrackPanelResizerCell(const std::shared_ptr<Channel>& channel);

private:
    /*!
     @pre `iChannel < group.NChannels()`
     */
    static TrackPanelResizerCell& GetFromChannelGroup(
        ChannelGroup& group, size_t iChannel);

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
};

#endif
